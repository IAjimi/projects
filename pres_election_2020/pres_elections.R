## SETUP ####
getwd()
setwd("C:/Users/ia767/Documents/pres_elections")

### LOADING LIBRARIES
library(readr)
library(readxl)
library(tidyverse)
library(leaps)
library(broom)

### LOADING DATA
senate <- read_csv("1976-2018-senate.csv")
house <- read_csv("1976-2018-house2.csv")
president <- read_csv("1976-2016-president.csv")
election_years <- distinct(president, year)$year

## ELECTORAL DATA ####
## Source: https://electionlab.mit.edu/data
## FIX TYPO IN PRESIDENT FILE
president$totalvotes[president$year == 2000 & president$state_fips == 31] <- 697019

## CLEANING & TRANSFORMING
senate <- senate %>% 
  filter(party == 'democrat') %>%
  mutate(senate_percent_vote = 100 * candidatevotes / totalvotes,
         year = year + 2) %>%
  select(year, state, state_po, state_fips, senate_candidatevotes = candidatevotes, senate_percent_vote) %>%
  group_by(year, state, state_po, state_fips) %>%
  summarize(mean_senate_percent_vote = mean(senate_percent_vote, na.rm = T)) 

house <- house %>% 
  mutate(party = case_when(
    state == 'Vermont' & candidate %in% c('Bernard Sanders', 'Bernie Sanders') ~ 'democrat',
    T ~ party
  )) %>%
  filter(party == 'democrat') %>%
  mutate(year = year + 2) %>%
  select(year, state, state_po, state_fips, candidatevotes, totalvotes) %>%
  group_by(year, state, state_po, state_fips) %>%
  summarize(house_percent_vote = 100 * sum(candidatevotes) / sum(totalvotes))

pres_participation <- president %>% distinct(year, state_fips, totalvotes)

president <- president %>% 
  filter(party == 'democrat') %>%
  group_by(year, state, state_po, state_fips, totalvotes) %>%
  summarise(candidatevotes = sum(candidatevotes)) %>% # has to be done like this bc of writein = False or True for AZ and MD
  ungroup() %>%
  mutate(pres_percent_vote = 100 * candidatevotes / totalvotes) %>%
  select(year, state, state_po, state_fips, pres_percent_vote)

## Add 2020
president_2020 <- president %>% 
  filter(year == 2016) %>%
  mutate(year = 2020,
         pres_percent_vote = NA)

president <- rbind(president, president_2020)

president <- president %>%
  mutate(
    incumbent_party = case_when( # 1 is 'R'
      year == 1976 ~ 1,
      year == 1980 ~ 0,
      year >= 1984 & year <= 1992 ~ 1,
      year >= 1996 & year <= 2000 ~ 0,
      year >= 2004 & year <= 2008 ~ 1,
      year >= 2012 & year <= 2016 ~ 0,
      year == 2020 ~ 1
    ),
    potus_run_reelect = case_when( # POTUS running for reelection?
      year == 1976 ~ 1, # check
      year == 1980 ~ 1,
      year == 1984 ~ 1,
      year == 1988 ~ 0,
      year == 1992 ~ 1,
      year == 1996 ~ 1,
      year == 2000 ~ 0,
      year == 2004 ~ 1,
      year == 2008 ~ 0,
      year == 2012 ~ 1,
      year == 2016 ~ 0,
      year == 2020 ~ 1
    )
  )

president <- president %>%
  left_join(house, by = c("year", "state", "state_po", "state_fips")) %>%
  left_join(senate, by = c("year", "state", "state_po", "state_fips")) %>%
  left_join(pres_participation, by = c("year", "state_fips"))

# Fill missing values (Senate)
president <- president %>%
  group_by(state) %>%
  fill(mean_senate_percent_vote) %>% 
  ungroup()

## ECONOMIC DATA ####
## Personal Income by State
## Source: https://www.bea.gov/ (SQINC1)
bea_personal_income_state <- read_excel("bea_personal_income_state.xls", 
                                        skip = 5)
# Cleaning Df
bea_personal_income_state <- bea_personal_income_state %>% 
  gather(year, val, `1976`:`2019`) %>%
  mutate(Description = if_else(Description == 'Population (persons) 1/', 'population', 'per_capita_personal_income'),
         GeoName = str_replace_all(GeoName, ' \\*', ''),
         year = as.numeric(year), # gather changed it to character
         year = if_else(year == 2019, 2020, year)) %>% 
  select(-LineCode) %>% # otherwise spread doesn't fill properly
  spread(Description, val) %>%
  select(state = GeoName, year, per_capita_personal_income, population) %>%
  filter(!is.na(per_capita_personal_income))

## Add to Pres
president <- president %>%
  left_join(bea_personal_income_state, by = c("year", "state"))

# Add YoY Change + Z score var
president <- president %>%
  split(.$state) %>%
  map(mutate, 
      yoy_per_cap_income_change = per_capita_personal_income / lag(per_capita_personal_income), 
      yoy_pop_change = population / lag(population),
      mean_pcpi = c(NA, zoo::rollmean(per_capita_personal_income, 2)), #rollmean doesn't include NA when can't compute mean
      mean_pcpi = lag(mean_pcpi), # lag to get avg of prev 2 years
      zscore_pcpi = (per_capita_personal_income - mean_pcpi) / sd(per_capita_personal_income)
      ) %>%
  map(select, -mean_pcpi) %>%
  do.call(rbind.data.frame, .)

## GDP Nationwide, Real GDP, Chained Dollars (2012), Seasonally Adjusted (source BEA)
bea_gdp_nationwide <- read_excel("bea_gdp_nationwide.xls", skip = 5)
bea_gdp_nationwide <- bea_gdp_nationwide[c(2:nrow(bea_gdp_nationwide)), ]

# Fix Column Names
gdp_col_names <- c("Line", 'Description')

for (i in c(1:length(c(1976:2019)))){
  yr <- c(1976:2019)[i]
  
  for (Q in c('Q1', 'Q2', 'Q3', 'Q4')){
      new_name <- paste(yr, Q, sep = '_')
      gdp_col_names <- c(gdp_col_names, new_name)
  }
}

gdp_col_names <- c(gdp_col_names, c('2020_Q1', '2020_Q3')) #it's actually 2020_Q2, renamed so that it will work w/ code below

# Clean Data
names(bea_gdp_nationwide) <- gdp_col_names

bea_gdp_nationwide <- bea_gdp_nationwide %>%
  select_if(str_detect(names(.), 'Q3|Description') == TRUE) %>% # only keeping Q3: around election time
  mutate_if(str_detect(names(.), 'Q3') == TRUE, as.numeric) %>%
  gather(year, metric, -Description) %>%
  mutate(year = str_replace(year, '_Q3', ''),
         Description = case_when(
           Description == 'Gross domestic product' ~ 'gdp',
           Description == 'Personal consumption expenditures' ~ 'personal_consumption_exp',
           Description == 'Exports' ~ 'exports',
           T ~ Description
         )) %>%
  filter(Description %in% c('gdp', 'personal_consumption_exp', 'exports')) %>% # note: bc of the way the xls file is formatted, other fields may need renaming
  spread(Description, metric) %>% 
  mutate_if(is.numeric, 
            funs(
              YOY = . / lag(.),
              zscore = (. - lag(c(NA, zoo::rollmean(., 2))) ) / sd(.)
              )
            ) %>%
  mutate(year = as.numeric(year))

president <- president %>%
  left_join(bea_gdp_nationwide, by = c("year"))

## SOCIO-ECONOMIC & DEMOGRAPHIC DATA ####
## Educational Attainment 
# https://www.census.gov/library/publications/2010/demo/educational-attainment-1940-2000.html (i think)
## HS (1970:2000)
high_school_degree <- read_csv("table05a.csv", skip = 9)
high_school_degree <- high_school_degree[c(13:63), ]
high_school_degree <- high_school_degree %>%
  select(state = `Geographic area`, `1970`, `1980`, `1990`, `2000`) %>%
  mutate(state = str_replace(state, '..', '')) %>%
  gather(year, per_hs_degree, -state)

# Changing dates so they'll map to election years in president df
new_high_school_degree <- data.frame()

for (yr in election_years[election_years <= 2010 & !election_years %in% c(1980, 2000)]) { ## NEED FRESHER DATA
  
  decade_yr <- substring(yr, 1, 3) # get decade
  
  relevant_data <- high_school_degree %>% 
    filter(substring(year, 1, 3) == decade_yr) %>%
    mutate(year = as.numeric(yr))
  
  new_high_school_degree <- rbind(new_high_school_degree, relevant_data)
}

high_school_degree <- rbind(high_school_degree, new_high_school_degree)

## BA (1970:2000)
bachelor_degree <- read_csv("table06a.csv", skip = 8)
bachelor_degree <- bachelor_degree[c(9:59), ]
bachelor_degree <- bachelor_degree %>%
  select(state = `Geographic area`, `1970`, `1980`, `1990`, `2000`) %>%
  mutate(state = str_replace(state, '..', '')) %>%
  gather(year, per_bachelor_degree, -state)

# Changing dates so they'll map to election years in president df
new_bachelor_degree <- data.frame()

for (yr in election_years[election_years <= 2010 & !election_years %in% c(1980, 2000)]) { ## see if date needs changing
  
  decade_yr <- substring(yr, 1, 3) # get decade
  
  relevant_data <- bachelor_degree %>% 
    filter(substring(year, 1, 3) == decade_yr) %>%
    mutate(year = as.numeric(yr))
  
  new_bachelor_degree <- rbind(new_bachelor_degree, relevant_data)
}

bachelor_degree <- rbind(bachelor_degree, new_bachelor_degree)

### Other Edu Attainment Series
## 2012
ACSDT1Y2012 <- read_csv("C:/Users/ia767/Downloads/ACSDT1Y2012.B15003_2020-10-06T073920/ACSDT1Y2012.B15003_data_with_overlays_2020-10-06T073910.csv", 
                        skip = 1)
ACSDT1Y2012 <- ACSDT1Y2012[ , c(1:3, 35:52)] # remove all pre-HS completion data

ACSDT1Y2012 <- ACSDT1Y2012 %>%
  select_if(str_detect(names(.), 'Margin') == FALSE) %>% # remove margins
  mutate(state_fips = str_sub(id, start = -2), # get state fips
         state_fips = as.numeric(state_fips)) %>%
  select(-id)

# Get total HS or more
ACSDT1Y2012$tot_hs <- rowSums(select(ACSDT1Y2012, "Estimate!!Total!!Regular high school diploma":"Estimate!!Total!!Doctorate degree"))

# Complete DF
ACSDT1Y2012 <- ACSDT1Y2012 %>%
  mutate(per_hs_degree = 100 * tot_hs / `Estimate!!Total`,
         per_bachelor_degree = NA,
         year = 2012) %>% # actually 2019 but doing this for preds purposes
  select(state = `Geographic Area Name`, year, per_hs_degree, per_bachelor_degree)

## 2016
ACSDT1Y2016 <- read_csv("C:/Users/ia767/Downloads/ACSDT1Y2016.B15003_2020-10-06T081731/ACSDT1Y2016.B15003_data_with_overlays_2020-10-06T081718.csv", 
                        skip = 1)
ACSDT1Y2016 <- ACSDT1Y2016[ , c(1:3, 35:52)] # remove all pre-HS completion data

ACSDT1Y2016 <- ACSDT1Y2016 %>%
  select_if(str_detect(names(.), 'Margin') == FALSE) %>% # remove margins
  mutate(state_fips = str_sub(id, start = -2), # get state fips
         state_fips = as.numeric(state_fips)) %>%
  select(-id)

# Get total HS or more
ACSDT1Y2016$tot_hs <- rowSums(select(ACSDT1Y2016, "Estimate!!Total!!Regular high school diploma":"Estimate!!Total!!Doctorate degree"))

# Complete DF
ACSDT1Y2016 <- ACSDT1Y2016 %>%
  mutate(per_hs_degree = 100 * tot_hs / `Estimate!!Total`,
         per_bachelor_degree = NA,
         year = 2016) %>% # actually 2019 but doing this for preds purposes
  select(state = `Geographic Area Name`, year, per_hs_degree, per_bachelor_degree)

## 2019
ACSDT1Y2019 <- read_csv("C:/Users/ia767/Downloads/ACSDT1Y2019.B15003_2020-10-06T073806/ACSDT1Y2019.B15003_data_with_overlays_2020-10-06T073759.csv",
                        skip = 1)
ACSDT1Y2019 <- ACSDT1Y2019[ , c(1:3, 35:52)] # remove all pre-HS completion data

ACSDT1Y2019 <- ACSDT1Y2019 %>%
  select_if(str_detect(names(.), 'Margin') == FALSE) %>% # remove margins
  mutate(state_fips = str_sub(id, start = -2), # get state fips
         state_fips = as.numeric(state_fips)) %>%
  select(-id)

# Get total HS or more
ACSDT1Y2019$tot_hs <- rowSums(select(ACSDT1Y2019, "Estimate!!Total:!!Regular high school diploma":"Estimate!!Total:!!Doctorate degree"))

# Complete DF
ACSDT1Y2019 <- ACSDT1Y2019 %>%
  mutate(per_hs_degree = 100 * tot_hs / `Estimate!!Total:`,
         per_bachelor_degree = NA,
         year = 2020) %>% # actually 2019 but doing this for preds purposes
  select(state = `Geographic Area Name`, year, per_hs_degree, per_bachelor_degree)

### JOIN EDU TO PRESIDENT DF
## Combined
degree_df <- full_join(high_school_degree, bachelor_degree, by= c('state', 'year')) %>%
  mutate(year = as.numeric(year))

degree_df <- rbind(degree_df, ACSDT1Y2012)
degree_df <- rbind(degree_df, ACSDT1Y2016)
degree_df <- rbind(degree_df, ACSDT1Y2019)

president <- president %>%
  left_join(degree_df, by = c("state", "year"))


## Census Data (https://www2.census.gov/programs-surveys/popest/datasets/) ####
### 1970: Race of Population by County
### Codebook: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/1900-1980/co-asr-7079-layout.txt
demo_1970s <- read_csv("co-asr-7079.csv", col_names = FALSE)

demo_1970s <- demo_1970s %>% 
  mutate(
    under_19 = X4 + X5 + X6 + X7,
    adults = X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15,
    over_60 = X16 + X17 + X18 + X19 + X20 + X21,
    subtotal = under_19 + adults + over_60,
    X3 = case_when(
      X3 == 1 ~ 'White male',
      X3 == 2 ~ 'White female',
      X3 == 3 ~ 'Black male',
      X3 == 4 ~ 'Black female',
      X3 == 5 ~ 'Other races male',
      X3 == 6 ~ 'Other races female'
    ),
    state_fips = as.numeric(str_sub(X2, 1, 2))
  ) %>%
  select(year = X1, state_fips, race_sex_code = X3, under_19, adults, over_60, subtotal)

### 1980 (1980, 1984, 1988)
### Source: https://www.census.gov/data/tables/time-series/demo/popest/1980s-county.html
full_demo_1980s <- data.frame()

for (i in c(1980, 1984, 1988)){
  
  file_path <- paste("pe-02-", i, ".xls", sep = '')
  demo_1980s <- read_excel(file_path,  skip = 5)
  
  demo_1980s <- demo_1980s  %>% 
    mutate(
      under_19 = `Under 5 years` + `5 to 9 years` + `10 to 14 years` + `15 to 19 years`,
      adults = `20 to 24 years` + `25 to 29 years` + `30 to 34 years` + `35 to 39 years` + `40 to 44 years` 
      + `45 to 49 years` + `50 to 54 years` + `55 to 59 years`,
      over_60 = `60 to 64 years`  + `65 to 69 years` + `70 to 74 years` + `75 to 79 years` + `80 to 84 years` 
      + `85 years and over`,
      subtotal = under_19 + adults + over_60,
      state_fips = as.numeric(str_sub(`FIPS State and County Codes`, 1, 2))
    ) %>%
    select(year = `Year of Estimate`, state_fips, 
           race_sex_code = `Race/Sex Indicator`, under_19, adults, over_60, subtotal)
  
  full_demo_1980s <- rbind(full_demo_1980s, demo_1980s)
}

### 1990
### Source: https://www2.census.gov/programs-surveys/popest/datasets/1990-2000/counties/asrh/co-99-10.txt
full_demo_1990s <- data.frame()

for (i in c(1:56)){
  # Formatting i into State FIPS
  if (i < 10){
    state_fips <- paste('0', i, sep = '')
  } else{
    state_fips <- as.character(i)
  }
  tryCatch({
    # Downloading & Cleaning Data
    census_url <- paste("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/counties/asrh/casrh",
                        state_fips, ".txt", sep = '')
    print(census_url)
    demo_1990s <- read.delim(census_url, skip = 17, header = FALSE)
    demo_1990s <- map(demo_1990s, str_split, ' {1,}') 
    demo_1990s <- demo_1990s$V1 %>% map(as.numeric) %>% do.call(rbind.data.frame, .)
    names(demo_1990s) <- c("year", "FIPS", "race_sex_code", "X4", "X5", "X6", "X7",
                           "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15",
                           "X16", "X17", "X18", "X19", "X20", "X21")
    
    demo_1990s <- demo_1990s %>% 
      filter(race_sex_code <= 10) %>% # removing the hispanic of any race to avoid double-counting
      mutate(
        under_19 = X4 + X5 + X6 + X7,
        adults = X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15,
        over_60 = X16 + X17 + X18 + X19 + X20 + X21,
        subtotal = under_19 + adults + over_60,
        race_sex_code = case_when(
          race_sex_code == 1 ~ 'White Non-Hispanic Male',
          race_sex_code == 2 ~ 'White Non-Hispanic Female',
          race_sex_code == 3 ~ 'White Hispanic Male',
          race_sex_code == 4 ~ 'White Hispanic Female',
          race_sex_code == 5 ~ 'Black Male',
          race_sex_code == 6 ~ 'Black Female',
          race_sex_code == 7 ~ 'American Indian and Alaska Native Male',
          race_sex_code == 8 ~ 'American Indian and Alaska Native Female',
          race_sex_code == 9 ~ 'Asian and Pacific Islander Male',
          race_sex_code == 10 ~ 'Asian and Pacific Islander Female'
        ),
        state_fips = as.numeric(state_fips) # FIPS changed since prior files
      ) %>%
      select(year, state_fips, race_sex_code, under_19, adults, over_60, subtotal)
    
    full_demo_1990s <- rbind(full_demo_1990s, demo_1990s)    
  }, error=function(e){})
  
}

### 2000
### Intercensal Estimates of the Resident Population by Sex, Race, and Hispanic Origin for Counties
### Source: https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html
demo_00s <- read_csv("co-est00int-sexracehisp.csv")
demo_00s <- demo_00s %>%
  select(-SUMLEV, - COUNTY, -STNAME, -CTYNAME, -POPESTIMATE2010) %>%
  gather(year, est_pop, ESTIMATESBASE2000:CENSUS2010POP) %>%
  filter(ORIGIN == 0) %>% # ORIGIN diff. btw Hispanic and Non-Hispanic, this keeps total of both
  mutate(year = parse_number(year),
         state_fips = as.numeric(STATE),
  )

### 2010
### Source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
demo_2010s <- read_csv("cc-est2019-alldata.csv")

demo_2010s <- demo_2010s %>%
  select(STATE, YEAR, AGEGRP, TOT_FEMALE, BA_MALE, BA_FEMALE, WA_MALE, WA_FEMALE) %>%
  filter(AGEGRP == 0) %>% # removes total age slot
  mutate(
    STATE = as.numeric(STATE),
    YEAR = case_when(
      YEAR <= 3 ~ 2010,
      YEAR == 4 ~ 2011,
      YEAR == 5 ~ 2012,
      YEAR == 6 ~ 2013,
      YEAR == 7 ~ 2014,
      YEAR == 8 ~ 2015,
      YEAR == 9 ~ 2016,
      YEAR == 10 ~ 2017,
      YEAR == 11 ~ 2018,
      YEAR == 12 ~ 2020 # actually is 2019 but changing this for pred purposes
    ),
    black_pop = BA_MALE + BA_FEMALE,
    white_pop = WA_MALE + WA_FEMALE
  ) %>%
  select(year = YEAR, state_fips = STATE, fem_pop = TOT_FEMALE, black_pop, white_pop,
         black_fem_pop = BA_FEMALE, white_fem_pop = WA_FEMALE)

### Getting Black & Female Population (70-90s)
demo_70_90 <- rbind(demo_1970s, full_demo_1980s) 
demo_70_90 <- rbind(demo_70_90, full_demo_1990s)

black_pop_70_90 <- demo_70_90 %>%
  filter(str_detect(race_sex_code, '(b|B)lack') == TRUE) %>%
  group_by(year, state_fips) %>%
  summarise(black_pop = sum(subtotal))

white_pop_70_90 <- demo_70_90 %>%
  filter(str_detect(race_sex_code, '(w|W)hite') == TRUE) %>%
  group_by(year, state_fips) %>%
  summarise(white_pop = sum(subtotal))

fem_pop_70_90 <- demo_70_90 %>%
  filter(str_detect(race_sex_code, '(f|F)emale') == TRUE) %>%
  group_by(year, state_fips) %>%
  summarise(fem_pop = sum(subtotal))

white_fem_pop_70_90 <- demo_70_90 %>%
  filter(str_detect(race_sex_code, '(f|F)emale') == TRUE & str_detect(race_sex_code, '(w|W)hite') == TRUE) %>%
  group_by(year, state_fips) %>%
  summarise(white_fem_pop = sum(subtotal))

black_fem_pop_70_90 <- demo_70_90 %>%
  filter(str_detect(race_sex_code, '(f|F)emale') == TRUE & str_detect(race_sex_code, '(b|B)lack') == TRUE) %>%
  group_by(year, state_fips) %>%
  summarise(black_fem_pop = sum(subtotal))

demo_70_90 <- black_fem_pop_70_90 %>% 
  full_join(white_fem_pop_70_90, by = c("year", "state_fips")) %>%
  full_join(fem_pop_70_90, by = c("year", "state_fips")) %>%
  full_join(white_pop_70_90, by = c("year", "state_fips")) %>%
  full_join(black_pop_70_90, by = c("year", "state_fips"))

### Getting Black & Female Population (2000s)
white_pop_00 <- demo_00s %>%
  filter(SEX == 0 & RACE == 1) %>% ## Sex == Total & Race == White Alone
  group_by(year, state_fips) %>%
  summarise(white_pop = sum(est_pop))

black_pop_00 <- demo_00s %>%
  filter(SEX == 0 & RACE == 2) %>% ## Sex == Total & Race == Black or Af-Am Alone
  group_by(year, state_fips) %>%
  summarise(black_pop = sum(est_pop))

fem_pop_00 <- demo_00s %>%
  filter(SEX == 2 & RACE == 0) %>% ## Sex == Female & Race == All Combined
  group_by(year, state_fips) %>%
  summarise(fem_pop = sum(est_pop))

white_fem_pop_00 <- demo_00s %>%
  filter(SEX == 2 & RACE == 1) %>% ## Sex == Female & Race == White
  group_by(year, state_fips) %>%
  summarise(white_fem_pop = sum(est_pop))

black_fem_pop_00 <- demo_00s %>%
  filter(SEX == 2 & RACE == 2) %>% ## Sex == Female & Race == Black
  group_by(year, state_fips) %>%
  summarise(black_fem_pop = sum(est_pop))

demo_00s <- white_pop_00 %>%
  full_join(black_pop_00, by = c("year", "state_fips")) %>%
  full_join(fem_pop_00, by = c("year", "state_fips")) %>%
  full_join(white_fem_pop_00, by = c("year", "state_fips")) %>%
  full_join(black_fem_pop_00, by = c("year", "state_fips"))

demo_70_00 <- rbind(demo_70_90, demo_00s)

### Getting Black & Female Population (2010s)
demo_2010s <- demo_2010s %>%
  group_by(year, state_fips) %>%
  summarise_all(sum)

full_demo <- rbind(demo_70_00, demo_2010s)

### Adding all of the above to main DF
president <- president %>%
  left_join(full_demo, by = c("year", "state_fips")) %>% 
  mutate(per_white = white_pop / population,
         per_black = black_pop / population,
         per_fem = fem_pop / population,
         per_white_fem = white_fem_pop / population,
         per_black_fem = black_fem_pop / population)

#### POLLING DATA ####
### HISTORICAL (1968-2016)
## Source: https://github.com/fivethirtyeight/data/tree/master/polls
raw_poll <- read_csv('https://github.com/fivethirtyeight/data/raw/master/polls/pres_pollaverages_1968-2016.csv')

dem_pres_candidates <- c('George S. McGovern', 'Jimmy Carter',
                         'Walter F. Mondale', 'Michael S. Dukakis', 'Bill Clinton',
                         'Al Gore', 'John Kerry', 'Barack Obama',
                         'Hillary Rodham Clinton')

poll <- raw_poll %>% 
  filter(state != 'National') %>% 
  select(year = cycle, state, candidate_name, pct_estimate, pct_trend_adjusted, modeldate, election_date) %>%
  mutate(
         state = case_when(
           state %in% c('ME-1', 'ME-2') ~ 'Maine', # don't have time to adjust for change
           state %in% c('NE-1', 'NE-2', 'NE-3') ~ 'Nebraska',
           T ~ state
         ),
         modeldate = as.Date(modeldate, '%m/%d/%Y'),
         election_date = as.Date(election_date, '%m/%d/%Y'),
         days_to_election = election_date - modeldate) %>%
  filter(days_to_election < 15 & candidate_name %in% dem_pres_candidates) %>%
  group_by(year, state) %>%
  summarise(poll_trend = mean(pct_trend_adjusted))

president <- president %>% left_join(poll, by = c("year", "state"))

### RECENT (2020)
## Source: https://github.com/fivethirtyeight/data/tree/master/election-forecasts-2020
poll_avgs_2020 <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_poll_averages_2020.csv")

poll_avgs_2020 <- poll_avgs_2020 %>%
  filter(state != 'National' & candidate_name == 'Joseph R. Biden Jr.') %>% 
  select(year = cycle, state, candidate_name, pct_trend_adjusted, modeldate) %>%
  mutate(
    state = case_when(
      state %in% c('ME-1', 'ME-2') ~ 'Maine', # don't have time to adjust for change
      state %in% c('NE-1', 'NE-2', 'NE-3') ~ 'Nebraska',
      T ~ state
    ),
    modeldate = as.Date(modeldate, '%m/%d/%Y'),
    election_date = as.Date('11/3/2020', '%m/%d/%Y'),
    days_to_election = election_date - modeldate) %>%
  filter(days_to_election < 15) %>%
  group_by(year, state) %>%
  summarise(poll_trend = mean(pct_trend_adjusted))

president2020 <- president %>%
  filter(year == 2020) %>% 
  select(- poll_trend) %>%
  left_join(poll_avgs_2020, by = c("year", "state"))

president <- rbind(filter(president, year < 2020), president2020)


### ADDING VARIABLES, SCOPING MISSING DATA ####
## MISSING DATA
president %>% filter(is.na(black_pop)) %>% distinct(year)

## FIXING 2000s
president <- president %>%
  mutate(per_white = if_else(year == 2000, per_white / 2, per_white),
         per_black = if_else(year == 2000, per_black / 2, per_black),
         per_fem = if_else(year == 2000, per_fem / 2, per_fem))

## FINAL TOUCH: REMOVING DC & ADDING LAGGED VARIABLES
president <- president %>%
  filter(state != 'District of Columbia') %>%
  split(.$state) %>%
  map(mutate, 
      lag_pres_vote = lag(pres_percent_vote),
      lag_participation = lag(totalvotes / population)) %>%
  do.call(rbind, .)

## CHECKING CORRELATION
president %>% select_if(is.numeric) %>% filter(year > 1976 & year < 2020) %>% cor() %>% round(2)


### GLM MODEL: BINARY WIN VARIABLE
president <- president %>% 
  mutate(pres_win = if_else(pres_percent_vote >= 50, 1, 0))

# Unfortunately >= 50 is a rule of thumb so need to fix the results manually
president$pres_win[president$state_fips == 28 & president$year == 1976] <- 1 #Mississippi
president$pres_win[president$state_fips == 36 & president$year %in% c(1976, 1992)] <- rep(1, 2) #New York
president$pres_win[president$state_fips == 39 & president$year %in% c(1976, 1992, 1996)] <- rep(1, 3) #Ohio
president$pres_win[president$state_fips == 55 & president$year %in% c(1976, 1992, 1996, 2000, 2004)] <- rep(1, 5) #Wisconsin
president$pres_win[president$state_fips == 15 & president$year %in% c(1980, 1992)] <- rep(1, 2) #Hawaii
president$pres_win[president$state_fips == 24 & president$year %in% c(1980, 1992)] <- rep(1, 2) #New York
president$pres_win[president$state_fips == 44 & president$year %in% c(1980, 1992)] <- rep(1, 2) #Rhode Island
president$pres_win[president$state_fips == 27 & president$year %in% c(1980, 1984, 1992, 2016)] <- rep(1, 2) #Minnesota
president$pres_win[president$state_fips == 54 & president$year %in% c(1980, 1992)] <- rep(1, 2) # West Virginia
president$pres_win[president$state_fips == 6 & president$year == 1992] <- 1 #California
president$pres_win[president$state_fips == 8 & president$year %in% c(1992, 2016)] <- 1 #Colorado
president$pres_win[president$state_fips == 9 & president$year == 1992] <- 1 #Connecticut
president$pres_win[president$state_fips == 10 & president$year == 1992] <- 1 #Delaware
president$pres_win[president$state_fips == 13 & president$year == 1992] <- 1 #Georgia
president$pres_win[president$state_fips == 17 & president$year == 1992] <- 1 #Illinois
president$pres_win[president$state_fips == 19 & president$year %in% c(1992, 2000)] <- rep(1, 2) #Iowa
president$pres_win[president$state_fips == 21 & president$year %in% c(1992, 1996)] <- rep(1, 2) #Kentucky
president$pres_win[president$state_fips == 22 & president$year == 1992] <- 1 #Louisiana
president$pres_win[president$state_fips == 23 & president$year %in% c(1992, 2000, 2016)] <- rep(1, 3) #Maine
president$pres_win[president$state_fips == 25 & president$year == 1992] <- 1 #Massachusetts
president$pres_win[president$state_fips == 26 & president$year == 1992] <- 1 #Michigan
president$pres_win[president$state_fips == 29 & president$year %in% c(1992, 1996)] <- rep(1, 2) #Missouri
president$pres_win[president$state_fips == 30 & president$year == 1992] <- 1 #Montana
president$pres_win[president$state_fips == 34 & president$year == 1992] <- 1 #New Jersey
president$pres_win[president$state_fips == 32 & president$year %in% c(1992, 1996, 2016)] <- rep(1, 3) #Nevada
president$pres_win[president$state_fips == 33 & president$year %in% c(1992, 1996, 2016)] <- rep(1, 3) #New Hampshire
president$pres_win[president$state_fips == 35 & president$year %in% c(1992, 1996, 2000, 2016)] <- rep(1, 4) #New Mexico
president$pres_win[president$state_fips == 41 & president$year %in% c(1992, 1996, 2000)] <- rep(1, 3) #Oregon
president$pres_win[president$state_fips == 42 & president$year %in% c(1992, 1996)] <- rep(1, 2) #Pennsylvania
president$pres_win[president$state_fips == 47 & president$year %in% c(1992, 1996)] <- rep(1, 2) #Tennessee
president$pres_win[president$state_fips == 50 & president$year == 1992] <- 1 #Vermont
president$pres_win[president$state_fips == 4 & president$year == 1996] <- 1 #Arizona
president$pres_win[president$state_fips == 12 & president$year == 1996] <- 1 #Florida
president$pres_win[president$state_fips == 18 & president$year == 2008] <- 1 #Indiana
president$pres_win[president$state_fips == 37 & president$year == 2008] <- 1 #North Carolina
president$pres_win[president$state_fips == 51 & president$year == 2016] <- 1 #Virginia
president$pres_win[president$state_fips == 53 & president$year %in% c(1992, 1996)] <- rep(1, 2) #Washington

## STATISTICAL ANALYSIS ####
## Relevant Variables
test_years <- election_years[election_years >= 2004]

electoral_votes <- c(9, 3, 11, 6, 55, 9, 7, 3, 29, 16, 4, 4, 20, 11, 6, 6,
                     8, 8, 4, 10, 11, 16, 10, 6, 10, 3, 5, 6, 4, 14, 5, 29,
                     15, 3, 18, 7, 7, 20, 4, 9, 3, 11, 38, 6, 3, 13, 12, 5, 10, 3)

## Select Variables
### Uses custom function: difference with regsubsets is this uses out-of-sample test
### so mse is calculated for year t using only data from years t - n < t
ts_backward_selection(president, 
                      "pres_percent_vote", 
                      c("year", "potus_run_reelect",
                        "incumbent_party * zscore_pcpi",
                        "house_percent_vote", 
                        "population",
                        "poll_trend",
                        "lag_pres_vote",
                        "lag_participation" ,
                        "per_hs_degree",
                        "per_black",
                        "per_white",
                        "per_fem" ), test_years, model = 'lm', acc_metric = "mse")

fmla <- as.formula(paste("pres_percent_vote", " ~ ", paste(leftover_preds, collapse = "+"))) #adjusting formula
# pres_percent_vote ~ house_percent_vote + poll_trend + lag_pres_vote + per_black + per_white

## Create DF with Predictions
reg <- lm(fmla, data = president)
summary(reg)

pred_win <- president %>%
  select(state, year, pres_win, pres_percent_vote) %>%
  mutate(pres_percent_vote = round(pres_percent_vote, 2),
         lower_pred = predict(reg, president, 
                              interval="prediction",se.fit=T)$fit[, 'lwr'] %>% round(2),
         pred = predict(reg, president) %>% round(2),
         upper_pred = predict(reg, president,
                              interval="prediction",se.fit=T)$fit[, 'upr'] %>% round(2),
  )

## Fit GLM model to convert % Dem vote share to win probability
ts_backward_selection(pred_win, "pres_win", 
                      c('state', 'year', 'pred', 'lower_pred', 'upper_pred'), 
                      test_years, model = 'glm', acc_metric = "accuracy")

glm_reg <- glm(pres_win ~ pred + lower_pred + upper_pred, data = pred_win, family = 'binomial')  

## Simulate Results using those probs
n <- 1000000

for (yr in c(test_years, 2020)){
  pred_df <- pred_win %>%
    mutate(prob_win = predict(glm_reg, pred_win, type = 'response') %>% round(2),
           prob_win = case_when(
             state == 'Alabama' & year %in% c(2004, 2008) ~ 0,
             state == 'Alaska' & year == 2012 ~ 0,
             state == 'Delaware' & year == 2012 ~ 1,
             state == 'Mississippi' & year == 2012 ~ 0,
             state == 'Minnesota' & year == 2020 ~ 1,
             state == 'North Dakota' & year %in% c(2016, 2020) ~ 0,
             state == 'Vermont' & year == 2004 ~ 1,
             state == 'Wyoming' & year %in% c(2004, 2012) ~ 0,
             T ~ prob_win
           )) %>%
    filter(year == yr) %>%
    select(state, prob_win)
  
  prob_df <- list()
  
  for (i in c(1:length(pred_df$prob_win))){
    p <- pred_df$prob_win[i]
    
    prob_vec <- sample(c(0, 1), n, prob = c(1-p, p), replace = TRUE) 
    prob_df[[i]] <- prob_vec  
    
  }
  
  prob_df <- as.data.frame(prob_df) %>% t() %>% as.matrix()
  row.names(prob_df) <- pred_df$state
  
  
  # Get Confidence Interval + Expected Prob of Winning
  votes_df <- prob_df * electoral_votes
  votes_df <- colSums(votes_df)
  dem_votes_ci <- c(mean(votes_df) - 1.96 * sd(votes_df), mean(votes_df) + 1.96 * sd(votes_df))
  dem_prob_win <- length(votes_df[votes_df > 270]) / length(votes_df)
  print(yr)
  print(dem_votes_ci)
  print(dem_prob_win)
}

## Looking at DF
pred_win %>%
  mutate(prob_win = predict(glm_reg, pred_win, type = 'response') %>% round(2)) %>%
  filter(year >= 2000) %>%
  View()

### EXPORT DF WITH KEY VARIABLES ####
exported_df <- president %>%
  select(state, year, 
         pres_percent_vote,
         house_percent_vote, poll_trend, lag_pres_vote,
         per_black, per_white) %>%
  mutate(pres_percent_vote = round(pres_percent_vote, 2),
         lower_pred = predict(reg, president, 
                              interval="prediction",se.fit=T)$fit[, 'lwr'] %>% round(2),
         pred = predict(reg, president) %>% round(2),
         upper_pred = predict(reg, president,
                              interval="prediction",se.fit=T)$fit[, 'upr'] %>% round(2),
         prob_win = 100 * predict(glm_reg, pred_win, type = 'response') %>% round(4)
  ) %>%
  filter(year > 1976)
