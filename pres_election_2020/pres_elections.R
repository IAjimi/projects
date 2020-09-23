## ADDING DEMOGRAPHICS
## ADDING URBAN % POP


## SETUP ####
### LOADING LIBRARIES
library(readr)
library(readxl)
library(tidyverse)

### LOADING DATA
senate <- read_csv("C:/Users/ia767/Downloads/dataverse_files/1976-2018-senate.csv")
house <- read_csv("C:/Users/ia767/Downloads/house_dataverse_files/1976-2018-house2.csv")
president <- read_csv("C:/Users/ia767/Downloads/president_dataverse_files/1976-2016-president.csv")

## ELECTORAL DATA ####
senate <- senate %>% 
  filter(party == 'democrat') %>%
  mutate(senate_percent_vote = 100 * candidatevotes / totalvotes,
         year = year + 2) %>%
  select(year, state, state_po, state_fips, senate_candidatevotes = candidatevotes, senate_percent_vote) %>%  group_by(year, state, state_po, state_fips) %>%
  summarize(mean_senate_percent_vote = mean(senate_percent_vote, na.rm = T)) 

house <- house %>% 
  filter(party == 'democrat') %>%
  mutate(house_percent_vote = 100 * candidatevotes / totalvotes,
         year = year + 2) %>%
  select(year, state, state_po, state_fips, house_candidatevotes = candidatevotes, house_percent_vote) %>%
  group_by(year, state, state_po, state_fips) %>%
  summarize(mean_house_percent_vote = mean(house_percent_vote, na.rm = T),
            sd_house_percent_vote = sd(house_percent_vote, na.rm = T)) %>%
  mutate(sd_house_percent_vote = replace_na(sd_house_percent_vote, 0))

pres_participation <- president %>% distinct(year, state_fips, totalvotes)

president <- president %>% 
  filter(party == 'democrat') %>%
  mutate(pres_percent_vote = 100 * candidatevotes / totalvotes) %>%
  select(year, state, state_po, state_fips, pres_percent_vote) %>%
  mutate(
    incumbent_party = case_when(
      year == 1976 ~ 'R',
      year == 1980 ~ 'D',
      year >= 1984 & year <= 1992 ~ 'R',
      year >= 1996 & year <= 2000 ~ 'D',
      year >= 2004 & year <= 2008 ~ 'R',
      year >= 2012 & year <= 2016 ~ 'D',
      year == 2020 ~ 'R'
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
bea_personal_income_state <- read_excel("C:/Users/ia767/Downloads/bea_personal_income_state.xls", 
                                        skip = 5)

bea_personal_income_state <- bea_personal_income_state %>% 
  gather(year, val, `1976`:`2019`) %>%
  mutate(Description = if_else(Description == 'Population (persons) 1/', 'population', 'per_capita_personal_income'),
         year = as.numeric(year)) %>% # gather changed it to character
  select(-LineCode) %>% # otherwise spread doesn't fill properly
  spread(Description, val) %>%
  select(state = GeoName, year, per_capita_personal_income, population) 

president <- president %>%
  left_join(bea_personal_income_state, by = c("year", "state"))

## Census Data (https://www2.census.gov/programs-surveys/popest/datasets/) ####
### 1970: Race of Population by County
### Codebook: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/1900-1980/co-asr-7079-layout.txt
demo_1970s <- read_csv("C:/Users/ia767/Downloads/co-asr-7079.csv", 
                        col_names = FALSE)

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
  
  file_path <- paste("C:/Users/ia767/Downloads/pe-02-", i, ".xls", sep = '')
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
demo_00s <- read_csv("C:/Users/ia767/Downloads/co-est00int-sexracehisp.csv")
demo_00s <- demo_00s %>%
  select(-SUMLEV, - COUNTY, -STNAME, -CTYNAME, -POPESTIMATE2010) %>%
  gather(year, est_pop, ESTIMATESBASE2000:CENSUS2010POP) %>%
  filter(ORIGIN == 0) %>% # ORIGIN diff. btw Hispanic and Non-Hispanic, this keeps total of both
  mutate(year = parse_number(year),
         state_fips = as.numeric(STATE),
        )

### 2010
### Source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
demo_2010s <- read_csv("C:/Users/ia767/Downloads/cc-est2019-alldata.csv")

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
      YEAR == 12 ~ 2019
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

### ADDING VARIABLES, SCOPING MISSING DATA ####
## MISSING DATA
president %>% filter(is.na(black_pop)) %>% distinct(year)

## 
president <- president %>%
  mutate(past_participation = lag(totalvotes / population))

## STATISTICAL ANALYSIS ####
## IMPROVING PREDICTIONS
### VARIABLE SELECTION: PICKING RIGHT VARS FOR MODEL
### ADDING RELEVANT VARS TO MODEL
### lag(past_Vote), sd(population), share of jobs in X sector
### GDP

reg <- lm(pres_percent_vote ~ year + state + 
            incumbent_party +
            past_participation + 
            mean_senate_percent_vote +
            mean_house_percent_vote + sd_house_percent_vote +
            population + per_capita_personal_income +
            per_white + per_black + per_fem + per_white_fem + per_black_fem,
   data = president)
summary(reg)

library(leaps)
models <- regsubsets(pres_percent_vote ~ year + state + 
                       incumbent_party +
                       past_participation + 
                       mean_senate_percent_vote +
                       mean_house_percent_vote + sd_house_percent_vote +
                       population + per_capita_personal_income +
                       per_white + per_black + per_fem + per_white_fem + per_black_fem,
                     data = president,
                     nvmax = 25,
                     method = "seqrep")
summary(models)


### TESTING ON 2016 ####
partial_reg <- lm(pres_percent_vote ~ year + state + 
                    incumbent_party +
                    past_participation + 
                    mean_senate_percent_vote +
                    mean_house_percent_vote + sd_house_percent_vote +
                    population + per_capita_personal_income +
                    per_white + per_black + per_fem + per_white_fem + per_black_fem,
          data = filter(president, year != 2016))
summary(partial_reg)

partial_reg <- lm(pres_percent_vote ~ year + state + 
                    incumbent_party +
                    mean_senate_percent_vote +
                    mean_house_percent_vote +
                    population + per_capita_personal_income +
                    per_black,
                  data = filter(president, year >= 1990))
summary(partial_reg)


## Comparing Predictions (Vote Share)
predicted_res <- predict(partial_reg, 
        filter(president, year == 2016 & !(state %in% c('Alaska', 'Arizona', 'District of Columbia','Idaho',  'Hawaii'))))

actual_res <- filter(president, year == 2016 & !(state %in% c('Alaska', 'Arizona', 'District of Columbia','Idaho',  'Hawaii')))$pres_percent_vote

president %>%
  filter(year == 2016 & !(state %in% c('Alaska', 'Arizona', 'District of Columbia','Idaho',  'Hawaii'))) %>%
  mutate(preds = predicted_res) %>%
  select(state, pres_percent_vote, preds) %>%
  View()

## Comparing Predictions (Outcome)
predicted_res[predicted_res < 50] <- 0
predicted_res[predicted_res >= 50] <- 1

actual_res[actual_res < 50] <- 0
actual_res[actual_res >= 50] <- 1

lm_error <- predicted_res - actual_res
lm_error[lm_error %in% c(1, -1)]

#### GLM / LOGIT-PROBIT
president <- president %>%
  mutate(win_dummy = if_else(pres_percent_vote > 50, 1, 0))

reg <- glm(win_dummy ~pres_percent_vote ~ year + state + 
             incumbent_party +
             past_participation + 
             mean_senate_percent_vote +
             mean_house_percent_vote + sd_house_percent_vote +
             population + per_capita_personal_income +
             per_white + per_black + per_fem + per_white_fem + per_black_fem,
           data = president, family = "binomial")
summary(reg)

glm_preds <- predict(reg, 
                     filter(president, year == 2016 & !(state %in% c('Alaska', 'Arizona', 'District of Columbia','Idaho',  'Hawaii'))),
                     type="response") %>%
  round(2)

glm_preds[glm_preds < 50] <- 0
glm_preds[glm_preds >= 50] <- 1

error <- glm_preds - actual_res
error[error %in% c(1, -1)]
## 6 out of 29 states wrong
