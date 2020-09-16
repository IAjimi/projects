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
  select(year, state, state_po, state_fips, senate_candidatevotes = candidatevotes, senate_percent_vote)

house <- house %>% 
  filter(party == 'democrat') %>%
  mutate(house_percent_vote = 100 * candidatevotes / totalvotes,
         year = year + 2) %>%
  select(year, state, state_po, state_fips, house_candidatevotes = candidatevotes, house_percent_vote) %>%
  group_by(year, state, state_po, state_fips) %>%
  summarize(mean_house_percent_vote = mean(house_percent_vote, na.rm = T),
            sd_house_percent_vote = sd(house_percent_vote, na.rm = T)) %>%
  mutate(sd_house_percent_vote = replace_na(sd_house_percent_vote, 0))

president <- president %>% 
  filter(party == 'democrat') %>%
  mutate(pres_percent_vote = 100 * candidatevotes / totalvotes) %>%
  select(year, state, state_po, state_fips, pres_percent_vote)

president <- president %>%
  left_join(house) %>%
  left_join(senate) 

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
  left_join(bea_personal_income_state, by = c("year", "state")) %>%
  mutate(senate_participation = senate_candidatevotes / population)

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
  select(year = X1, full_fips = X2, state_fips, race_sex_code = X3, under_19, adults, over_60, subtotal)

### 1980
### Source: https://www.census.gov/data/tables/time-series/demo/popest/1980s-county.html
demo_1980s <- read_excel("C:/Users/ia767/Downloads/pe-02.xls", 
                    skip = 5)

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
  select(year = `Year of Estimate`, full_fips = `FIPS State and County Codes`, state_fips, 
         race_sex_code = `Race/Sex Indicator`, under_19, adults, over_60, subtotal)

### 1990
### Source: https://www2.census.gov/programs-surveys/popest/datasets/1990-2000/counties/asrh/co-99-10.txt

### 2010
### Source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
demo_2010s <- read_csv("C:/Users/ia767/Downloads/cc-est2019-alldata.csv")

demo_2010s <- demo_2010s %>%
  select(STATE, YEAR, AGEGRP, TOT_FEMALE, BA_MALE, BA_FEMALE) %>%
  filter(AGEGRP != 0) %>% # removes total age slot
  mutate(
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
    AGEGRP = case_when(
      YEAR < 5 ~ "under_19",
      YEAR >= 5 & YEAR <= 12 ~ "adults",
      YEAR > 12 ~ "over_60"
    ),
    black_pop = BA_MALE + BA_FEMALE
  ) %>%
  select(year = YEAR, state_fips = STATE, AGEGRP, female_pop = TOT_FEMALE, black_pop)

########

# Get Total Black Population by Age Group
co_asr_7079 <- co_asr_7079 %>%
  group_by(year, state_fips, race_sex_code) %>%
  summarise_if(is.numeric, list(sum = sum, sd = sd))

pe_02 %>%
  group_by(year, state_fips, race_sex_code) %>%
  summarise_if(is.numeric, list(sum = sum, sd = sd))

####### NEED TO FIX YEAR FOR JOIN!!

president <- president %>%
  left_join(co_asr_7079) %>% 
  mutate(per_black = subtotal_black_sum / population)

## STATISTICAL ANALYSIS ####
reg <- lm(pres_percent_vote ~ year + state + 
            senate_percent_vote + senate_participation +
            mean_house_percent_vote + sd_house_percent_vote +
            population + per_capita_personal_income +
            per_black + adults_black_sum,
   data = president)
summary(reg)
