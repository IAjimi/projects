## Libraries
library(tidyverse)

## Useful Variables
current_days_to_election <- as.numeric(as.Date('11/03/2020', '%m/%d/%Y') - Sys.Date())
swing_states <- c('Arizona', 'Florida', 'Georgia', 'Iowa', 'Michigan', 'Minnesota', 'Nevada',
                  'North Carolina', 'Ohio', 'Pennsylvania', 'Texas', 'Wisconsin')
dem_pres_candidates <- c('George S. McGovern', 'Jimmy Carter',
                         'Walter F. Mondale', 'Michael S. Dukakis', 'Bill Clinton',
                         'Al Gore', 'John Kerry', 'Barack Obama',
                         'Hillary Rodham Clinton')

rep_pres_candidates <- c('Richard M. Nixon', 'Gerald R. Ford', 'Ronald Reagan', 'George Bush',
                         'Bob Dole', 'George W. Bush', 'John McCain', 'Mitt Romney', 'Donald Trump')

## Importing Data
polls_eda <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_poll_averages_2020.csv")
polls_eda <- mutate(polls_eda, election_date = as.Date('11/03/2020', '%m/%d/%Y'))

## Joining and Cleaning Data
polls_eda <- raw_poll %>% 
  select(cycle, state, modeldate, candidate_name, pct_estimate, pct_trend_adjusted, election_date) %>%
  mutate(election_date = as.Date(election_date, '%m/%d/%Y')) %>%
  rbind(., polls_eda)

polls_eda <- polls_eda %>%
  filter(state != 'National' & cycle >= 2000) %>% 
  select(- pct_estimate) %>%
  mutate(
    state = case_when(
      state %in% c('ME-1', 'ME-2') ~ 'Maine', # don't have time to adjust for change
      state %in% c('NE-1', 'NE-2', 'NE-3') ~ 'Nebraska',
      T ~ state
    ),
    modeldate = as.Date(modeldate, '%m/%d/%Y'),
    days_to_election = election_date - modeldate) %>%
  filter(days_to_election <= 100) %>%
  group_by(cycle, days_to_election, state, candidate_name) %>%
  summarise(poll_trend = mean(pct_trend_adjusted))

## Donald Trump
polls_eda %>%
  filter(candidate_name == 'Donald Trump' & state %in% swing_states) %>%
  ggplot(aes(days_to_election, poll_trend, color = as.character(cycle),  group = state)) +
  geom_path(data = . %>% filter(cycle == 2016)) +
  geom_path(data = . %>% filter(cycle == 2020)) +
  geom_point(data = . %>% filter(cycle == 2020 & days_to_election == current_days_to_election), alpha = 0.8) +
  scale_x_reverse() +
  facet_wrap(~ state) +
  labs(x = 'Days to Election', y = 'Donald Trump % Poll', color = 'Year')

## Democrats
polls_eda %>%
  filter(candidate_name %in% c('Joseph R. Biden Jr.',  'Barack Obama', 'Hillary Rodham Clinton') &
           state %in% swing_states) %>%
  ggplot(aes(days_to_election, poll_trend, color = as.character(cycle),  group = state)) +
  geom_hline(yintercept = 50, alpha = 0.3, linetype = 'dashed') +
  geom_path(data = . %>% filter(cycle == 2008), alpha = 0.8) +
  geom_path(data = . %>% filter(cycle == 2012), alpha = 0.8) +
  geom_path(data = . %>% filter(cycle == 2016), alpha = 0.8) +
  geom_path(data = . %>% filter(cycle == 2020)) +
  geom_point(data = . %>% filter(cycle == 2020 & days_to_election == current_days_to_election), alpha = 0.8) +
  scale_color_manual(values = c('grey', 'grey', 'grey', "#2E74C0")) +
  scale_x_reverse() +
  facet_wrap(~ state) +
  labs(x = 'Days to Election', y = 'Democrat % Poll', color = 'Year') +
  theme_bw()

## POLLING GAP
polls_eda %>%
  filter(candidate_name %in% c('Joseph R. Biden Jr.', dem_pres_candidates, rep_pres_candidates) &
           state %in% swing_states) %>%
  mutate(party = if_else(candidate_name %in% rep_pres_candidates, 'Rep', 'Dem')) %>%
  select(-candidate_name) %>%
  spread(party, poll_trend) %>%
  mutate(point_spread = Dem - Rep) %>%
  ggplot(aes(days_to_election, point_spread, color = as.character(cycle),  group = state)) +
  geom_hline(yintercept = 0, alpha = 0.3, linetype = 'dashed') +
  #geom_hline(yintercept = mean(poll_error), alpha = 0.5, linetype = 'dashed') + # polling error
  #geom_hline(yintercept = - mean(poll_error), alpha = 0.5, linetype = 'dashed') + # polling error
  geom_path(data = . %>% filter(cycle == 2008)) +
  geom_path(data = . %>% filter(cycle == 2012)) +
  geom_path(data = . %>% filter(cycle == 2016)) +
  geom_path(data = . %>% filter(cycle == 2020)) +
  geom_point(data = . %>% filter(cycle == 2020) %>% filter(days_to_election == current_days_to_election), alpha = 0.8) +
  scale_color_manual(values = c('grey', 'grey', 'grey', "#2E74C0")) +
  scale_x_reverse() +
  facet_wrap(~ state) +
  labs(x = 'Days to Election', y = 'Spread % Poll Dem v. Rep', color = 'Year') +
  theme_bw()

