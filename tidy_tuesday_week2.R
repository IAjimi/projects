####Week 2: NFL Cap Values####
library("cowplot")
library("tidyverse")
library(fiftystater)

tidy_tuesday_week2 <- read_excel("C:/Users/ia767/Downloads/tidy_tuesday_week2.xlsx")
nfl <- tidy_tuesday_week2
names(nfl) <- c("year", "CB", "DL", "LB", "OL", "QB", "RB", "S", "ST", "TE", "WR")

nfl_long <- nfl %>% gather(position, salary, 2:11) #gathers data so we can use facet grid more easily

nfl_long$role <- NA #adds type of role: offensive v defensive
nfl_long$role[nfl_long$position %in% c("OL", "QB", "RB", "TE", "WR")] <- "Offensive"
nfl_long$role[!(nfl_long$position %in% c("OL", "QB", "RB", "TE", "WR"))] <- "Defensive"


off_plot <- nfl_long %>% filter(role == "Offensive") %>%  #creates grid for offensive positions
  ggplot( aes(year, salary/1000000))+geom_jitter(alpha = 0.3) + 
  geom_smooth(aes(color = "#ea9b41"), size = 2, se = FALSE, show.legend = FALSE) + facet_grid(. ~ position) +
  labs(x = "Year", y = "Salary (mn Dollars)")

def_plot <- nfl_long %>% filter(role != "Offensive") %>% #creates grid for defensive positions
  ggplot( aes(year, salary/1000000))+geom_jitter(alpha = 0.3) + 
  geom_smooth(aes(color = "#ea9b41"), size = 2, se = FALSE, show.legend = FALSE) + facet_grid(. ~ position) +
  labs(x = "Year", y = "Salary (mn Dollars)")

plot_grid(off_plot, def_plot, labels = c('Off', 'Def'), nrow = 2) #groups both grids together
