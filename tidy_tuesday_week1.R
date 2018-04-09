####TIDY TUESDAY####
####Week 1, U.S. Av. Tuition####
###packages
library("tidyverse")
library(fiftystater)

###loading data (from https://github.com/rfordatascience/tidytuesday/tree/master/data)
us_avg_tuition <- read_excel("us_avg_tuition.xlsx")

###cleaning/shaping data
#changing column names
names(us_avg_tuition) <- c("State", paste("year", seq(2004, 2015, 1), sep = "_")) #change col names

#changes states to lowercase (used for maps later)
us_avg_tuition$State <- tolower(us_avg_tuition$State)

#add state abbreviation (the states are ordered so shld be fine)
us_avg_tuition$state_abb <- state.abb

#adding net change and percentage change columns
us_avg_tuition <- mutate(us_avg_tuition, net_change_0515 = year_2015 - year_2005,
                         per_change_0515 = 100 * (year_2015 - year_2005) / (year_2005)) 

#creating U.S. average 
us_avg_tuition[51, ] <- NA
us_avg_tuition$State[51] <- "USA" #creating usa entry
us_avg_tuition$state_abb[51] <- "US"
us_avg_tuition[51, -c(1, 14)] <-  unname(apply(us_avg_tuition[1:50, -c(1, 14)], 2, mean)) #with values = mean of all other states

###Plots
##Creating Heat Map
#shows ten-year change in inflation adjusted fees
#mainly from https://cran.r-project.org/web/packages/fiftystater/vignettes/fiftystater.html
ggplot(us_avg_tuition, aes(map_id = State)) + 
  geom_map(aes(fill = us_avg_tuition$per_change_0515), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + #gets rid of x axis labels
  scale_y_continuous(breaks = NULL) + #gets rid of y axis labels
  scale_fill_continuous(low =  "#ffd8b5", high = "#ff0000",
    breaks=c(75,50,25), name = "Percentage Change", guide = "legend") +
  labs(x = "", y = "", title = "Percentage Change in State Tuition Fees, U.S. 2005-2015") +
  theme(legend.position = "right", 
        panel.background = element_blank()) #gets rid of grid


##bar chart
#creating labels
abbs <- arrange(us_avg_tuition, us_avg_tuition$net_change_0515)[14] #arranges df by net change
state_abb_2 <- abbs$state_abb #selects state abbs column of abbs (abbs is a tibble, not vector)
state_abb_2[seq(0, 51, 2)] <- "" #replaces 1 in 2 state abb by empty space so x label is legible

#creating bar plot
ggplot(us_avg_tuition) + geom_col(aes(fct_reorder(State, net_change_0515), net_change_0515, 
                            fill = per_change_0515)) +
  scale_fill_continuous(low =  "#ffd8b5", high = "#ff0000",
                        breaks=c(75,50,25), name = "Percentage Change",  guide = "legend") +
  scale_x_discrete(labels= state_abb_2) + 
  labs(x = "", y = "Net Change (U.S. Dollars)", title = "Net Change in State Tuition Fees, U.S. 2005-2015") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), #tilts axis 
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) #hides vertical grid lines 

