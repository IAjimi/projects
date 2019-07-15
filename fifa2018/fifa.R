library(readr)
library(tidyverse)
library(ggrepel)

CompleteDataset <- read_csv("C:/Users/ia767/Downloads/fifa-18-demo-player-dataset/CompleteDataset.csv")
CompleteDataset <-  select(CompleteDataset, -X1, -Photo, -Flag, - `Club Logo`) #trimming df

#cleaning up wage and value strings
cash_clean <- function(some_string){
  some_string <- str_replace(some_string, "???", "")
  some_string[str_detect(some_string, "M") == TRUE] <- as.numeric(str_replace(some_string[str_detect(some_string, "M") == TRUE] , "M", ""))*1000000
  some_string[str_detect(some_string, "K") == TRUE] <- as.numeric(str_replace(some_string[str_detect(some_string, "K") == TRUE] , "K", ""))*1000
  some_string <- as.numeric(some_string)
  some_string
}

CompleteDataset$Value <- cash_clean(CompleteDataset$Value)
CompleteDataset$Wage <- cash_clean(CompleteDataset$Wage)

##for some charac scores
#some are character because of "56-1" or "55+3" notation
#finding these columns
fix <- names(sapply(CompleteDataset, class))[(sapply(CompleteDataset, class) == "character") & #keeps character types only
                                        !(names(sapply(CompleteDataset, class)) %in% c("Name", "Nationality", "Club", "Preferred Positions", "main_position"))]
#sapply(CompleteDataset, class) shows type of for every column


for (i in c(1:ncol(CompleteDataset))){
  if (names(CompleteDataset)[i] %in% fix == TRUE){
    newstring <- rep(NA, nrow(CompleteDataset))
    
    for (j in c(1:nrow(CompleteDataset))){
      if (str_detect(CompleteDataset[j, i], "-") == TRUE){
        newstring[j] <- str_split(CompleteDataset[j, i], "-")[[1]][1] 
      } else {
        newstring[j] <- as.numeric(CompleteDataset[j, i])
      }
    }
    CompleteDataset[ , i] <- as.numeric(newstring)
  }
}


#preliminary plots
## top players
top_valued <- CompleteDataset %>% arrange(desc(Value)) %>% top_n(4, Value)
key_players <- top_valued$Name
key_players_id <- top_valued$Special

### wage & value
reg <- lm(Value ~ Wage, data = CompleteDataset) #regression that gets added to plot


ggplot(CompleteDataset, aes(Wage, Value, label = Name)) + 
  geom_jitter(aes(color = !(Name %in% key_players  & Special %in% key_players_id)), 
              alpha = 0.4) + 
  geom_abline(intercept = unname(reg$coefficients[1]), slope = unname(reg$coefficients[2]),
              alpha = 0.65, color = "red") + # regression line
  geom_text_repel(data = filter(CompleteDataset, 
                                Name %in% key_players & Special %in% key_players_id), color = "red") +
  #using negative inverts the scale
  scale_color_manual(guide = FALSE, #adding legend
                     values = c("red", "grey")) +  #legend labels
  labs(y = "Value (mn, ???)", 
       title = "Soccer Player Value v Wage", 
       subtitle = "Wage and Value Tend to Be Fairly Proportional",
       caption = "Source: Kaggle, FIFA 18 Database")

### wage & overall stats
ggplot(CompleteDataset, aes(Overall, Wage, label = Name)) + 
  geom_jitter(aes(color = !(Name %in% key_players  & Special %in% key_players_id)), 
              alpha = 0.4) + 
  geom_text_repel(data = filter(CompleteDataset, 
                                Name %in% key_players & Special %in% key_players_id), color = "red") +
  #using negative inverts the scale
  scale_color_manual(guide = FALSE, #adding legend
                     values = c("red", "grey")) +  #legend labels
  labs(x = "Overall Statistics", y = "Wage (???)", 
       title = "Soccer Player Wage v Overall Statistics", 
       subtitle = "Marginal Increases in Overall Stats Are Increasingly Costly",
       caption = "Source: Kaggle, FIFA 18 Database")

## potential
ggplot(CompleteDataset, aes(Potential - Overall, Wage, label = Name)) + 
  geom_jitter(aes(color = !(Name %in% key_players  & Special %in% key_players_id)), 
              alpha = 0.4) + 
  geom_text_repel(data = filter(CompleteDataset, 
                                Name %in% key_players & Special %in% key_players_id), color = "red") +
  #using negative inverts the scale
  scale_color_manual(guide = FALSE, #adding legend
                     values = c("red", "grey")) +  #legend labels
  labs(y = "Wage (???)", 
       title = "Soccer Player Wage & Potential-to-Current Overall Statistic Gap", 
       subtitle = "Neymar's Current Wage May Be Low Due To Potential Growth Uncertainty",
       caption = "Source: Kaggle, FIFA 18 Database"
       )

### age and value
ggplot(CompleteDataset, aes(Age, Value/1000000, label = Name)) + 
  geom_jitter(aes(color = !(Name %in% key_players  & Special %in% key_players_id)), 
              alpha = 0.4) + 
  #geom_smooth(alpha = 0.2, se = FALSE, color = "red") +
  geom_text_repel(data = filter(CompleteDataset, 
                                Name %in% key_players & Special %in% key_players_id), color = "red") +
  #using negative inverts the scale
  scale_color_manual(guide = FALSE, #adding legend
                     values = c("red", "grey")) +  #legend labels
  labs(y = "Value (mn, ???)", 
       title = "Soccer Player Value and Age", 
       subtitle = "Players' Value Decreases With Age, Including for Top Players",
       caption = "Source: Kaggle, FIFA 18 Database")

### age and WAGE
ggplot(CompleteDataset, aes(Age, Wage, label = Name)) + 
  geom_jitter(aes(color = !(Name %in% key_players  & Special %in% key_players_id)), 
              alpha = 0.4) + 
  #geom_smooth(alpha = 0.2, se = FALSE, color = "red") +
  geom_text_repel(data = filter(CompleteDataset, 
                                Name %in% key_players & Special %in% key_players_id), color = "red") +
  #using negative inverts the scale
  scale_color_manual(guide = FALSE, #adding legend
                     values = c("red", "grey")) +  #legend labels
  labs(y = "Wage (???)", 
       title = "Soccer Player Value and Age", 
       subtitle = "Players' Value Decreases With Age, Including for Top Players",
       caption = "Source: Kaggle, FIFA 18 Database")

#### tipping point for wage and value are different
graphing_select <- CompleteDataset %>% mutate(smaller_Value = Value/100) %>%
  select(Name, Age, Special, Wage, smaller_Value)

gather(graphing_select, measure, number, Wage:smaller_Value) %>%
  ggplot(aes(Age, number, label = Name)) +
  geom_jitter(aes(color = !(Name %in% key_players  & Special %in% key_players_id)), 
              alpha = 0.4) + 
  geom_smooth(alpha = 0.2, se = FALSE, color = "red") +
  scale_color_manual(guide = FALSE, #adding legend
                     values = c("red", "grey")) +  #legend labels
  geom_text_repel(data = . %>% filter(Name %in% key_players & Special %in% key_players_id), color = "red") +
  labs(y = "", 
       title = "Soccer Player Wage and Value per Age", 
       caption = "Source: Kaggle, FIFA 18 Database") +
  facet_grid(~ measure)




#### do clubs overpay?



## Mean Value & Points per Club
clubs <- CompleteDataset %>% group_by(`Club`) %>% summarize(value = mean(Value), charac = mean(Overall),
                                                   age = mean(Age)) %>% arrange(desc(charac))

ggplot(clubs, aes(value, charac)) + geom_point()
ggplot(CompleteDataset, aes(Overall, Value)) + geom_jitter(alpha = 0.5)
ggplot(CompleteDataset, aes(Overall, Wage)) + geom_jitter(alpha = 0.5)
#theory: value increases so sharply because a marginal increase in talent at high levels
#yields higher returns + overall scores from FIFA don't capture 'intangibles'
#either skill or additional income (star power) that comes from player
### star economics: long tail
ggplot(CompleteDataset, aes(log(Overall), log(Value))) + geom_jitter(alpha = 0.5)

#maybe some clubs are flush with money & overpay??
key_clubs <- CompleteDataset %>% group_by(`Club`) %>% summarize(value = mean(Value)) %>% 
  arrange(desc(value)) %>% top_n(10)
top_clubs <- key_clubs$Club 


ggplot(CompleteDataset, aes(Overall, Value)) + 
  geom_jitter(alpha = 0.5, aes(color = !(Club %in% top_clubs)), alpha = 0.6) + 
  #using negative inverts the scale
  scale_color_manual("", #name of legend
                      guide = "legend", #adding legend
                      values = c("red", "lightgrey"), #colors used
                      labels = c("Top Clubs", "Others"))  #legend labels
##seems like players vary quite a bit even within these top clubs

##maybe position matters?
CompleteDataset$main_position <- NA
CompleteDataset$main_position <- sapply(strsplit(CompleteDataset$`Preferred Positions`, " "), `[`, 1)

top_positions <- CompleteDataset %>% group_by(`main_position`) %>% summarize(worth = mean(Value)) %>% 
  arrange(desc(worth)) %>% top_n(3)
bottom_positions <- CompleteDataset %>% group_by(`main_position`) %>% summarize(worth = mean(Value)) %>% 
  arrange(worth) %>% top_n(3)
top_positions <- top_positions$main_position 
bottom_positions <- bottom_positions$main_position

##tree regression
tree <- rpart(Value ~ Special + Acceleration + Aggression + Agility + Balance + `Ball control` + 
                Composure + Crossing + Curve + Dribbling + Finishing + Interceptions, 
              data = filter(CompleteDataset, main_position %in% c("ST", "LW", "RW")))

