### Loading Libraries ####
library(readr)
library(tidyverse)
library(sandwich)
library(lmtest)
library(ISLR)
library(leaps)

### Loading Data ####
NCAA_brackets <- read_csv("C:/Users/ia767/Downloads/NCAA 2018 - Sheet3 (1).csv")
NCAA_2018_1 <- read_csv("C:/Users/ia767/Downloads/NCAA 2018 - ESPN.csv")
NCAA_2018_2 <- read_csv("C:/Users/ia767/Downloads/NCAA 2018 - Kenpom.csv", 
                             skip = 1)

### Preparing Data ####
### Changing Column Names
names(NCAA_brackets) <- str_replace(names(NCAA_brackets), " ", "_")
names(NCAA_2018_1) <- str_replace(names(NCAA_2018_1), " ", "_")

### Adding a Proper Wins and Losses Column to NCAA_2018_1
NCAA_2018_1$Wins <- NA
NCAA_2018_1$Losses <- NA

for(i in c(1:nrow(NCAA_2018_1))) {
  NCAA_2018_1$Wins[i] <- str_split(NCAA_2018_1$`W-L`, "-")[[i]][1] #selects number before dash (wins)
  NCAA_2018_1$Losses[i] <- str_split(NCAA_2018_1$`W-L`, "-")[[i]][2] #selects number after dash (losses)
}

NCAA_2018_1$Wins <- as.numeric(NCAA_2018_1$Wins) #saves results in a df column, as numeric
NCAA_2018_1$Losses <- as.numeric(NCAA_2018_1$Losses)

### Trimming DF
NCAA_2018_1 <- NCAA_2018_1 %>% select(RK, TEAM, BPI, BPI_OFF, BPI_DEF, Wins, Losses)

### Preparing Team Names for DF Merge
#some names are written differently between files

##starting with NCAA_2018_1
#first I find the names that already match
teams <- unique( NCAA_2018_1$TEAM[NCAA_2018_1$TEAM %in% c( NCAA_brackets$Team_1, NCAA_brackets$Team_2)] )
length(teams) #45 match

#then the ones which don't
unique(NCAA_brackets$Team_1[!(NCAA_brackets$Team_1 %in% teams)])
unique(NCAA_brackets$Team_2[!(NCAA_brackets$Team_2 %in% teams)])

#one problem is the use of "St." as an abbreviation for state
NCAA_brackets$Team_1 <- str_replace(NCAA_brackets$Team_1 , "St.", "State")
NCAA_brackets$Team_2 <- str_replace(NCAA_brackets$Team_2 , "St.", "State")

teams <- unique( NCAA_2018_1$TEAM[NCAA_2018_1$TEAM %in% c( NCAA_brackets$Team_1, NCAA_brackets$Team_2)] )
length(teams) #now 53 match

#let's try to detect the other problems
teams_no_match <- unique(c(NCAA_brackets$Team_1[!(NCAA_brackets$Team_1 %in% teams)],
                    NCAA_brackets$Team_2[!(NCAA_brackets$Team_2 %in% teams)]))

for(i in c(1:length(teams_no_match))) {
  print( unique(NCAA_2018_1$TEAM[str_detect(NCAA_2018_1$TEAM, teams_no_match[i])]) )
}

#Loyola-Chicago, Kansas State and Penn State need to be changed
NCAA_2018_1$TEAM[str_detect(NCAA_2018_1$TEAM, "Loyola")] <- "Loyola"
NCAA_brackets$Team_1[str_detect(NCAA_brackets$Team_1, "Kansas")] <- "Kansas State"
NCAA_brackets$Team_2[str_detect(NCAA_brackets$Team_2, "Kansas")] <- "Kansas State"
NCAA_brackets$Team_1[str_detect(NCAA_brackets$Team_1, "Penn")] <- "Penn State"
NCAA_brackets$Team_2[str_detect(NCAA_brackets$Team_2, "Penn")] <- "Penn State"

### for NCAA_2018_2, different team names
#problem: seed number is included in team name
#so if there is a number
#remove it and remove last character
new_team <- rep(NA, length(NCAA_2018_2$Team))        

for (i in c(1:length(NCAA_2018_2$Team))) {
  if (str_detect(NCAA_2018_2$Team, '[0-9]+')[i] == TRUE) { #if number
      interm <- strsplit(NCAA_2018_2$Team, ' ')[[i]] #splits string for each space
      n <- length(interm) #stores length of list
      new_team[i] <- paste0(( strsplit(NCAA_2018_2$Team, ' ')[[i]][-n]), collapse = " ")
  #strsplit(NCAA_2018_2$Team, ' ')[[i]][-n] picks everything but last split (the number)
  #paste0 collapse all the splits back together
  } else {
    new_team[i] <- NCAA_2018_2$Team[i] #if no number, save as is
  }
}

NCAA_2018_2$Team <- new_team #saving results

#same problem as in NCAA_brackets where st. is used as abbrv. for state
NCAA_2018_2$Team <- str_replace(NCAA_2018_2$Team , "St.", "State")

teams <- unique( NCAA_2018_2$Team[NCAA_2018_2$Team %in% c( NCAA_brackets$Team_1, NCAA_brackets$Team_2)] )
length(teams) #55 match

#same as before: let's try to find the other problems
teams_no_match <- unique(c(NCAA_brackets$Team_1[!(NCAA_brackets$Team_1 %in% teams)],
                           NCAA_brackets$Team_2[!(NCAA_brackets$Team_2 %in% teams)]))

for(i in c(1:length(teams_no_match))) {
  print( unique(NCAA_2018_2$Team[str_detect(NCAA_2018_2$Team, teams_no_match[i])]) )
}

#we need to change Miami FL to Miami and Loyola Chicago to Loyola
NCAA_2018_2$Team[str_detect(NCAA_2018_2$Team, "Loyola")] <- "Loyola"
NCAA_2018_2$Team[str_detect(NCAA_2018_2$Team, "Miami")] <- "Miami"

## Merging
#first separately. every df has a lot of columns, want to find best vars for each df individually
NCAA_big <- left_join(NCAA_brackets, NCAA_2018_1, by = c("Team_1" = "TEAM"))
NCAA_big <- left_join(NCAA_big, NCAA_2018_1, by = c("Team_2" = "TEAM"), suffix = c("_1", "_2") )

NCAA_big2 <- left_join(NCAA_brackets, NCAA_2018_2, by = c("Team_1" = "Team"))
NCAA_big2 <- left_join(NCAA_big2, NCAA_2018_2, by = c("Team_2" = "Team"), suffix = c("_1", "_2") )

NCAA_big <- filter(NCAA_big, !is.na(BPI_1), !is.na(BPI_2) )
NCAA_big2 <- filter(NCAA_big2, !is.na(Rk_1), !is.na(Rk_2) )


### NCAA_2018_1 #####
### NCAA_2018_1 Linear Regression -- Predicting Score Gap ####
#using forward subset regression
reg <- regsubsets(Score_Gap ~ BPI_1+ BPI_OFF_1 + BPI_DEF_1 + Wins_1 + BPI_2+BPI_OFF_2 + 
                    BPI_DEF_2 + Wins_2 , data = NCAA_big, nvmax = 10)
reg_sum <- summary(reg) 

which.max(reg_sum$adjr2) #biggest adj rsq -> 4
which.min(reg_sum$rss) #lowest rss -> 8
which.min(reg_sum$cp) #lowest cp -> 4
which.min(reg_sum$bic) #lowest bic -> 2 

#model 2 has BPI_DEF_1 and Wins 2
#model 4 has BPI_1, BPI_OFF_1, BPI_DEF_1 and Wins 2 -> high corr between BPI_OFF_1 and BPI_1 so not 
#best idea
#model 8 has everything -> also not best idea


### NCAA_2018_1 #####
### NCAA_2018_1 Linear Regression -- Predicting Score Gap ####
#same logic!

#using forward subset regression on first subset of variables
reg <- regsubsets(Score_Gap ~ AdjEM_1_1 + AdjO_1 + AdjD_1 + AdjT_1 + Luck_1 + AdjEM_1_1_1 +
                    OppO_1 + OppD_1 , data = NCAA_big2, nvmax = 10 , method = "forward")  #important to use raw = TRUE
reg_sum <- summary(reg) 

which.max(reg_sum$adjr2) #biggest adj rsq -> 3
which.min(reg_sum$rss) #lowest rss -> 8
which.min(reg_sum$cp) #lowest cp -> 3
which.min(reg_sum$bic) #lowest bic -> 3 most accurate criterion

#most concise & accurate seems to be the following
reg2 <- lm(Score_Gap ~ AdjD_1 + AdjT_1 + OppD_1 , data = NCAA_big2)
summary(reg2) 

#second batch of variables
reg <- regsubsets(Score_Gap ~ AdjEM_2_1 + AdjEM_2_2 + AdjO_2 + AdjD_2 + AdjT_2 + Luck_2  + AdjEM_1_2 + 
                    OppO_2 + OppD_2 + AdjEM_2_2_2 , data = NCAA_big2, nvmax = 10 , method = "forward")  #important to use raw = TRUE
reg_sum <- summary(reg) 

which.max(reg_sum$adjr2) #biggest adj rsq -> 2
which.min(reg_sum$cp) #lowest cp -> 2
which.min(reg_sum$bic) #lowest bic -> 2 most accurate criterion

#best variables here are AdjEM_2_2, AdjEM_2_2_2
#they are *not* the same thing 
#cor(NCAA_big2$AdjEM_2_2_2, NCAA_big2$AdjEM_2_2)

reg2 <- lm(Score_Gap ~ AdjEM_2_2_2 + AdjEM_2_2, data = NCAA_big2)
summary(reg2) 

#now combining best variables of both batches
reg <- regsubsets(Score_Gap ~ AdjD_1 + AdjT_1 + OppD_1 + AdjEM_2_2 + AdjEM_2_2_2, data = NCAA_big2, nvmax = 10 , method = "forward")  #important to use raw = TRUE
reg_sum <- summary(reg) 

which.max(reg_sum$adjr2) #biggest adj rsq -> 5
which.min(reg_sum$cp) #lowest cp -> 5
which.min(reg_sum$bic) #lowest bic -> 5 

### Combining the two big DFs ####
#first trimming the dfs
a <- select(NCAA_big2, Team_1, Team_2, Score_Gap, Win, AdjD_1, AdjT_1, OppD_1, AdjEM_2_2, AdjEM_2_2_2)
b <- select(NCAA_big,  Team_1, Team_2, Score_Gap, Win, BPI_DEF_1, Wins_2)
NCAA <- left_join(a, b, by = c("Team_1", "Team_2", "Score_Gap", "Win"))

#another subset regression
reg <- regsubsets(Score_Gap ~ AdjD_1 + AdjT_1 + OppD_1 + AdjEM_2_2 + AdjEM_2_2_2 +
                    BPI_DEF_1 + Wins_2, data = NCAA, nvmax = 10) 
reg_sum <- summary(reg) 

which.max(reg_sum$adjr2) #biggest adj rsq -> 4
which.min(reg_sum$cp) #lowest cp -> 4
which.min(reg_sum$bic) #lowest bic -> 2

#gives us  AdjT_1 + (AdjEM_2_2_2) + BPI_DEF_1 + (Wins_2)
