library(class) #new package
library(readr)
library(tidyverse)
library(sandwich)
library(lmtest)

####Data####
#https://github.com/scruwys/and-the-award-goes-to/blob/master/data/nominations.csv
nominations <- read_csv("C:/Users/ia767/Downloads/nominations.csv")  %>% 
  select(-href) %>% filter(year >= 1995)

#adding 2016 results
new_results <- (nominations$year == 2016 & nominations$award == "Oscar")
nominations$winner[new_results & nominations$name == "Casey Affleck"] <- 1
nominations$winner[new_results & nominations$name == "Emma Stone"] <- 1
nominations$winner[new_results & nominations$name == "Mahershala Ali"] <- 1
nominations$winner[new_results & nominations$name == "Viola Davis"] <- 1
nominations$winner[new_results & nominations$name == "Damien Chazelle"] <- 1


#counting total MOVIE nominations
nom_numb <- nominations %>% filter(award == "Oscar") %>% count(film)
nominations <- left_join(nominations, nom_numb) #adding to df

#splitting df to change pivot some columns
oscars <- nominations %>% filter(award == "Oscar" & category == "Actress")
others <- nominations %>% filter(award != "Oscar" & category == "Actress") %>%
  spread(award, winner) #changes df so each award is a column

#merging df
oscars <- left_join(oscars, others, by = c("category", "film", "name", "year"))
names(oscars)[names(oscars) == 'Golden Globe'] <- "GG" #renaming golden globes

#implementing -1 penalty
oscars$BAFTA[is.na(oscars$BAFTA)] <- -1
oscars$GG[is.na(oscars$GG)] <- -1
oscars$Guild[is.na(oscars$Guild)] <- -1 #choice! tech. SAG only started in 1995


###finding out optimal K####
set.seed(1) #set seed bc knn() breaks ties using random numbers

attempts <- c(1:100) #number of times to run k
n_k <- c(1:89) #number of ks to try
class_corr <- rep(0, max(n_k))
class_corr_av <- rep(0, max(attempts))
best_k <- rep(0, max(attempts)) #k that min error rate

for (i in attempts) {
  random_order <- sample(c(1:nrow(oscars)), nrow(oscars), replace = FALSE) #shuffles row numbers
  test <- random_order[1:(nrow(oscars)/5)] #picks first 1/5 of rows
  train <- random_order[(nrow(oscars)/5):nrow(oscars)]
  
  train_X <- cbind(oscars$year, oscars$n.x, oscars$GG, oscars$Guild, oscars$BAFTA)[train, ] #predictors
  test_X <- cbind(oscars$year, oscars$n.x, oscars$GG, oscars$Guild, oscars$BAFTA)[test, ]
  train_winner <- oscars$winner[train]
  
  for (j in n_k){
    knn_pred <- knn(train_X, test_X, train_winner, k = j)
    table(knn_pred, oscars$winner[test])
    class_corr[j] <- 1 - mean(knn_pred == oscars$winner[test]) #error rate
  }
  
  class_corr_av[i] <- mean(class_corr, na.rm = TRUE)
  best_k[i] <- which.min(class_corr)
}

mean(class_corr_av)
mean(best_k)

###using optimal k###
set.seed(1) #set seed bc knn() breaks ties using random numbers

attempts <- c(1:100) #number of times to run k
class_corr <- rep(0, max(n_k))

for (i in attempts) {
  random_order <- sample(c(1:nrow(oscars)), nrow(oscars), replace = FALSE) #shuffles row numbers
  test <- random_order[1:(nrow(oscars)/5)] #picks first 1/5 of rows
  train <- random_order[(nrow(oscars)/5):nrow(oscars)]
  
  train_X <- cbind(oscars$n.x, oscars$GG, oscars$Guild, oscars$BAFTA)[train, ] #predictors
  test_X <- cbind(oscars$n.x, oscars$GG, oscars$Guild, oscars$BAFTA)[test, ]
  train_winner <- oscars$winner[train]
  
  knn_pred <- knn(train_X, test_X, train_winner, k = 3.25)
  table(knn_pred, oscars$winner[test])
  class_corr[i] <- 1 - mean(knn_pred == oscars$winner[test]) #error rate]
  
}

mean(class_corr)

###THIS YEAR###
#actors sheet is short excel file w name, GG, Guild wins for the nominees
oscars_Sheet1 <- read_csv("C:/Users/ia767/Downloads/oscars - Sheet1.csv") %>% filter(!is.na(Guild))
oscars_Sheet1 <- oscars_Sheet1[ , c(1:7)]

train_X <- cbind(oscars$BAFTA, oscars$GG, oscars$Guild, oscars$n.x) #predictors
train_winner <- oscars$winner

oscar_preds_2019 <- knn(train_X, oscars_Sheet1[ , c(3:5, 7)], train_winner, k = 3.25)
names(oscar_preds_2019) <- oscars_Sheet1$name #attaching name to predictions
oscar_preds_2019

