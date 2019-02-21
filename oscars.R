library(readr)
library(tidyverse)
library(sandwich)
library(lmtest)

####Data####
#https://github.com/scruwys/and-the-award-goes-to/blob/master/data/nominations.csv
nominations <- read_csv("C:/Users/ia767/Downloads/nominations.csv")  %>% 
  select(-href) %>%
  filter(category == "Actor")

#adding df
films <- read_csv("C:/Users/ia767/Downloads/films.csv") %>% 
  select(bom_domestic, film, imdb_score, rt_critic_score)
#problem with country: international countries fare too poorly overall
#problem with imdb score: Could be determined after the fact, biased (high score BC won oscar)
#rt_critic_score & bom_domestic: insign (see code below)

#cleaning film df
films$bom_domestic <- substring(films$bom_domestic, 2) #gets rid of first character ($)
films$bom_domestic <- str_replace_all(films$bom_domestic, ",", "") #gets rid of commas
films$bom_domestic <- as.numeric(films$bom_domestic)

#splitting df to change pivot some columns
oscars <- nominations %>% filter(award == "Oscar")
others <- nominations %>% filter(award != "Oscar") %>%
  spread(award, winner, 0) #changes df so each award is a column, 0 used instead of NA
#could use -1 : penalty for not being nominated

#merging df
oscars <- left_join(oscars, others, by = c("category", "film", "name", "year"))
names(oscars)[names(oscars) == 'Golden Globe'] <- "GG" #renaming golden globes

oscars <- left_join(oscars, films, by ="film")

### Probit ####
##Implementing CV
###goal: find overall prediction error + best prediction cutoff
attempts <- c(1:50)
best_seq <- rep(0, max(attempts))
class_error_av <- rep(0, max(attempts))
class_error <- list(c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10),
                    c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10),
                    c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10),
                    c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10),
                    c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10), c(1:10))

for (i in attempts) {
  random_order <- sample(c(1:nrow(oscars)), nrow(oscars), replace = FALSE) #shuffles row numbers
  random <- random_order[1:(nrow(oscars)/5)] #picks first 1/5 of rows
  not_random <- random_order[(nrow(oscars)/5):nrow(oscars)]
  
  myprobit <- glm(winner ~ year + GG + Guild + bom_domestic, 
                family = binomial(link = "probit"), data = oscars[not_random, ])
  coeftest(myprobit, vcov = vcovHC(myprobit, type="HC1")) #using robust s.e.
  
  #predictions
  yhat_probit <- predict(myprobit, newdata = oscars[random, ], type = "response")
  
  for (j in c(1:10)){

    probit_predicts <- rep(0, length(yhat_probit))
    probit_predicts[yhat_probit > seq(0.5, 0.95, 0.05)[j]] <- 1
    class_error[[i]][j] <- 1 - sum(diag(table(probit_predicts, oscars$winner[random]))) / length(oscars$winner[random])
    #classification error

    }
  
  best_seq[i] <- which.min(class_error[[i]]) #finds best cutoff for prediction
  class_error_av[i] <- mean(class_error[[i]])
}

mean(class_error_av) #mean class_error for each iteration
mean(best_seq) #mean prediction cutoff that min class error

##Generally 0.55 performs best

###New Probit Loop ####
##just cv, using 0.55 as cutoff
attempts <- c(1:100)
new_class_error <- rep(0, max(attempts))
bom_sign <- rep(0, max(attempts))

for (i in attempts) {
  random_order <- sample(c(1:nrow(oscars)), nrow(oscars), replace = FALSE) #shuffles row numbers
  random <- random_order[1:(nrow(oscars)/5)] #picks first 1/5 of rows
  not_random <- random_order[(nrow(oscars)/5):nrow(oscars)]
  
  myprobit <- glm(winner ~ year + GG + Guild + bom_domestic, 
                  family = binomial(link = "probit"), data = oscars[not_random, ])
  coeftest(myprobit, vcov = vcovHC(myprobit, type="HC1")) #using robust s.e.
  bom_sign[i] <- coeftest(myprobit, vcov = vcovHC(myprobit, type="HC1"))[5,4] #sign level of bom_dom
  
  #predictions
  yhat_probit <- predict(myprobit, newdata = oscars[random, ], type = "response")
  
  #class error
  probit_predicts <- rep(0, length(yhat_probit))
  probit_predicts[yhat_probit > 0.55] <- 1
  new_class_error[i] <- 1 - sum(diag(table(probit_predicts, oscars$winner[random]))) / length(oscars$winner[random])
}

mean(new_class_error) #mean class_error for each iteration
mean(bom_sign) #mean dom -- generally insignificant

### Final Model (for now) ####
oscar_preds <- glm(winner ~ year + GG + Guild, family = binomial(link = "probit"), data = oscars)
coeftest(myprobit, vcov = vcovHC(myprobit, type="HC1")) #using robust s.e.

#predictions
oscar_preds_yhat <- predict(oscar_preds, newdata = oscars, type = "response")
oscars$preds <- oscar_preds_yhat
oscars$error <- oscars$winner - oscars$preds

oscars %>% select(-award, -category, -bom_domestic, -imdb_score) %>% filter(year >= 2014)

oscars %>% select(-award, -category, -bom_domestic, -imdb_score) %>% arrange(error) 

probit_predicts <- rep(0, length(yhat_probit))
probit_predicts[yhat_probit > 0.55] <- 1
1 - sum(diag(table(probit_predicts, oscars$winner[random]))) / length(oscars$winner[random])


