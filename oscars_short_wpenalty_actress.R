library(readr)
library(tidyverse)
library(sandwich)
library(lmtest)

####Data####
#https://github.com/scruwys/and-the-award-goes-to/blob/master/data/nominations.csv
nominations <- read_csv("C:/Users/ia767/Downloads/nominations.csv")  %>% 
  select(-href)

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
oscars$BAFTA[is.na(oscars$BAFTA)] <- 0
oscars$GG[is.na(oscars$GG)] <- 0
oscars$Guild[is.na(oscars$Guild)] <- 0 #choice! tech. SAG only started in 1995


### Probit ####
##just cv, using 0.55 as cutoff
attempts <- c(1:500)
new_class_error <- rep(0, max(attempts))

for (i in attempts) {
  random_order <- sample(c(1:nrow(oscars)), nrow(oscars), replace = FALSE) #shuffles row numbers
  random <- random_order[1:(nrow(oscars)/5)] #picks first 1/5 of rows
  not_random <- random_order[(nrow(oscars)/5):nrow(oscars)]
  
  myprobit <- glm(winner ~ year + GG + Guild + BAFTA + n.x, 
                  family = binomial(link = "probit"), data = oscars[not_random, ])
  coeftest(myprobit, vcov = vcovHC(myprobit, type="HC1")) #using robust s.e.
  
  #predictions
  yhat_probit <- predict(myprobit, newdata = oscars[random, ], type = "response")
  
  #class error
  probit_predicts <- rep(0, length(yhat_probit))
  probit_predicts[yhat_probit > 0.55] <- 1
  new_class_error[i] <- 1 - sum(diag(table(probit_predicts, oscars$winner[random]))) / length(oscars$winner[random])
}

mean(new_class_error) #mean class_error for each iteration

### Final Model (for now) ####
oscar_preds <- glm(winner ~ year + GG + Guild + n.x + BAFTA, family = binomial(link = "probit"), data = oscars)
coeftest(oscar_preds, vcov = vcovHC(myprobit, type="HC1")) #using robust s.e.

#predictions
oscar_preds_yhat <- predict(oscar_preds, newdata = oscars, type = "response")
oscars$preds <- oscar_preds_yhat
oscars$error <- oscars$winner - oscars$preds

#looking at how predictions fare recently
oscars %>% select(-award, -category) %>% filter(year >= 2014)

#residuals plot
oscars %>% filter(winner == 1) %>% ggplot(aes(year, error)) + geom_point()
##worse predictions before 1995 bc SAG awards weren't given out then

###THIS YEAR###
#actors sheet is short excel file w name, GG, Guild wins for the nominees
oscars_Sheet1 <- read_csv("C:/Users/ia767/Downloads/oscars - Sheet1.csv")
oscar_preds_2019 <- predict(oscar_preds, newdata = oscars_Sheet1, type = "response")
names(oscar_preds_2019) <- oscars_Sheet1$name #attaching name to predictions
oscar_preds_2019

#graphing predictions
oscars_Sheet1$preds <- oscar_preds_2019 #add preds to df
oscars_Sheet1$name <- str_to_title(oscars_Sheet1$name) #make names nicer

oscars_Sheet1 %>% filter(category == "Actress") %>%
  ggplot(aes(reorder(name, preds), preds)) + 
  geom_col(position = "dodge", color = "grey70") +
  #scale_y_continuous(labels = scales::percent) +
  ylim(c(0:1)) +
  coord_flip()  + 
  labs(y = "Probability of Winning", x = "",
       title = "Olivia Colman Favorite", 
       subtitle = "Glenn Close & Lady Gaga Neck to Neck")

#notes
##bafta improves the fit of the model here, which isnt true for best supp actress, best actor, ...
## a lot of unobservable variables, related to 'oscar politics' (who is liked in the industry,
## who's oscar is still due, etc..)
