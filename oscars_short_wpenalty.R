library(readr)
library(tidyverse)
library(sandwich)
library(lmtest)

####Data####
#https://github.com/scruwys/and-the-award-goes-to/blob/master/data/nominations.csv
nominations <- read_csv("C:/Users/ia767/Downloads/nominations.csv")  %>% 
  select(-href) %>%
  filter(category == "Actor")

#splitting df to change pivot some columns
oscars <- nominations %>% filter(award == "Oscar")
others <- nominations %>% filter(award != "Oscar") %>%
  spread(award, winner, -1) #changes df so each award is a column, -1 used instead of NA

#merging df
oscars <- left_join(oscars, others, by = c("category", "film", "name", "year"))
names(oscars)[names(oscars) == 'Golden Globe'] <- "GG" #renaming golden globes

#implementing -1 penalty
oscars$BAFTA[is.na(oscars$BAFTA)] <- -1
oscars$GG[is.na(oscars$GG)] <- -1
oscars$Guild[is.na(oscars$Guild)] <- -1


### Probit ####
##just cv, using 0.55 as cutoff
attempts <- c(1:100)
new_class_error <- rep(0, max(attempts))

for (i in attempts) {
  random_order <- sample(c(1:nrow(oscars)), nrow(oscars), replace = FALSE) #shuffles row numbers
  random <- random_order[1:(nrow(oscars)/5)] #picks first 1/5 of rows
  not_random <- random_order[(nrow(oscars)/5):nrow(oscars)]
  
  myprobit <- glm(winner ~ year + GG + Guild, 
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
oscar_preds <- glm(winner ~ year + GG + Guild, family = binomial(link = "probit"), data = oscars)
coeftest(myprobit, vcov = vcovHC(myprobit, type="HC1")) #using robust s.e.

#predictions
oscar_preds_yhat <- predict(oscar_preds, newdata = oscars, type = "response")
oscars$preds <- oscar_preds_yhat
oscars$error <- oscars$winner - oscars$preds

#looking at how predictions fare recently
oscars %>% select(-award, -category) %>% filter(year >= 2014)

###THIS YEAR###
#actors sheet is short excel file w name, GG, Guild wins for the nominees
oscar_preds_2019 <- predict(oscar_preds, newdata = actors_Sheet1, type = "response")
names(oscar_preds_2019) <- actors_Sheet1$name #attaching name to predictions
oscar_preds_2019

#graphing predictions
actors_Sheet1$preds <- oscar_preds_2019 #add preds to df
actors_Sheet1$name <- str_to_title(actors_Sheet1$name) #make names nicer

ggplot(actors_Sheet1, aes(reorder(name, preds), preds)) + 
  geom_col(position = "dodge", color = "grey70") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()  + 
  labs(y = "Probability of Winning", x = "",
       title = "Rami Malek in the Lead for Best Actor", 
       subtitle = "Following Golden Globes & Actors' Guild Wins")
