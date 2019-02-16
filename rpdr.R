## Prediction RPDR Winner
#Loading Libraries
library(readr)
library(tidyverse)

#Loading data
all_episodes <- read_csv("C:/Users/ia767/Downloads/RPDR Data Tables - all_episodes.csv")
all_contestants <- read_csv("C:/Users/ia767/Downloads/RPDR Data Tables - all_contestants.csv")
all_rankings <- read_csv("C:/Users/ia767/Downloads/RPDR Data Tables - all_rankings.csv")

#trimming df
all_contestants <- all_contestants %>% 
  select(-contestant_entrance, -contestant_name, -handle_instagram, -handle_twitter)

#filtering episode outcomes
## adding constraint of episode number 6 being the max number we draw data from
ep_result <- all_rankings %>% filter(episode_placement %in% c("SAFE", "HIGH", 'WIN') 
                                     #& episode_number <= 6
                                     ) %>%
  group_by(contestant_id) %>% count(episode_placement)

ep_result <- spread(ep_result, episode_placement, n)

ep_result$HIGH[is.na(ep_result$HIGH)] <- 0
ep_result$SAFE[is.na(ep_result$SAFE)] <- 0
ep_result$WIN[is.na(ep_result$WIN)] <- 0

#merging contestant info
ex <- left_join(all_contestants, ep_result, by = "contestant_id")

### Regressions
## LM 
reg <- lm(season_outcome ~  season_number + SAFE + HIGH, data = ex)
summary(reg)
#problem: SAFE HIGH count increases naturally w number appearances

errors <- ex$season_outcome[ex$contestant_id %in% names(predict(reg, data = ex))] - 
          predict(reg, data = ex)
mse <- mean( errors^2 , na.rm = TRUE)

mse

plot(errors)

## Trying out Binary Model
ex$winner <- 0
ex$winner[ex$season_outcome <= 2] <- 1 #top two finalists

myprobit <- glm(winner ~  season_number + SAFE + HIGH, 
                family = binomial(link = "probit"), data = ex)
summary(myprobit)

prediction <- predict(myprobit, data = ex)
threshold <- quantile(predict(myprobit, data = ex), 0.45)
prediction[prediction <= threshold] <- 0
prediction[prediction != 0] <- 1

tables <- table(prediction,
                ex$winner[ex$contestant_id %in% names(predict(reg, data = ex))])

1 - sum(diag(tables)) / sum(tables)#classification error 

winner_match <- tables[2, 2] / sum(tables)
winner_match #correct winner prediction
