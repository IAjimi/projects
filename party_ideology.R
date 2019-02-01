## POLITICAL IDEOLOGY: SENATE 116####
load("C:/Users/ia767/Downloads/senate_116.RData")

library("tidyverse")
library("ggalt")
library("ggrepel")

## Quick Function ####
nice <- function(x) {
  x <- sapply(str_split(x, ","), `[`, 1) #takes first element of name, before comma
  x <- str_to_title(x) #changes lastname to title form
  return(x) #returns name
}

## Data Set Work ####
#Coding Political Parties Differently
senate_116$party[senate_116$party_code == 100] <- "D"
senate_116$party[senate_116$party_code == 200] <- "R"
senate_116$party[!(senate_116$party_code %in% c(100, 200))] <- "Ind"

#Adding Political Party Colors
party_colors <- c("#2E74C0", "darkgrey", "#CB454A")

#Adding Democratic Presidential Candidates
pres_candidates_short <- c("HARRIS, Kamala Devi", "WARREN, Elizabeth",
                           "BOOKER, Cory Anthony", "GILLIBRAND, Kirsten", "SANDERS, Bernard")

### Graphs ####
#Graph 1: Ideological Split Across Party Lines
## more basic
ggplot(senate_116, aes(nominate_dim1, nominate_dim2, color = party)) + 
  geom_point(size = 2) +
  scale_color_manual(values = party_colors) +
  labs(title = "Strong Ideological Differences Across Party Lines",
       subtitle = "Democrats & Republicans Divided Over Economic Issues",
       x = "Economically Left-Right",
       y = "Socially Liberal-Conservative",
       color = "Party") 


## highlighting Democratic Presidential Candidates
ggplot(senate_116, aes(nominate_dim1, nominate_dim2, label = nice(bioname))) + 
  geom_point(aes(color = party), size = 2) +
  geom_point(data = subset(senate_116, bioname %in% pres_candidates_short),
             size = 2, color = "#1f4f82") +
  geom_text_repel(data = subset(senate_116, bioname %in% pres_candidates_short),
                  color = "#1f4f82") +
  guides(color = FALSE) +
  scale_color_manual(values = party_colors) +
  labs(title = "Strong Ideological Differences Across Party Lines",
       subtitle = "Current Democratic Presidential Candidates Particularly Left-Leaning Economically",
       x = "Economically Left-Right",
       y = "Socially Liberal-Conservative",
       color = "Party")

#Graph 3a,b: Ideological Split Across Party Lines, by Senator Birth Year
## Dimension 1
ggplot(senate_116, aes(born, nominate_dim1)) + 
  geom_point(aes(color = party), size = 2) +
  geom_point(data = subset(senate_116, bioname %in% pres_candidates_short),
             size = 2, color = "#1f4f82") +
  geom_text_repel(data = subset(senate_116, bioname %in% pres_candidates_short),
                  color = "#1f4f82", aes(label = nice(bioname))) +
  scale_color_manual(values = party_colors) + 
  labs(title = "Strong Ideological Differences Across Party Lines",
       subtitle = "Economic Policy Divide Persists Across Age Cohorts",
       x = "Birth Year",
       y = "Economically Left-Right") + 
  guides(color = FALSE)

## Dimension 2
ggplot(senate_116, aes(born, nominate_dim2)) + 
  geom_point(aes(color = party), size = 2) +
  geom_point(data = subset(senate_116, bioname %in% pres_candidates_short),
             size = 2, color = "#1f4f82") +
  geom_text_repel(data = subset(senate_116, bioname %in% pres_candidates_short),
                  color = "#1f4f82", aes(label = nice(bioname))) +
  scale_color_manual(values = party_colors) +
  labs(title = "Strong Ideological Differences Across Party Lines",
       subtitle = "Pattern Less Clear For Social Views",
       x = "Birth Year",
       y = "Socially Liberal-Conservative") + 
  guides(color = FALSE)