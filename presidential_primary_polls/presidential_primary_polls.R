library(readr)
library(tidyverse)

president_primary_polls <- read_csv("C:/Users/ia767/Downloads/president_primary_polls.csv")
president_primary_polls$start_date <- as.Date(president_primary_polls$start_date, "%m/%d/%y")

### getting average percentage for democrat candidates from reliable polls
#february
dem_feb <- president_primary_polls %>% filter(fte_grade %in% c("A-", "A", "A+") & party == "DEM") %>% 
  filter(start_date <= "2019-02-05") %>%
  group_by(candidate_name) %>%
  summarize(pct = mean(pct)) %>%
  arrange(desc(pct))

#may
dem_may <- president_primary_polls %>% filter(fte_grade %in% c("A-", "A", "A+") & party == "DEM") %>% 
  filter(start_date >= "2019-05-05") %>%
  group_by(candidate_name) %>%
  summarize(pct = mean(pct)) %>%
  arrange(desc(pct))

### selecting key candidates
feb_5 <- dem_feb[c(1:5), ] #top 5 in feb
may_5 <- dem_may[c(1:5), ] #top 5 in may
key_candidates <- full_join(feb_5, may_5) #union of both
key_candidates <- unique(key_candidates$candidate_name)


### joining data frames
dem_feb <- mutate(dem_feb, date = "2019-02-05")
dem_may <- mutate(dem_may, date = "2019-05-05")
dem_feb$date <- as.Date(dem_feb$date)
dem_may$date <- as.Date(dem_may$date)

full_frame <- rbind(dem_feb, dem_may)
full_frame <- filter(full_frame, candidate_name %in% key_candidates)

### first plot: points
ggplot(full_frame, aes(date, pct/100, color = reorder(candidate_name, -pct))) + 
  geom_point(size = 2.5) +
  geom_line(alpha = 0.4) +
  geom_text_repel(data = filter(full_frame, date == "2019-05-05" ),
    label = c("Biden", "Sanders", "Warren", "Harris", "Buttigieg", "O'Rourke")) +
  scale_y_continuous(labels=scales::percent) +
  xlim(as.Date(c("2019-01-23", "2019-05-15"))) +
  guides(color = FALSE) +
  labs(title = "Democratic Primary Poll Numbers",
       subtitle = "Pollsters Rated A- And Higher, February and May 2019",
       x = "", y = "Positive Opinions", 
       caption = "Source: FiveThirtyEight")


### adding donald trump
don_feb <- president_primary_polls %>% filter(fte_grade %in% c("A-", "A", "A+") & 
                                                candidate_name == "Donald Trump") %>% 
  filter(start_date <= "2019-02-05") %>%
  group_by(candidate_name) %>%
  summarize(pct = mean(pct)) 

don_apr <- president_primary_polls %>% filter(fte_grade %in% c("A-", "A", "A+") & 
                                                candidate_name == "Donald Trump") %>% 
  filter(start_date >= "2019-04-15") %>%
  group_by(candidate_name) %>%
  summarize(pct = mean(pct)) 

don_feb <- mutate(don_feb, date = "2019-02-05")
don_apr <- mutate(don_apr, date = "2019-04-15")
don_feb$date <- as.Date(don_feb$date)
don_apr$date <- as.Date(don_apr$date)

full_frame <- rbind(full_frame, don_feb)
full_frame <- rbind(full_frame, don_apr)

## new plot
ggplot(full_frame, aes(date, pct/100)) + 
  geom_point(data = filter(full_frame, candidate_name != "Donald Trump"), 
             size = 2.5, color = "#2E74C0") +
  geom_point(data = filter(full_frame, candidate_name == "Donald Trump"), 
             size = 2.5, color = "#CB454A") +
  geom_line(data = filter(full_frame, candidate_name != "Donald Trump"),
            aes(group = candidate_name), alpha = 0.4, color = "#2E74C0") +
  geom_line(data = filter(full_frame, candidate_name == "Donald Trump"),
            aes(group = candidate_name), alpha = 0.4, color = "#CB454A") +
  geom_text_repel(data = filter(full_frame, date == "2019-05-05" ),
                  label = c("Biden", "Sanders", "Warren", "Harris", "Buttigieg", "O'Rourke"),
                  color = "#2E74C0") +
  geom_text_repel(data = filter(full_frame, date == "2019-04-15" ),
                  label = c("Trump"), color = "#CB454A") +
  scale_y_continuous(labels=scales::percent) +
  xlim(as.Date(c("2019-01-23", "2019-05-15"))) +
  guides(color = FALSE) +
  labs(title = "2020 Presidential Election Polls",
       subtitle = "Pollsters Rated A- And Higher, February and May 2019",
       x = "", y = "Positive Opinions", 
       caption = "Source: FiveThirtyEight")

