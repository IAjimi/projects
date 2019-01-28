#### FREDDIE MAC HOUSE PRICE INDEX ####
library(readxl)
library(tidyverse)
library(ggrepel)

housing <- read_excel("C:/Users/ia767/Downloads/State_and_US_SA.xls", skip = 5)
#some useless extra lines due to excel format

# Putting 'Month' Column into Data format
## initially formated 1990M01 
housing$Month <- str_replace(housing$Month, "M", "/")
housing$Month <- paste(housing$Month,"/01",sep="") #adding day for transition to date format
housing$Month <- as.Date(housing$Month, "%Y/%m/%d")

# Renaming Columns
housing <- dplyr::rename(housing, US = `United States seasonally adjusted`,
                         Date = Month)

# Changing DataFrame Shape from Wide to Long
housing <- gather(housing, State, Price, -Date)

# Putting Price as numeric
housing$Price <- as.numeric(housing$Price)

# Plot
ggplot(housing, aes(Date, Price, label = State)) + 
  geom_line(aes(group = State), color = "grey") +
  geom_line(data = subset(housing, State == "US"), color = "red", size = 1.25) +
  geom_text_repel(data = subset(housing, State == "US" & Date == "2018-10-01"), color = "red") +
  labs(x = "", y = "Price Index", title = "Housing Prices per State, 1975 - 2018",
       caption = "Index set at 100 on Dec 2000")

# Plot 2
#housing %>% filter(Date == "2018-11-01") %>% arrange(desc(Price))
key_states <- c("DC", "HI", "CA", "MI", "OH", "IL")
#states w highest and lowest house prices

ggplot(subset(housing, Date >= "2000-12-01"), aes(Date, Price, label = State)) + 
  geom_line(aes(group = State), color = "grey") +
  geom_line(data = subset(housing, State == "US" & Date >= "2000-12-01"), 
            color = "red", size = 1.25) +
  geom_text_repel(data = subset(housing, State == "US" & Date == "2018-11-01"), color = "red") +
  geom_text_repel(data = subset(housing, State %in% key_states & Date == "2018-11-01"), 
                  color = "grey") +
  labs(x = "", y = "Price Index", title = "Housing Prices per State, 2000-2018",
       caption = "Index set at 100 on Dec 2000")

# Alternate
ggplot(subset(housing, Date >= "2000-12-01"), aes(Date, Price, label = State)) + 
  geom_line(aes(group = State), color = "grey") +
  geom_line(data = subset(housing, State %in% key_states & Date >= "2000-12-01"), 
            aes(group = State), color = "orange", size = 1) +
  geom_text_repel(data = subset(housing, State %in% key_states & Date == "2018-11-01"), 
                  color = "orange") +
  labs(x = "", y = "Price Index", title = "Housing Prices per State, 2000-2018",
       caption = "Index set at 100 on Dec 2000")

# Smallest Crisis Related Shock
housing %>% filter(Date >= "2007-01-01" & Date <= "2009-01-01") %>% 
  group_by(State) %>% 
  summarize(variance = sd(Price)) %>% arrange(variance)

stable_states <- c("IA", "TX", "OK", "KY")

ggplot(subset(housing, Date >= "2000-12-01"), aes(Date, Price, label = State)) + 
  geom_line(aes(group = State), color = "grey") +
  geom_line(data = subset(housing, State %in% stable_states & Date >= "2000-12-01"), 
            aes(group = State), color = "red", size = 1) +
  geom_text_repel(data = subset(housing, State %in% stable_states & Date == "2018-11-01"), 
                  color = "red") +
  labs(x = "", y = "Price Index", title = "Housing Prices per State, 2000-2018",
       subtitle = "States Least Affected by 2008 Crash",
       caption = "Index set at 100 on Dec 2000")
