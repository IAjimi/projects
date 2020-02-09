### Loading Libraries ####
library(xml2)
library(httr)
library(rvest)
library(tidyverse)

#### Scraping Links ####
## Need to scrape 2016 Onwards
oscar_links <- c(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actor" = "Actor" ,
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actress" = "Actress",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Supporting_Actor" = "Supporting Actor",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Supporting_Actress"= "Supporting Actress",
  'https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Direction' = "Director",
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Actor" = "Actor",
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Actress" = "Actress",
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Supporting_Actor" = "Supporting Actor",
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Actress_in_a_Supporting_Role" = "Supporting Actress",
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Adapted_Screenplay" = "Adapted Screenplay",
  "https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Original_Screenplay" = "Original Screenplay",
  'https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Motion_Picture_%E2%80%93_Drama' = "Best Picture",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actor_%E2%80%93_Motion_Picture_Drama" = "Best Picture",
  'https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Director' = "Director",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actor_%E2%80%93_Motion_Picture_Drama" = "Actor",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actor_%E2%80%93_Motion_Picture_Musical_or_Comedy" = "Actor - Comedy",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actress_%E2%80%93_Motion_Picture_Comedy_or_Musical" = "Actress",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Actress_in_a_Motion_Picture_%E2%80%93_Drama" = "Actress - Drama",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Supporting_Actress_%E2%80%93_Motion_Picture" = "Supporting Actress",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Screenplay" = "Adapted Screenplay",
  "https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Screenplay" = "Original Screenplay" ,
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Male_Actor_in_a_Leading_Role" = "Actor",
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Female_Actor_in_a_Leading_Role" = "Actress",
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Male_Actor_in_a_Supporting_Role" = "Supporting Actor",
  "https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Female_Actor_in_a_Supporting_Role" = "Supporting Actress"
)

### Map Get Request ####
get_nominees <- function(award_, link_){
  
  award_name <- award_
  url <- link_
  wiki_resp <- read_html(url)
  
  if (str_detect(url, "Academy_Award") == TRUE){
    num_ <- 2
  } else if (str_detect(url, "BAFTA") == TRUE){
    num_ <- c("Actor" = 8, "Actress" = 7, "Supporting Actor" = 6, "Supporting Actress" = 6,
              "Director" = 6, 
              "Adapted Screenplay" = 4, "Original Screenplay" = 4)[award_name]
  } else if (str_detect(url, "Golden_Globe") == TRUE){
    num_ <- c(
      "Actress" = 7,   "Actress - Drama" = 8, "Supporting Actress" = 8,
      "Director" = 8,  "Best Picture" = 8, "Actor" = 8, "Actor - Comedy" = 7,
      "Adapted Screenplay" = 8, "Original Screenplay" = 8)[award_name]  
  } else {
    num_ <- c("Actress" = 3,   "Actor" = 3, "Supporting Actor" = 3, "Supporting Actress" = 3    )[award_name]
  }
  
  
  wiki_text <- html_table(html_nodes(wiki_resp, ".wikitable"), fill = TRUE)[[num_]] %>%  as.data.frame() 
  
  if (str_detect(url, "Academy_Awards") == FALSE & str_detect(url, "Screenplay") == TRUE){
    num_ <- 3
  } else{
    num_ <- 2
  }
  
  if (str_detect(url, "Academy_Awards") == TRUE){
    nominees <- wiki_text %>% 
      select(year = Year, name = names(wiki_text)[2], film = Film) %>% #positional trick so isn't sensitive to actor/actress
      mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>%
      filter(year > 2016 & year <= 2019)
  } else{
    if (str_detect(url, "Screenplay|Picture") == TRUE & str_detect(url, "(A|a)ctr.") == FALSE){
      num_ <- 3
    } else{
      num_ <- 2
    }
    nominees <- wiki_text %>% 
      select(year = Year, name = names(wiki_text)[num_], film = Film) %>% #positional trick so isn't sensitive to actor/actress
      mutate(year = as.numeric(str_extract(year, "^...."))) %>%
      filter(!is.na(name)) %>%
      filter(year >= 2016 & year <= 2019)
  }
  
  winners <- nominees %>%
    split(.$year) %>%
    map(select, name) %>%
    map(head, 1) %>%
    unlist() %>%
    unname()
  
  nominees <- nominees %>%
    mutate(winner = if_else(name %in% winners, 1, 0),
           film = str_replace_all(film, "\\*|???|???", ""),
           film = str_replace_all(film, "(  )|( $)", ""),
           name = str_replace_all(name, "\\*|???|???", ""),
           name = str_replace_all(name, "(  )|( $)", ""),
           category = award_name,
           award = str_replace(url, "https://en.wikipedia.org/wiki/", ""),
           award = str_split(award, "_") %>% map(1)
    )
  
  return(nominees)
}

all_nominees <- map2(as.list(oscar_links), as.list(names(oscar_links)), get_nominees) %>%
  do.call("rbind.data.frame", .)

rownames(all_nominees) <- c()


### Dealing With More Complicated Links ####

#"Director" = "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Director", NOPE

  failed_urls <- c("https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Supporting_Actor_%E2%80%93_Motion_Picture", 
                   'https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Picture',
                   "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture",
                   "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Adapted_Screenplay",
                   "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Original_Screenplay")
  
  get_failed_links <- function(url){
    
    wiki_resp <- read_html(url)
    
    if (str_detect(url, "Golden|BAFTA") == TRUE){
      raw_input <- html_nodes(wiki_resp, ".wikitable")[8]
    } else if (str_detect(url, "Academy_Award") == TRUE & str_detect(url, "Original") == FALSE){
      raw_input <- html_nodes(wiki_resp, ".wikitable")[10]
    } else{
      raw_input <- html_nodes(wiki_resp, ".wikitable")[8]
    }
    
    raw_input <- raw_input %>% 
      html_nodes("tr") %>% 
      html_nodes("td") %>% 
      html_text() %>%
      str_replace_all("\n{1,}", " ") %>%
      str_replace_all("\\(.{1,}\\)", "") %>% #removing things between (parenthesis)
      str_replace_all("( $)|( {2,})", "") %>% #replacing extra spaces
      str_replace_all("(\\[)|(\\])", "") %>% #removing []
      str_replace_all(" [0-9]$", "")#replacing footnotes
    
    # Save as DF
    raw_input <- as.data.frame(raw_input, stringsAsFactors = FALSE)
    names(raw_input) <- c("name")
    
    # Add year As Separate Column
    if (str_detect(url, "Academy_Award_for_Best_Original_Screenplay") == TRUE){
      year_row <- 1 + 10 * c(1:(nrow(raw_input) / 10) - 1)
      year_ <- 2010 + c(1:(nrow(raw_input) / 10) - 1) 
      raw_input$year <- NA
      raw_input$year[year_row] <- year_
    } else{
      raw_input$year <- as.numeric(raw_input$name)
      raw_input$year[raw_input$year == 1917] <- NA
    }
    raw_input <- raw_input %>% fill(year) %>% filter(year != name) %>% filter(year >= 2016) %>% filter(name != "Best Film")
    
  # Remove Character Names

  if ( str_detect(url, "Golden_Globe") == TRUE & str_detect(url, "Supporting") == TRUE ){
    character_name <- 2 + 3 * c(0:nrow(raw_input)) #every third row 
    raw_input <- raw_input[-c(character_name), ]
    rm(character_name)
  } else if ( str_detect(url, "BAFTA") == TRUE){
    character_name <- 3 + 4 * c(0:nrow(raw_input)) #every fourth row 
    raw_input <- raw_input[-c(character_name), ]
    rm(character_name)
  } else if  ( str_detect(url, "Academy_Award") == TRUE & str_detect(url, "Adapted_Screenplay") == TRUE ){
    character_name <-  3 * c(1:(nrow(raw_input) / 3)) #removing name of OG script
    raw_input <- raw_input[-c(character_name), ]
    rm(character_name)
  }
  
  rownames(raw_input) <- c() #reset rows
  
  # Add Movie Names
  raw_input$film <- NA
  
  if (str_detect(url, "Golden_Globe") == TRUE & str_detect(url, "Supporting")){
    film_name <- 2 * c(1:(nrow(raw_input)/2))
    raw_input$film[film_name - 1] <- raw_input$name[film_name]
  } else if  ( str_detect(url, "BAFTA") == TRUE) {
    film_name <- 1 + 3 * c(0:(nrow(raw_input)/3 - 1))
    raw_input$film[film_name] <- raw_input$name[film_name]
  } else if (str_detect(url, "Academy_Award") == TRUE & ( str_detect(url, "Best_Picture") == TRUE | str_detect(url, "Screenplay") == TRUE ) ) {
    film_name <- 1 + 2 * c(0:(nrow(raw_input)/2 - 1)) #every two rows starting with first row
    raw_input$film[film_name] <- raw_input$name[film_name]
  } else{
    film_name <- 2 * c(1:(nrow(raw_input)/2 ))
    raw_input$film[film_name - 1] <- raw_input$name[film_name]
  }
  
  nominees <- raw_input %>% fill(film) %>% filter(film != name)
  
  ## Removing Country
  if (str_detect(url, "BAFTA_Award_for_Best_Picture") == TRUE){
    country_name <- 2 * c(0:(nrow(raw_input)/2))
    nominees <- nominees[-country_name, ]
  }
  
  
  ## Adding Winners
  winners <- nominees %>%
    split(.$year) %>%
    map(select, name) %>%
    map(head, 1) %>%
    unlist() %>%
    unname()
  
  nominees <- nominees %>%
    mutate(winner = if_else(name %in% winners, 1, 0),
           film = str_replace_all(film, "\\*|???|???", ""),
           film = str_replace_all(film, "(  )|( $)", ""),
           name = str_replace_all(name, "\\*|???|???", ""),
           name = str_replace_all(name, "(  )|( $)", ""),
           category = url %>% str_split("for_") %>% map(2),
           category = category %>% str_split("%") %>% map(1),
           category = str_replace_all(category, "_", " "),
           award = str_replace(url, "https://en.wikipedia.org/wiki/", ""),
           award = str_split(award, "_") %>% map(1))
  
  return(nominees)
}


other_nominations <- map(as.list(failed_urls), get_failed_links) %>% do.call("rbind.data.frame", .)

### COMBINING THE NOMINATIONS ####
nominees <- rbind(all_nominees, other_nominations)

nominees <- nominees %>% mutate(
  winner = if_else(year == 2019 & award == "oscar", 0, winner),
  category = str_split(category, " -") %>% map(1),
  category = as.character(category),
  category = str_replace_all(category, '(Best )|\\"|( $)', ""),
  award = as.character(award)
  )  %>% 
  mutate(
    award = case_when(
      award == "Golden" ~ "GG",
      award == "Academy" ~ "oscar",
      T ~ award
    )
  ) %>%
  distinct() 

nominees %>% group_by(category) %>% count()


### RETRIEVING WIKI LINKS FOR FURTHER INFO ####
oscar_links <- c(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actor",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actress",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Supporting_Actor",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Supporting_Actress",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture", ##
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Adapted_Screenplay", ##
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Original_Screenplay" ##
)


### GETTING URLS FOR RECENT MOVIES ####

get_wiki_urls <- function(url){
  
  wiki_resp <- read_html(url)
  print(url)
  
  if (str_detect(url, "Best_Picture|Screenplay") == TRUE & str_detect(url, "Original") == FALSE){
    raw_input <- html_nodes(wiki_resp, ".wikitable")[10]
  } else if (str_detect(url, "(Actor)|(Actress)") == TRUE){
    raw_input <- html_nodes(wiki_resp, ".wikitable")[2]
  } else{
    raw_input <- html_nodes(wiki_resp, ".wikitable")[8]
  }
  
  raw_input <- raw_input %>% 
    html_nodes("tr") %>% 
    html_nodes("td") %>% html_nodes("a") %>% 
    html_attr("href")
  
  raw_input <- as.data.frame(raw_input, stringsAsFactors = FALSE)
  names(raw_input) <- c("link")
  
  raw_input <- raw_input %>%
    mutate(
      film = str_replace_all(link, "/wiki/", ""),
      film = str_replace_all(film, "_", " "),
      film = str_replace_all(film, "\\(.{1,}\\)", ""), #removes all things after ()
      film = str_replace_all(film, "( $)|(  )", "")
    )  
  
  return(raw_input)
}

#### CREATING DATASET ####
list_urls_scraping <- map(oscar_links, get_wiki_urls) %>% do.call("rbind.data.frame", .) %>% distinct()

## Missing URLS 
left_join(nominations, list_urls_scraping) %>% filter(year >= 2016) %>% nrow()
left_join(nominations, list_urls_scraping) %>% filter(year >= 2016) %>% filter(is.na(link)) %>% nrow()


infoless_movies <- left_join(nominees, list_urls_scraping) %>% 
  filter(year >= 2016) %>% 
  filter(!is.na(link)) %>%
  mutate(link = paste("https://en.wikipedia.org/", link, sep = "")) %>% 
  distinct(film, link)


extra_links <- infoless_movies %>% group_by(film) %>% count() %>% filter(n > 1)

correct_links <- infoless_movies %>% 
  filter(film %in% extra_links$film) %>%
  filter(str_detect(link, "film")) %>%
  mutate(year = str_extract(link, "\\(...._") %>% str_replace_all("\\(|_", "") %>% as.numeric()) %>%
  filter(is.na(year) | year >= 2016) %>%
  select(-year)

infoless_movies <- infoless_movies %>% 
  filter(!(film %in% extra_links$film)) %>%
  rbind(., correct_links)

infoless_movies <- filter(infoless_movies, !(film %in% c("Rachel Weisz", "Russell Bufalino", "Joe Pesci")))

### GETTING MOVIE INFO FROM WIKIPEDIA ####
get_movie_info <- function(url){
  
  wiki_resp <- read_html(url)
  raw_input <- html_table(html_nodes(wiki_resp, ".infobox"), fill = TRUE)
  raw_input <- raw_input[[1]]
  raw_input <- as.data.frame(raw_input, stringsAsFactors = FALSE)
  names(raw_input) <- c("key_", "val_")
  
  
  col_nams <- c("Written by", "Starring", "Release date", "Running time", "Country", "Budget", "Box office")
  
  raw_input <- raw_input %>%
    filter(key_ %in% col_nams)
  
  names_ <- unname(raw_input[ , 1])
  
  raw_input <- raw_input %>% t() %>% as.data.frame() 
  
  
  row.names(raw_input) <- c()
  names(raw_input) <- names_
  raw_input <- raw_input[2, ]
  
  
  raw_input <- raw_input %>%
    mutate(
      link = url,
      writers_count = if_else(any(str_detect(names(raw_input), "Written by")), 1 , 0),
      stars_count = 1 + str_count(Starring, "\n"),
      running_time = str_replace_all(`Running time`, "\\[.{1,}\\]", ""),
      running_time = str_replace(running_time, "minutes", ""),
      release_date = str_extract_all(`Release date`, "....-..-.."), #extracting dates
      release_date = str_split(release_date, ",") %>% map(2) %>% str_replace_all("\\)", ""), #cleaning dates, picking second
      release_date = str_extract_all(release_date, "-..-") %>% str_replace_all("-", "") %>% as.numeric(), #cleaning date, picking month
      Country = str_split(Country, "\n") %>% map(1) %>% as.character()
    ) %>%
    mutate_if(is.factor, str_replace_all, "\\[.{1,}\\]", "") %>% #replace all content within brackets
    mutate_if(is.character, str_replace_all, "\\(|\\)", "") %>% #remove all ()
    select(- `Release date`, - Starring, - `Running time`)  
  
  if(any(str_detect(names(raw_input), "Written") == TRUE)){
    raw_input <- raw_input %>% mutate(writers_count = writers_count + str_count(`Written by`, "\n")) %>% select(- `Written by`)
  }
  
  col_nams <- c("writers_count", "stars_count", "release_date", "running_time", "Country", "Budget", "Box office")
  
  for (i in col_nams){
    if (i %in% names(raw_input) == FALSE){
      raw_input[, i] <- NA
    }
  }
  
  return(raw_input)
}

oscars_new_info <- map(as.list(infoless_movies$link), get_movie_info) %>% do.call("rbind.data.frame", .)

oscars_new_info <- oscars_new_info %>%
  mutate(film = infoless_movies$film) 

mov1 <- nominees %>% 
  mutate(film = str_replace_all(film, "(^ )|( $)", "")) %>% 
  left_join(., oscars_new_info, by = "film") %>%
  filter(is.na(Country)) %>%
  distinct(film)
  #nrow()

nominees <- nominees %>% 
  mutate(film = str_replace_all(film, "(^ )|( $)", "")) %>% 
  left_join(., oscars_new_info, by = "film")

### CLEAN TO MERGE WITH OG DATASET ####
nominees %>% names()
nominees_trim <- select(nominees, winner, award, category, year, country = Country, release_date, running_time, stars_count, writers_count)

#splitting df to change pivot some columns
nominees_oscar <- nominees %>% filter(award == "oscar")
others <- nominees %>% filter(award != "oscar") %>% spread(award, winner, 0) #changes df so each award is a column, 0 used instead of NA

#merging df
new_oscars <- left_join(nominees_oscar, others)
new_oscars_trim <- select(new_oscars, winner, category, year, 
                          BAFTA, GG, Guild = Screen, country = Country, release_date, running_time, stars_count, writers_count)

#trimming OG dataset  
oscars %>% names()
oscars_trim <- select(oscars, winner, category, year, BAFTA, GG, Guild, country, release_date, running_time, stars_count, writers_count)
oscars_trim %>% names()

head(oscars_trim)
head(new_oscars_trim)

tail(oscars_trim)
tail(new_oscars_trim)

oscars <- rbind(oscars_trim, new_oscars_trim)
oscars <- oscars %>% 
  mutate(country = str_replace_all(country, "\\[.{1,}\\]", ""),
         running_time = as.numeric(running_time)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) 

# Removing Columns With Too Many Missing Values
cols_na <- apply(oscars, 2, is.na) %>% apply(., 2, sum)
oscars <- oscars[ , names(cols_na)[cols_na <= 395]]
oscars <- oscars[complete.cases(oscars), ]
oscars <- oscars %>% select(-name, -film, -award)

# Setting CV Parameter
set.seed(15)

ctrl <- trainControl(method = "cv", number = 10)

nnetGrid <- expand.grid(.decay = c(0, 0.01, 0.5, .1),    .size = c(1:30))

oscars$winner <- as.factor(oscars$winner)

## BINOMIAL
log_model <- oscars %>% 
  split(.$category) %>%
  map(., select, winner, year, BAFTA, GG, Guild, release_date, running_time, stars_count, writers_count) %>%
  map(~ (train(winner ~ ., data = ., method = "glm", family = "binomial", trControl = ctrl))) %>%
  map("results") %>%
  map("Accuracy")

## Neural Net Model
nnet_model <- oscars %>% 
  split(.$category) %>%
  map(., select, -category) %>%
  map(~ (train(winner ~ ., data = ., method = "nnet", tuneGrid = nnetGrid, preProcess = c("center", "scale"), trace = FALSE, trControl = ctrl))) %>%
  map("results") %>%
  map("Accuracy") %>%
  map(mean)


nnet_model2 <- oscars %>% 
  split(.$category) %>%
  map(., select, winner, year, BAFTA, GG, Guild, release_date, running_time, stars_count, writers_count) %>%
  map(~ (train(winner ~ ., data = ., method = "nnet", tuneGrid = nnetGrid, preProcess = c("center", "scale"), trace = FALSE, trControl = ctrl))) %>%
  map("results") %>%
  map("Accuracy") %>%
  map(mean)

### 
oscars %>% 
  filter(year >= 1995) %>%
  split(.$category) %>%
  map(., select, winner, year, BAFTA, GG, Guild, release_date, running_time, stars_count, writers_count) %>%
  map(~ (train(winner ~ ., data = ., method = "glm", family = "binomial", trControl = ctrl))) %>%
  map("results") %>%
  map("Accuracy")


newdata <- oscars %>% filter(year == 2019)

models <- oscars %>% 
  filter(year >= 1995 & year < 2019) %>%
  filter(category != "Director") %>%
  split(.$category) %>%
  map(., select, winner, year, BAFTA, GG, Guild, release_date, running_time, stars_count, writers_count) %>%
  map(~ (train(winner ~ ., data = ., method = "glm", family = "binomial", trControl = ctrl)))


predictions <- map2(models, newdata %>% split(.$category), predict, type = "prob") %>% map("1")

newdata %>%  filter(category == "Supporting Actress")
predictions[["Supporting Actress"]]
new_oscars %>% filter(year == 2019) %>% filter(category == "Supporting Actress") %>% select(name, film, running_time, BAFTA)
