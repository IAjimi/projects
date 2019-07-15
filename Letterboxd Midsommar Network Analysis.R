### MIDSOMMAR LETTERBOXD NETWORK ANALYSIS
# loads of help from https://kateto.net/networks-r-igraph

### Preparation ####
# Loading Libraries
library(readr)
library(igraph)
library(tidyverse)

# Loading Datasets
letterboxd_network <- read_csv("Coding/ON GITHUB/letterboxd_network.csv")
movie_reviews <- read_csv("Coding/ON GITHUB/movie_reviews_midsommar.csv")

### Mapping the Whole Network ####
# Creating list of vertices
all_users <- unique(c(letterboxd_network$following, letterboxd_network$followers))
### merging following & followers, keeping unique users only


# Using movie reviews to add info on having seen movie or not
new_df <- data.frame(user = all_users, seen = rep(0, length(all_users)))
new_df$seen[new_df$user %in% movie_reviews$user == TRUE] <- 1

net <- graph_from_data_frame(d=letterboxd_network, vertices=new_df, directed=T) 

## changing color attribute
colrs <- V(net)$seen
colrs[V(net)$seen == 1] <- "tomato"
colrs[V(net)$seen != 1] <- "gold"

V(net)$color <- colrs 

## plot
l <- layout_nicely(net) #network layout layout 

ec <- eigen_centrality(net, directed=T, weights=NA)$vector #using eigencentrality as weights
plot(net, edge.arrow.size=.4, vertex.label=NA, vertex.size= ec*30, layout=l)
legend(x=-1.5, y=-1.1,  c("Seen","Not Yet Seen"), 
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

deg_in <- degree(net, mode="in") #using degree (n followers) as weights
plot(net, edge.arrow.size=.4, vertex.label=NA, vertex.size= deg_in/2, layout=l)
legend(x=-1.5, y=-1.1,  c("Seen","Not Yet Seen"), 
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

### Mapping Select Network ####
# mapping portion of the network related to high eigencentrality users
users_0 <- names(sort(ec, decreasing = TRUE)[1:5])

users_1 <- unique(c(filter(letterboxd_network, following %in% users_0 | followers %in% users_0)$following,
                    filter(letterboxd_network, following %in% users_0 | followers %in% users_0)$followers))

select_network <- filter(letterboxd_network, following %in% users_1 | followers %in% users_1)
select_users <- unique(c(select_network$following, select_network$followers))

select_seen_frame <- data.frame(user = select_users, seen = rep(0, length(select_users)))
select_seen_frame$seen[select_seen_frame$user %in% movie_reviews$user == TRUE] <- 1

## plot
net_select1 <- graph_from_data_frame(d=select_network, vertices=select_seen_frame, directed=T) 

## changing color attribute
colrs <- V(net_select1)$seen
colrs[V(net_select1)$seen == 1] <- "tomato"
colrs[V(net_select1)$seen != 1] <- "gold"

V(net_select1)$color <- colrs 

## plot
l <- layout_nicely(net_select1) #layout 

ec <- eigen_centrality(net_select1, directed=T, weights=NA)$vector #using eigencentrality as weights
plot(net_select1, edge.arrow.size=.4, vertex.label=NA, vertex.size= ec*30, layout=l,
     main = "Letterboxd Network Sample")
legend(x=-1.5, y=-1.1,  c("Seen","Not Yet Seen"), 
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

### Mapping Even More Select Network ###
## exactly same as before but with only 1 user
users_1 <- unique(c(filter(letterboxd_network, following %in% "aimeereynolds" | followers %in% "aimeereynolds")$following,
                    filter(letterboxd_network, following %in% "aimeereynolds" | followers %in% "aimeereynolds")$followers))

select_network_aimee <- filter(letterboxd_network, following %in% users_1 | followers %in% users_1)
select_users_aimee <- unique(c(select_network_aimee$following, select_network_aimee$followers))

select_aimee_frame <- data.frame(user = select_users_aimee, seen = rep(0, length(select_users_aimee)))
select_aimee_frame$seen[select_aimee_frame$user %in% movie_reviews$user == TRUE] <- 1

## plot
net_select2 <- graph_from_data_frame(d=select_network_aimee, vertices=select_aimee_frame, directed=T) 

## changing color attribute
colrs <- V(net_select2)$seen
colrs[V(net_select2)$seen == 1] <- "tomato"
colrs[V(net_select2)$seen != 1] <- "gold"

V(net_select2)$color <- colrs 

## plot
l <- layout_nicely(net_select2) #layout 

ec <- eigen_centrality(net_select2, directed=T, weights=NA)$vector #using eigencentrality as weights
plot(net_select2, edge.arrow.size=.4, vertex.label=NA, vertex.size= ec*30, layout=l,
     main = "Letterboxd Network Sample")
legend(x=-1.5, y=-1.1,  c("Seen","Not Yet Seen"), 
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

## Looping Graph - Change over Time ####
## creating set of dates to loop over
movie_reviews$date <- as.Date(movie_reviews$date) #creating date type object
dates <- unique(movie_reviews$date) #keeping unique dates
dates <- dates[!is.na(dates)] #removing NA
dates <- sort(dates) #ordering dates

## pre-plot factors
net_plot <- graph_from_data_frame(d=select_network_aimee, vertices=select_users_aimee, directed=T) 
l <- layout_nicely(net_plot) 
ec <- eigen_centrality(net_plot, directed=T, weights=NA)$vector #using eigencentrality as weights

## loop
for (i in c(1:length(dates))) { #creating graph for every day

  #changing color attribute based on viewing data
  new_df <- data.frame(user = select_users_aimee, seen = rep(0, length(select_users_aimee)))
  new_df$seen[new_df$user %in% filter(movie_reviews, date <= dates[i])$user == TRUE] <- 1 #add views
  
  colrs <- new_df$seen
  colrs[new_df$seen == 1] <- "tomato"
  colrs[new_df$seen != 1] <- "gold"
  
  V(net_plot)$color <- colrs 
  
  jpeg(paste(dates[i], ".jpeg"))
  
  plot(net_plot, edge.arrow.size=.4, vertex.label=NA, vertex.size= ec*30, layout=l,
       main = "Letterboxd Network Sample", sub = dates[i])
  legend(x=-1.5, y=-1.1,  c("Seen Midsommar","Not Seen Midsommar"), 
         pch=21, col="#777777", pt.bg= c("tomato", "gold"), pt.cex=2, cex=.8, bty="n", ncol=1)
  
  dev.off() #doesnt print plot on screen
}
