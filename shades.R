### SHADES ###
#from https://github.com/the-pudding/data/tree/master/makeup-shades
#https://pudding.cool/2018/06/makeup-shades/
library("tidyverse")
shades <- read_csv("C:/Users/ia767/Downloads/shades.csv")

#Creating Bins
shades$L_group <- cut(shades$L, 9) #creates 10 bins of equal range 

#colors
shade_col <- c("#583218", "#683a20", "#824f30", "#9b5b35", "#ac7752", "#ca9262", 
               "#d9a57d", "#e8bc99", "#fde3cc")

# Plot
shades_lightbin <- shades %>% 
  group_by(brand, brand_short, L_group) %>% #groups by brand, product range, and lightness bin
  summarise(n = sum(length(unique(L))))
#counts num of unique Lightness values per brand & lightness bin
 
shades_lightbin %>% filter(brand_short %in% c("fe", "mu")) %>% 
  ggplot(aes(L_group, n, fill = L_group)) + 
  geom_col(position = "dodge", color = "grey70") +
  scale_fill_manual(values = shade_col) +
  coord_flip()  + 
  guides(fill = FALSE) +
  facet_grid(~ brand) +
  labs(y = "Number of Different Shades", x = "Lightness Range",
       title = "Fenty v. Make Up For Ever", 
       subtitle = "Fenty Provides A Much Greater Range of Products")


## Plot US Bestsellers
shades_lightbin2 <- shades %>% filter(group == 2) %>% #us bestsellers only
  group_by(brand, brand_short, L_group) %>% #groups by brand, product range, and lightness bin
  summarise(n = sum(length(unique(L))))
#counts num of unique Lightness values per brand & lightness bin

shades_lightbin2 %>% 
  ggplot(aes(L_group, n, fill = L_group)) + 
  geom_col(position = "dodge", color = "grey70") +
  scale_fill_manual(values = shade_col) +
  coord_flip()  + 
  guides(fill = FALSE) +
  facet_wrap(~ brand) +
  labs(y = "Number of Different Shades", x = "Lightness Range",
       title = "Foundation Range Per U.S. Bestselling Brand")

## Annex ####
### automated color selector
#### unused bc yields variable results

#avail_cols <- rep(0, 9)
#temp_col <- NA

#for(i in c(1:9)) {
#  temp_col <- shades$hex[shades$L_group == levels(cut(shades$L, 9))[i] ]
#  avail_cols[i] <- sample(temp_col, 1)
#}

#avail_cols <- paste("#", avail_cols, sep = "")
#shade_col <- avail_cols
