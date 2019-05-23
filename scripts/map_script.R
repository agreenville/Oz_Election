

library(eechidna)
library(ggplot2)
library(ggthemes)
data("nat_map16")
data("nat_data16")
library(tidyverse)

stns <- read.csv("data/GeneralPollingPlacesDownload-20499.csv",
                 header = TRUE)

ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = stns, aes(x = Longitude, y = Latitude),
    colour = "red", size = 1, alpha = 0.3, inherit.aes = FALSE) +
   xlim(c(112,157)) + 
  ylim(c(-44, -11)) + theme_map() + coord_equal()


macq <- inner_join(cp2.Macquarie, stns, by = c("PollingPlace" = "PollingPlaceNm"))

macq$`TEMPLEMAN%` <- as.numeric(macq$`TEMPLEMAN%`)
macq$`RICHARDS%` <- as.numeric(macq$`RICHARDS%`)
macq$Swing <- as.numeric(macq$Swing)

macq <- macq %>% filter(DivisionNm =="Macquarie") %>% 
    mutate(winning = ifelse(`TEMPLEMAN%` > `RICHARDS%` , "Labor", "LNP" )) %>%
  mutate(swing.to = ifelse(Swing<0, "LNP", "Labor")) %>%
  mutate(swing.abs = abs(Swing))

# Party leading at each booth (two-party prefered)

ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=winning),
              size = 2, alpha = 0.3, inherit.aes = FALSE) +
  xlim(c(149.9,151.5)) + 
  ylim(c(-34, -33)) + theme_map() + coord_equal() 

# Swing by booth

ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=swing.to),
             size = log(macq$swing.abs)*2, alpha = 0.3, inherit.aes = FALSE) +
  xlim(c(149.9,151.5)) + 
  ylim(c(-34, -33)) + theme_map() + coord_equal() 
