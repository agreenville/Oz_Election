################################################################################
################################################################################
# Plotting election results in space
#
###############################################################################
###############################################################################

# load packages
library(eechidna)
library(ggplot2)
library(ggthemes)

library(tidyverse)
library(ggmap)
library(rgdal)

# need API key to use google maps
# load api key
# key ="[your API key]"

Sys.getenv("key")

register_google(key = Sys.getenv("key"))


#############################################################################
# Read in data and data prep
#############################################################################
data("nat_map16")
data("nat_data16")

# read in pollingplace location data
stns <- read.csv("data/GeneralPollingPlacesDownload-20499.csv",
                 header = TRUE)
# read in AEC election division boundaries
election.bound <- readOGR(dsn="data/national-esri-fe2019/COM_ELB_region.shp", layer="COM_ELB_region")

# Join election results with polling place data
# not cp2.Macquarie is from Michaels election script
macq <- inner_join(cp2.Macquarie.24, stns, by = c("PollingPlace" = "PollingPlaceNm"))

# convert to numeric
macq$`TEMPLEMAN%` <- as.numeric(macq$`TEMPLEMAN%`)
macq$`RICHARDS%` <- as.numeric(macq$`RICHARDS%`)
macq$Swing <- as.numeric(macq$Swing)

# filter for macquarie and add field for who is in the lead
# and the swing to party in the lead
macq <- macq %>% filter(DivisionNm =="Macquarie") %>% 
    mutate(winning = ifelse(`TEMPLEMAN%` > `RICHARDS%` , "Labor", "LNP" )) %>%
  mutate(swing.to = ifelse(Swing<0, "LNP", "Labor")) %>%
  mutate(swing.abs = abs(Swing))

# viewing boundaries for Australia from package data
ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = stns, aes(x = Longitude, y = Latitude),
    colour = "red", size = 1, alpha = 0.3, inherit.aes = FALSE) +
   xlim(c(112,157)) + 
  ylim(c(-44, -11)) + theme_map() + coord_equal()


# Download base maps from Google maps
# at different zoom levels
map.pen.10 <- get_map(location="Penrith, Australia",
                 source= "google",
                 maptype = "terrain", crop=FALSE,
                 zoom=10)

map.pen.9 <- get_map(location="Penrith, Australia",
                      source= "google",
                      maptype = "terrain", crop=FALSE,
                      zoom=9)

map.pen.8 <- get_map(location="Penrith, Australia",
                     source= "google",
                     maptype = "terrain", crop=FALSE,
                     zoom=8)
# View maps
ggmap(map.pen.10)
ggmap(map.pen.9)
ggmap(map.pen.8)

# Convert for plotting in ggplt
map.pen.data.9 <- ggmap(map.pen.9)
map.pen.data.9

map.pen.data.10 <- ggmap(map.pen.10)
map.pen.data.10

map.pen.data.8 <- ggmap(map.pen.8)
map.pen.data.8

#################################################################################
# Mapping data
################################################################################

# Party leading at each booth (two-party prefered)
# using eechidna package election boundaries
ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=winning),
              size = 2, alpha = 0.3, inherit.aes = FALSE) +
  xlim(c(149.9,151.5)) + 
  ylim(c(-34, -33)) + theme_map() + coord_equal() 

# using eechidna package boundary layer
# Leading party per booth - two party preferred
ggmap(map.pen.9) +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=winning),
             size = 2, alpha = 1, inherit.aes = FALSE) +
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = subset(nat_map16, elect_div == "MACQUARIE")) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(149.9,151.5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-34, -33), expand = c(0, 0))

# using AEC shapfile
# Leading party per booth - two party preferred
macq.win.plot <- ggmap(map.pen.8) +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=winning),
             size = 2, alpha = 1, inherit.aes = FALSE) +
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Macquarie",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(149.9,151.5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-34, -32.9), expand = c(0, 0))


# Swing by booth
# Using eechidna package election boundaries
ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=swing.to),
             size = log(macq$swing.abs)*2, alpha = 0.3, inherit.aes = FALSE) +
  xlim(c(149.9,151.5)) + 
  ylim(c(-34, -33)) + theme_map() + coord_equal() 



# using AEC shapfile
# swing by booth - two party preferred
macq.swing.plot <- ggmap(map.pen.8) +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=swing.to),
             size = log(macq$swing.abs)*2, alpha = 0.5, inherit.aes = FALSE) +
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Macquarie",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(149.9,151.5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-34, -32.9), expand = c(0, 0))

################################################################################
# saving out plots
###############################################################################
# ggsave(filename = "output/macq.win.png", plot = macq.win.plot,
#        dpi=300)
# 
# ggsave(filename = "output/macq.swing.png", plot = macq.swing.plot,
#        dpi=300)

