################################################################################
################################################################################
# Plotting election results in space
#
###############################################################################
###############################################################################

# load packages
library(eechidna) # has spatial data of interest, but based on 2016
library(ggplot2)
library(ggthemes)

library(tidyverse)
library(ggmap)
library(rgdal)

# need API key to use google maps
# load api key
# need to obtain an API key from Google. Go to the registration page
# and sign up for a billing account 
# key ="[your API key]"

register_google(key = Sys.getenv("key"))


#############################################################################
# Read in data and data prep
#############################################################################
data("nat_map16")
data("nat_data16")

# read in pollingplace location data
stns <- read.csv("data/GeneralPollingPlacesDownload-24310.csv",
                 header = TRUE)
# read in AEC election division boundaries
election.bound <- readOGR(dsn="data/national-esri-fe2019/COM_ELB_region.shp", layer="COM_ELB_region")


# Join election results with polling place data
# note cp2.Macquarie is from Michaels election script
#macq <- inner_join(cp2.Macquarie.24, stns, by = c("PollingPlace" = "PollingPlaceNm"))

wah <- cp2.warringah.24 %>% inner_join(stns, by = c("PollingPlace" = "PollingPlaceNm")) %>%
         mutate_at(c('ABBOTT%', 'STEGGALL%', 'Swing'), as.numeric) %>%
        filter(DivisionNm=='Warringah') %>%
        mutate(winning = ifelse(`ABBOTT%` > `STEGGALL%`, "LNP", "Ind" )) %>%
        mutate(swing.to = ifelse(Swing<0, "Ind", "LNP")) %>%
        mutate(swing.abs = abs(Swing))

rob <- cp2.Robertson.27 %>% inner_join(stns, by = c("PollingPlace" = "PollingPlaceNm")) %>%
  mutate_at(c('WICKS%', 'CHARLTON%', 'Swing'), as.numeric) %>%
  filter(DivisionNm=='Robertson') %>%
  mutate(winning = ifelse(`WICKS%` > `CHARLTON%`, "LNP", "Labor" )) %>%
  mutate(swing.to = ifelse(Swing<0, "Labor", "LNP")) %>%
  mutate(swing.abs = abs(Swing))

# macq update
macq <- cp2.Macquarie.29 %>% inner_join(stns, by = c("PollingPlace" = "PollingPlaceNm")) %>%
  mutate_at(c('TEMPLEMAN%', 'RICHARDS%', 'Swing'), as.numeric) %>%
  filter(DivisionNm=='Macquarie') %>%
  mutate(winning = ifelse(`TEMPLEMAN%` > `RICHARDS%`, "Labor", "LNP" )) %>%
  mutate(swing.to = ifelse(Swing<0, "LNP", "Labor")) %>%
  mutate(swing.abs = abs(Swing))


# # convert to numeric
# macq$`TEMPLEMAN%` <- as.numeric(macq$`TEMPLEMAN%`)
# macq$`RICHARDS%` <- as.numeric(macq$`RICHARDS%`)
# macq$Swing <- as.numeric(macq$Swing)
# 
# # filter for macquarie and add field for who is in the lead
# # and the swing to party in the lead
# macq <- macq %>% filter(DivisionNm =="Macquarie") %>% 
#     mutate(winning = ifelse(`TEMPLEMAN%` > `RICHARDS%` , "Labor", "LNP" )) %>%
#   mutate(swing.to = ifelse(Swing<0, "LNP", "Labor")) %>%
#   mutate(swing.abs = abs(Swing))

# all booths
# create list of seats
seat.names <- unique(stns$DivisionNm)

cp2.all.booths <- list()
for(i in 1:length(seat.names)){
  cp2.all.booths[[i]] <- GetBooth2CP(seat.names[i])

}
names(cp2.all.booths) <- seat.names

# AEC download data. Some missing seats
all.booths <- read.csv("data/HouseTppByPollingPlaceDownload-24310.csv", header = TRUE)

all.booths.loc <- all.booths %>% inner_join(stns, by = 'PollingPlaceID') %>%
   mutate(winning = ifelse(Liberal.National.Coalition.Percentage == 0, 
    NA,      
     ifelse(Liberal.National.Coalition.Percentage > Australian.Labor.Party.Percentage, "LNP", "Labor" )
   ))

# Banks data
banks <- cp2.all.booths[["Banks"]] %>% inner_join(stns, by = c("PollingPlace" = "PollingPlaceNm")) %>%
  mutate_at(c('COLEMAN%', 'GAMBIAN%', 'Swing'), as.numeric) %>%
  filter(DivisionNm=='Banks') %>%
  mutate(winning = ifelse(`COLEMAN%` > `GAMBIAN%`, "LNP", "Labor" )) %>%
  mutate(swing.to = ifelse(Swing<0, "Labor", "LNP")) %>%
  mutate(swing.abs = abs(Swing))

     
     
# viewing boundaries for Australia from package data
ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = stns, aes(x = Longitude, y = Latitude),
    colour = "red", size = 1, alpha = 0.3, inherit.aes = FALSE) +
   xlim(c(112,157)) + 
  ylim(c(-44, -11)) + theme_map() + coord_equal()


# all booths data missing
all.booths.plot <- ggplot(data = nat_data16, aes(map_id = id)) + 
  geom_map(map = nat_map16, fill = "grey90", colour = "white") +
  geom_point(data = all.booths.loc, aes(x = Longitude, y = Latitude, 
                                        colour = winning),
              size = 1, alpha = 0.3, inherit.aes = FALSE) +
  scale_color_manual(values = c( "red","blue","#E7B800"))+
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

map.wah.12 <- get_map(location="Balgowlah, Australia",
                     source= "google",
                     maptype = "terrain", crop=FALSE,
                     zoom=12)

map.banks.12 <- get_map(location="PeakHurst, Australia",
                      source= "google",
                      maptype = "terrain", crop=FALSE,
                      zoom=12)

map.rob.10 <- get_map(location="Calga, Australia",
                        source= "google",
                        maptype = "terrain", crop=FALSE,
                        zoom=10)
# for saving R objects
# save(map.pen.8, map.pen.9,map.pen.10, file = "data/Macq_maps.RData")
# save(map.wah.12, map.banks.12,map.rob.10, file = "data/other_maps.RData")

# View maps
ggmap(map.pen.10)
ggmap(map.pen.9)
ggmap(map.pen.8)
ggmap(map.wah.12)
ggmap(map.banks.12)
ggmap(map.rob.10)

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
  scale_y_continuous(limits = c(-34, -32.9), expand = c(0, 0))

# using AEC shapfile
# Leading party per booth - two party preferred
macq.win.plot <- ggmap(map.pen.8) +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=winning),
             size = 3, alpha = 1, inherit.aes = FALSE) +
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Macquarie",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(149.9,151.5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-34, -32.9), expand = c(0, 0))

banks.win.plot <- ggmap(map.banks.12) +
  geom_point(data = banks, aes(x = Longitude, y = Latitude, colour=winning),
             size = 2, alpha = 1, inherit.aes = FALSE) +
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Banks",])) +
  theme_map() + coord_equal()  +
  scale_x_continuous(limits = c(150.9,151.15), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-34.01, -33.92), expand = c(0, 0))


# using AEC shapfile
# Leading party per booth - two party preferred
wah.win.plot <- ggmap(map.wah.12) +
  geom_point(data = wah, aes(x = Longitude, y = Latitude, colour=winning),
             size = 3, alpha = 1, inherit.aes = FALSE) +
  scale_color_manual(values = c("#E7B800", "blue"))+
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Warringah",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(151.15,151.35), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-33.89, -33.73), expand = c(0, 0))

# using AEC shapfile
# Leading party per booth - two party preferred
rob.win.plot <- ggmap(map.rob.10) +
  geom_point(data = rob, aes(x = Longitude, y = Latitude, colour=winning),
             size = 3, alpha = 1, inherit.aes = FALSE) +
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Robertson",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(150.95,151.48), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-33.6, -33.1), expand = c(0, 0))

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

wah.swing.plot <- ggmap(map.wah.12) +
  geom_point(data = wah, aes(x = Longitude, y = Latitude, colour=swing.to),
             size = (wah$swing.abs)/5, alpha = 0.5, inherit.aes = FALSE) +
  scale_color_manual(values = c("#E7B800", "blue"))+
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Warringah",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(151.15,151.35), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-33.89, -33.73), expand = c(0, 0))

banks.swing.plot <- ggmap(map.banks.12) +
  geom_point(data = banks, aes(x = Longitude, y = Latitude, colour=swing.to),
             size = (banks$swing.abs)/2, alpha = 0.5, inherit.aes = FALSE) +
  #  scale_color_manual(values = c("#E7B800", "blue"))+
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Banks",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(150.9,151.15), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-34.01, -33.92), expand = c(0, 0))

rob.swing.plot <- ggmap(map.rob.10) +
  geom_point(data = rob, aes(x = Longitude, y = Latitude, colour=swing.to),
             size = (rob$swing.abs)/2, alpha = 0.5, inherit.aes = FALSE) +
  #  scale_color_manual(values = c("#E7B800", "blue"))+
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Robertson",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(150.95,151.48), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-33.6, -33.1), expand = c(0, 0))

################################################################################
# saving out plots
###############################################################################
# ggsave(filename = "output/macq29-win.png", plot = macq.win.plot,
#        dpi=300)
# 
# ggsave(filename = "output/macq29-swing.png", plot = macq.swing.plot,
#        dpi=300)

# ggsave(filename = "output/wah-win.png", plot = wah.win.plot,
#               dpi=300)

# ggsave(filename = "output/wah-swing.png", plot = wah.swing.plot,
#        dpi=300)

# 
# ggsave(filename = "output/banks-win.png", plot = banks.win.plot,
#               dpi=300)
# 
# ggsave(filename = "output/banks-swing.png", plot = banks.swing.plot,
#        dpi=300)

# ggsave(filename = "output/allBooths-win.png", plot = all.booths.plot,
#        dpi=300)
# 
# ggsave(filename = "output/rob-win.png", plot = rob.win.plot,
#               dpi=300)
# 
# ggsave(filename = "output/rob-swing.png", plot = rob.swing.plot,
#        dpi=300)




