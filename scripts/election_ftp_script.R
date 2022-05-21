#*********************************************************************#
#### Election script for ftp data download and mapping ####
#*********************************************************************#

#### Load packages ####
library(aecfeedr)
library(tidyverse)
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


#*********************************************************************#
#### 2022 ####
#*********************************************************************#
# read in pollingplace location data
# downloaded from AEC:
# https://www.aec.gov.au/Electorates/maps.htm

stns <- read.csv("data/GeneralPollingPlacesDownload-24310.csv",
                 header = TRUE)
# read in AEC election division boundaries
# https://www.aec.gov.au/Electorates/gis/index.htm
election.bound <- readOGR(dsn="data/national-esri-fe2019/COM_ELB_region.shp", layer="COM_ELB_region")

#### Download base maps from Google maps ####
# at different zoom levels
#*********************************************************************#
map.pen.8 <- get_map(location="Penrith, Australia",
                     source= "google",
                     maptype = "terrain", crop=FALSE,
                     zoom=8)

map.wah.12 <- get_map(location="Balgowlah, Australia",
                      source= "google",
                      maptype = "terrain", crop=FALSE,
                      zoom=12)

map.went.12 <- get_map(location="Bondi Junction, Australia",
                      source= "google",
                      maptype = "terrain", crop=FALSE,
                      zoom=12)
map.koo.12 <- get_map(location="Canterbury, Victoria, Australia",
                       source= "google",
                       maptype = "terrain", crop=FALSE,
                       zoom=12)

# View maps
ggmap(map.pen.8)
ggmap(map.wah.12)
ggmap(map.went.12)
ggmap(map.koo.12)

#### Find url for ftp downloads for 2022 election ####
#**********************************************************************#

# ftp://mediafeed.aec.gov.au/27966/Detailed/Verbose/ from below
# See media feed info: https://www.aec.gov.au/media/mediafeed/index.htm
# https://results.aec.gov.au/
# Need to work out number for election year. Can use filezilla with this ftp:
# mediafeed.aec.gov.au

url.2022 <- feed_get_url(27966, 
                         granularity = "Detailed", 
                         verbosity = "Verbose",
                         archived = F)

# function for data wrangling
tp.seat <- function(seatofinterest){
  seat <- house.fp.2022 %>%
    purrr::pluck("contests") %>%
    filter(contest_name == seatofinterest)
  polling <- house.fp.2022 %>%
    purrr::pluck("pollingplaces") %>%
    filter(contest_id == seat$contest_id)
  candidates <- house.fp.2022 %>%
    purrr::pluck("candidates") %>%
    filter(contest_id == seat$contest_id)
  votes.pp <- house.2022 %>%
    purrr::pluck("results_tcp_by_pp") %>%
    filter(contest_id == seat$contest_id)
  votes.can <- votes.pp %>%
    left_join(candidates, by = "candidate_id" )
  return(votes.can)
}

### Download latest datasets ####
# Latest dataset: re-run to refresh data (updated every 5 min)
#*********************************************************************#
# get latest data file name
file.list <- feed_list_files(url.2022) %>%
  last(.)

# Download and unzip latest file  
feed.2022 <- feed_get_messages(url.2022, file.list,
                                   destpath = "data-raw")
# Process xml into a list
house.2022 <- read_results_house(feed.2022[47])
house.fp.2022 <- read_preload_house_fp(feed.2022[47])

### pick seats of interest ####
#*********************************************************************#
# Process list
# Join election results with polling place data
# macq update
macq <- tp.seat("Macquarie") %>% inner_join(stns, by = c("pollingplace_id" = "PollingPlaceID")) %>%
  group_by(pollingplace_id) %>%
  filter(votes==max(votes))

# Warringh update
warr <- tp.seat("Warringah") %>% inner_join(stns, by = c("pollingplace_id" = "PollingPlaceID")) %>%
  group_by(pollingplace_id) %>%
  filter(votes==max(votes))

# Wentworth update
went <- tp.seat("Wentworth") %>% inner_join(stns, by = c("pollingplace_id" = "PollingPlaceID")) %>%
  group_by(pollingplace_id) %>%
  filter(votes==max(votes))

# Kooyong update
koo <- tp.seat("Kooyong") %>% inner_join(stns, by = c("pollingplace_id" = "PollingPlaceID")) %>%
  group_by(pollingplace_id) %>%
  filter(votes==max(votes))

# using AEC shapfile
# Leading party per booth - two party preferred
macq.win.plot <- ggmap(map.pen.8) +
  geom_point(data = macq, aes(x = Longitude, y = Latitude, colour=affiliation_name),
             size = 1.5, alpha = 1, inherit.aes = FALSE) +
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Macquarie",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(149.9,151.5), expand = c(0, 0)) + # change based on area
  scale_y_continuous(limits = c(-34, -32.9), expand = c(0, 0)) # change based on area


warr.win.plot <- ggmap(map.wah.12) +
  geom_point(data = warr, aes(x = Longitude, y = Latitude, colour=candidate_name),
             size = 1.5, alpha = 1, inherit.aes = FALSE) +
  scale_color_manual(values=c("blue", "#E69F00"))+
  geom_polygon(aes(x = long, y =lat), colour="black", fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Warringah",])) +
  theme_map() + coord_equal() + 
  scale_x_continuous(limits = c(151.15,151.35), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-33.89, -33.73), expand = c(0, 0))

went.win.plot <- ggmap(map.went.12) +
  geom_point(data = went, aes(x = Longitude, y = Latitude,
                              colour=candidate_name),
             size = 1.5, alpha = 1, inherit.aes = FALSE) +
  scale_color_manual(values=c("blue", "#E69F00"))+
  geom_polygon(aes(x = long, y =lat), colour="black",
               fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Wentworth",])) +
  theme_map() + coord_equal() +
  scale_x_continuous(limits = c(151.20,151.35), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-33.97, -33.78), expand = c(0, 0))


koo.win.plot <- ggmap(map.koo.12) +
  geom_point(data = koo, aes(x = Longitude, y = Latitude,
                              colour=candidate_name),
             size = 1.5, alpha = 1, inherit.aes = FALSE) +
  scale_color_manual(values=c("blue", "#E69F00"))+
  geom_polygon(aes(x = long, y =lat), colour="black",
               fill=NA, size= 1,
               data = fortify(election.bound[election.bound$Elect_div=="Kooyong",])) +
  theme_map() + coord_equal() +
  scale_x_continuous(limits = c(144.97,145.15), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-37.9, -37.75), expand = c(0, 0))

#*****************************************************************************#
#### saving out plots ####
#*****************************************************************************#
ggsave(filename = "output/macqTP-20220521-12.png", plot = macq.win.plot,
       dpi=300,
       width = 150,
       height = 100,
       units = "mm")


ggsave(filename = "output/warrTP-20220521-12.png", plot = warr.win.plot,
       dpi=300,
       width = 150,
       height = 100,
       units = "mm")

ggsave(filename = "output/wentTP-20220521-12.png", plot = went.win.plot,
       dpi=300,
       width = 150,
       height = 100,
       units = "mm")

ggsave(filename = "output/kooTP-20220521-12.png", plot = koo.win.plot,
       dpi=300,
       width = 150,
       height = 100,
       units = "mm")
