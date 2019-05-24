##############################################################################################################################
# Michael McCarthy Election results
#
# Link to 2016 election results
##############################################################################################################################


suppressWarnings(library(RCurl))
suppressWarnings(library(rvest))


#########################################################################################
# Functions
#########################################################################################

# first preference vote for seat and party of interest

# set websites from AEC

seatsite2016="https://tallyroom.aec.gov.au/HouseDivisionalResults-24310.htm"

#https://results.aec.gov.au/20499/Website/HouseDivisionalResults-20499.htm" # 2016 seats

prefix2016 <- "https://tallyroom.aec.gov.au/"
  
#  "https://results.aec.gov.au/20499/Website/"


# First preference vote per booth
# 2019 AEC didn't provide this data on the web
GetBoothFP <- function(seatofinterest, partyofinterest)
{
  # re-code thepartyofinterest to conform with AEC names
  if(partyofinterest=="Greens") partyofinterest <-"The Greens"
  if(partyofinterest=="ALP") partyofinterest <-"Australian Labor Party"
  if(partyofinterest=="Lib") partyofinterest <-"Liberals"
  
  #seatsite2016="https://results.aec.gov.au/20499/Website/HouseDivisionalResults-20499.htm" # 2016 seats
  html2016 <- read_html(seatsite2016)
  
  seats2016_html <- html_nodes(html2016,'.filterDivision')
  seats2016_html
  seattxt2016 <- html_text(seats2016_html)
  #seatlinks2016 <- html_attrs(seats2016_html, "href")
  
  seat.links2016 <- html2016 %>% # feed `html2016` to the next step
    html_nodes(".filterDivision a") %>% # get the info for each division
    html_attr("href")
  
  #prefix2016 <- "https://results.aec.gov.au/20499/Website/"
  
  seat.links2016 <- paste(prefix2016, seat.links2016, sep="")
  
  if(seatofinterest=="Macnamara"){
    seatID<- which(seattxt2016=="Melbourne Ports")
  } else
  {
    SeatID <- which(seattxt2016==seatofinterest)
  }
  # from 2019 Melb Ports is named Macnamara, Check for Batman too.
  
  seat2016 <- seat.links2016[SeatID]  
  seathtml2016 <- read_html(seat2016)
  seats2016_html <- html_nodes(seathtml2016,'a')
  
  pollingplace.names2016 <- seats2016_html[grep("FirstPref", seats2016_html)]%>% html_text()
  
  firstprefs.links2016 <- seats2016_html[grep("FirstPref", seats2016_html)] %>% html_attr("href")
  firstprefs.links2016 <- paste(prefix2016, firstprefs.links2016, sep="")
  
  # only take one of each (the TCP results are also in ther with the same links )
  firstprefs.links2016 <- unique(firstprefs.links2016)
  pollingplace.names2016 <- unique(pollingplace.names2016)
  
  npollingplaces <- length(pollingplace.names2016)  # number of polling places in the seat
  
  for (i in 1:npollingplaces)
  {
    fp2016 <- firstprefs.links2016[i] %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="fp"]') %>%    
      html_table(header=TRUE, fill=TRUE)  # extracts the first prefs votes for the booth as a table
    
    fp2016 <- fp2016[[1]]  # only has one element
    
    partyID <- which(fp2016$Party == partyofinterest) # find the data assocaited with the party of interest
    if (i==1){  # if first booth, set up the dataset
      FPVotes2016 <- c(fp2016$Votes[partyID])   # Votes for party of interest
      TotalVotes2016 <- c(fp2016$Votes[length(fp2016$Votes)])  # Total votes in polling place
      FormalVotes2016 <- c(fp2016$Votes[length(fp2016$Votes)-2])  # Total votes in polling place
    }
    else{  # f not the first, add to the dataset that already exists
      FPVotes2016 <- c(FPVotes2016, fp2016$Votes[partyID])
      TotalVotes2016 <- c(TotalVotes2016, fp2016$Votes[length(fp2016$Votes)])
      FormalVotes2016 <- c(FormalVotes2016, fp2016$Votes[length(fp2016$Votes)-2])  # Total votes in polling place
    }
  }  # end for each pollingplace
  
  # The data are read as text from the table - need to strip out the commas (gsub) and convert to numeric (as.numeric)
  FPVotes2016 <- as.numeric(gsub(",", "", FPVotes2016, fixed=TRUE))
  FormalVotes2016 <- as.numeric(gsub(",", "", FormalVotes2016, fixed=TRUE))
  TotalVotes2016 <- as.numeric(gsub(",", "", TotalVotes2016, fixed=TRUE))
  
  FPpercent <- 100*FPVotes2016/FormalVotes2016  # % of formal first pref votes
  
  # gather the results into a dataframe
  booth.data <- data.frame(pollingplace.names2016, FPVotes2016, FormalVotes2016, TotalVotes2016, FPpercent)
  
  return(booth.data)
}

# two-party prefered by booth for seat of interest function
# 2019 election AEC didn't pubish these results on the website
GetBooth2CP <- function(seatofinterest)
{
  #seatsite2016="https://results.aec.gov.au/20499/Website/HouseDivisionalResults-20499.htm" # 2016 seats
  html2016 <- read_html(seatsite2016)
  
  seats2016_html <- html_nodes(html2016,'.filterDivision')
  seats2016_html
  seattxt2016 <- html_text(seats2016_html)
  #seatlinks2016 <- html_attrs(seats2016_html, "href")
  
  seat.links2016 <- html2016 %>% # feed `html2016` to the next step
    html_nodes(".filterDivision a") %>% # get the info for each division
    html_attr("href")
  
  #prefix2016 <- "https://results.aec.gov.au/20499/Website/"
  
  seat.links2016 <- paste(prefix2016, seat.links2016, sep="")
  
  # if(seatofinterest=="Macnamara"){
  #   seatID<- which(seattxt2016=="Melbourne Ports")
  # } else
  # {
    SeatID <- which(seattxt2016==seatofinterest)
  #}
  # from 2019 Melb Ports is named Macnamara, Check for Batman too?
  
  seat2016 <- seat.links2016[SeatID]  
  
  twocp2016 <- seat2016 %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="tcpbpp"]')  %>%  
    html_table(header=TRUE, fill=TRUE)  # extracts the 2CP votes for each booth as a table
  
  twocp2016 <- twocp2016[[1]]  # there should be only one element - get that table
  
  # Get surnames of candidate
  Cand1 <- sapply(strsplit(colnames(twocp2016)[3], ","), "[", 1)
  Cand2 <- sapply(strsplit(colnames(twocp2016)[5], ","), "[", 1)
  
  twocp2016 <- twocp2016[2:length(twocp2016$`Polling place`), ] # chop off the header line
  
  # rename the columns to something more succinct
  colnames(twocp2016) <- c("PollingPlace", "Formal", paste(Cand1,"Votes",sep=""), paste(Cand1,"%",sep=""), paste(Cand2,"Votes",sep=""), paste(Cand2,"%",sep=""), "Swing")
  
  return(twocp2016)
}


# returns primary vote for all candidates at a seat
GetAllCP <- function(seatofinterest)
{
  #seatsite2016="https://results.aec.gov.au/20499/Website/HouseDivisionalResults-20499.htm" # 2016 seats
  html2016 <- read_html(seatsite2016)
  
  seats2016_html <- html_nodes(html2016,'.filterDivision')
  seats2016_html
  seattxt2016 <- html_text(seats2016_html)
  #seatlinks2016 <- html_attrs(seats2016_html, "href")
  
  seat.links2016 <- html2016 %>% # feed `html2016` to the next step
    html_nodes(".filterDivision a") %>% # get the info for each division
    html_attr("href")
  
  #prefix2016 <- "https://results.aec.gov.au/20499/Website/"
  
  seat.links2016 <- paste(prefix2016, seat.links2016, sep="")
  
  if(seatofinterest=="Macnamara"){
    seatID<- which(seattxt2016=="Melbourne Ports")
  } else
  {
    SeatID <- which(seattxt2016==seatofinterest)
  }
  # from 2019 Melb Ports is named Macnamara, Check for Batman too?
  
  seat2016 <- seat.links2016[SeatID]  
  
  twocp2016 <- seat2016 %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="fp"]')  %>%  
    html_table(header=TRUE, fill=TRUE)  # extracts the 2CP votes for each booth as a table
  
  twocp2016 <- twocp2016[[1]]  # there should be only one element - get that table
  
  # # Get surnames of candidate
  # Cand1 <- sapply(strsplit(colnames(twocp2016)[3], ","), "[", 1)
  # Cand2 <- sapply(strsplit(colnames(twocp2016)[5], ","), "[", 1)
  # 
  # twocp2016 <- twocp2016[2:length(twocp2016$`Polling place`), ] # chop off the header line
  # 
  # # rename the columns to something more succinct
  # colnames(twocp2016) <- c("PollingPlace", "Formal", paste(Cand1,"Votes",sep=""), paste(Cand1,"%",sep=""), paste(Cand2,"Votes",sep=""), paste(Cand2,"%",sep=""), "Swing")
  # 
  return(twocp2016)
}

# returns 2-party preferred table for a seat

Get2CP <- function(seatofinterest)
{
  #seatsite2016="https://results.aec.gov.au/20499/Website/HouseDivisionalResults-20499.htm" # 2016 seats
  html2016 <- read_html(seatsite2016)
  
  seats2016_html <- html_nodes(html2016,'.filterDivision')
  seats2016_html
  seattxt2016 <- html_text(seats2016_html)
  #seatlinks2016 <- html_attrs(seats2016_html, "href")
  
  seat.links2016 <- html2016 %>% # feed `html2016` to the next step
    html_nodes(".filterDivision a") %>% # get the info for each division
    html_attr("href")
  
  #prefix2016 <- "https://results.aec.gov.au/20499/Website/"
  
  seat.links2016 <- paste(prefix2016, seat.links2016, sep="")
  
  if(seatofinterest=="Macnamara"){
    seatID<- which(seattxt2016=="Melbourne Ports")
  } else
  {
    SeatID <- which(seattxt2016==seatofinterest)
  }
  # from 2019 Melb Ports is named Macnamara, Check for Batman too?
  
  seat2016 <- seat.links2016[SeatID]  
  
  twocp2016 <- seat2016 %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="tcp"]')  %>%  
    html_table(header=TRUE, fill=TRUE)  # extracts the 2CP votes for each booth as a table
  
  twocp2016 <- twocp2016[[1]]  # there should be only one element - get that table
  
  # # Get surnames of candidate
  # Cand1 <- sapply(strsplit(colnames(twocp2016)[3], ","), "[", 1)
  # Cand2 <- sapply(strsplit(colnames(twocp2016)[5], ","), "[", 1)
  # 
  # twocp2016 <- twocp2016[2:length(twocp2016$`Polling place`), ] # chop off the header line
  # 
  # # rename the columns to something more succinct
  # colnames(twocp2016) <- c("PollingPlace", "Formal", paste(Cand1,"Votes",sep=""), paste(Cand1,"%",sep=""), paste(Cand2,"Votes",sep=""), paste(Cand2,"%",sep=""), "Swing")
  # 
  return(twocp2016)
}


######################################################################################################################################################################
#Usage
######################################################################################################################################################################
cp2.warringah.24 <- GetBooth2CP("Warringah")


cp2.Macquarie.24 <- GetBooth2CP("Macquarie")

booth.macq <- GetBoothFP("Macquarie", "Labor")

Get2CP("Macquarie")
GetAllCP("Macquarie")

# Some options you might like:

#GetBoothFP("Wills", "Australian Labor Party")
# GetBoothFP("Wills", "The Greens")
# GetBoothFP("Indi", "Liberal")
GetBoothFP("Warringah", "Liberal")
