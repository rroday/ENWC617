##
## Date Created: 2023-01-20
##
## Copyright (c) Rachel Roday, 2023
## Email: rroday@udel.edu
##
 ---------------------------
##
## Notes: Learning Glatos package for great lakes acoustic telemetry
   ## This file is saved to RRODAY PERSONAL ONE DRIVE-- VIP! (Not on local directory)
##        
##      
##        
## ---------------------------

# Load these first 5 sections to begin data visualization and statistics #
######################## Working Dir and Packages #############################

# Set WD
setwd("C:/Users/RER/Documents/Masters UD/Code")

#install.packages("devtools")
#devtools:::install_github("gearslaboratory/gdalUtils")
#install.packages("remotes")
#remotes::install_github("jsta/glatos")

# Load required libraries
library(glatos)
library(data.table) #setnames
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(network)
library(igraph)
library(ggraph)
library(tidygraph)
#library(argosfilter)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)
library(maps)
library(sf)
library(mapdata)
library(gganimate) 
library(gifski)
library(plotrix) 
library(ggforce)
library(lawstat)
library(epitools)
library(vcd)
library(FSA)
######################## Fxns and Fish data frame ###############################

# Create required functions and dataframes

#make new function to extract second element from a hyphen-delimited string
#apply get_rsn() to each record in Receiver column and create a new column 
#named receiver_sn
get_rsn <- function(x) strsplit(x, "-")[[1]][2]

#NOTE that lat and long are release lat and long from 
rcv <- data.frame(
  glatos_array = c("BR1", "BR2"),
  station = c("1","2"), 
  deploy_lat = c(39.75756, 39.73994),
  deploy_long = c(-75.55351, -75.5371),
  #deploy_lat = 39.75746,
  #deploy_long = -75.75746, 
  deploy_date_time = c(as.POSIXct("2022-05-10 10:00:00", tz = "UTC")),
  recover_date_time = c(as.POSIXct("2022-10-01 17:11:00", tz = "UTC")),
  ins_serial_no = "486334",
  stringsAsFactors = FALSE) 



## Extract transmitter code and ID 
#make a new function to extract id from Transmitter
#i.e., get third element of hyphen-delimited string
parse_tid <- function(x) strsplit(x, "-")[[1]][3]

#make a new function to extract codespace from Transmitter
#i.e., get first two elements of hyphen-delimited string
parse_tcs <- function(x) {
  #split on "-" and keep first two extracted elements
  tx <- strsplit(x, "-")[[1]][1:2]
  #re-combine and separate by "-"
  return(paste(tx[1:2], collapse = "-"))
} 

# Make an example animal (fish) data frame
# This will change from year to year, as more fish are tagged. 

### You must check this against Ed's data in the google drive because fish #1 is a male....
fsh <- data.frame(
  animal_id = c(13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
  tag_code_space = c("A69-1604","A69-1604","A69-1604","A69-1604","A69-1604",
                     "A69-1604","A69-1604","A69-1604","A69-1604","A69-1604",
                     "A69-1604","A69-1604","A69-1604","A69-1604",
                     "A69-9007","A69-9007",
                     "A69-1602","A69-1602","A69-1602","A69-1602","A69-1602",
                     "A69-1602","A69-1602","A69-1602","A69-1602","A69-1602"),
  tag_id_code = c("32457","32458","32459","32460","32461","32462","32463",
                  "32464","32465","32466","32467","32468","32469","32470",
                  "15557","15558","49176", "49177","49178","49179","49180",
                  "49181","49182","49183","49184","49185"), 
  common_name = "American Shad", 
  release_date_time = as.POSIXct(c("2022-05-10 10:00:00", "2022-05-10 10:00:00",
                                   "2022-05-10 10:00:00", "2022-05-10 10:00:00",
                                   "2022-05-10 10:00:00", "2022-05-10 10:00:00",
                                   "2022-05-10 10:00:00", "2022-05-10 10:00:00",
                                   "2022-05-10 10:00:00", "2022-05-10 10:00:00", 
                                   "2022-05-10 10:00:00", "2022-05-10 10:00:00", 
                                   "2022-05-10 10:00:00", "2022-05-10 10:00:00",
                                   "2021-05-11 10:00:00", "2021-05-11 10:00:00",
                                   "2021-05-11 10:00:00", "2021-05-11 10:00:00",
                                   "2021-05-11 10:00:00", "2021-05-11 10:00:00",
                                   "2021-05-11 10:00:00", "2021-05-11 10:00:00",
                                   "2021-05-11 10:00:00", "2021-05-11 10:00:00",
                                   "2021-05-11 10:00:00", "2021-05-11 10:00:00"), 
                                 tz = "UTC"),
  recapture_date_time = as.POSIXct(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                     NA, NA, NA, NA, NA),
                                   tz = "UTC"), 
  Sex = c("Female","Female","Female","Female","Female", "Male", "Female", "Male",
          "Male","Female","Female","Female", "Male", "Male",
          "Male","Male","Female", "Male", "Female", "Male","Male","Female",
          "Male","Female","Female","Female") , 
  Insertion_type = c("Gastric", "Gastric", "Surgical", "Surgical","Surgical",
                     "Surgical","Surgical","Surgical","Surgical","Surgical",
                     "Surgical", "Surgical","Surgical","Surgical", "Surgical", 
                     "Surgical","Surgical",
                     "Surgical","Surgical","Surgical","Surgical","Surgical",
                     "Surgical", "Surgical","Surgical","Surgical"),
  Length.m = c(0.495, 0.540, 0.54, 0.545, 0.495, 0.495, 0.55, 0.52, 0.465, 
                0.530, 0.515, 0.555, 0.490, 0.455,
                0.472,0.493,0.593,0.484,0.522,0.493,0.475,0.517,0.446,0.540,0.521,0.525), 
  Deploy.Year = c("2022", "2022","2022", "2022","2022", "2022","2022", "2022",
                  "2022", "2022","2022", "2022","2022", "2022",
                  "2021","2021","2021","2021","2021","2021",
                  "2021","2021","2021","2021","2021","2021"),
  stringsAsFactors = FALSE)  


colpal <- c("Atlantic" = "#4477AA", 
            "Bay" = "#EE6677", 
            "Brandywine 1" = "#228833", 
            "Brandywine 2" = "#AA3377",
            "Brandywine 3" = "#66CCEE", 
            "River" = "#E69F00", "#000000" )

survival_gap <- as.duration(hms("120:00:00"))

world <- ne_coastline(scale = "medium", returnclass = "sf")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
canada <- map_data("worldHires", "Canada") 

######################## Loading  data ##############################

# Go! Get! Your! Data
# Loading Data from Station BR1 (Receiver VR2Tx_486334)
BR1_2022 <- read.csv("VR2Tx_486334_20230119_2.csv", as.is = TRUE, check.names = FALSE, 
                fileEncoding = "UTF-8-BOM")
BR1_2022$`Station Name` <- as.character(BR1_2022$`Station Name`)
# Loading Data from Ian Park -- All Reciever data with Ed's fish from 2021 and 2022 (Minus BR1)
# AllDetects_2021_2022 <- read.csv("Shad_2021_and_2022_data.csv",as.is = TRUE, 
#                                  check.names = FALSE, fileEncoding = "UTF-8-BOM")

AllDetects_2021_2022 <- read.csv("Copy of EdShad_FROMIANPARK.csv",as.is = TRUE, 
                                 check.names = FALSE, fileEncoding = "UTF-8-BOM")


# Loading more data from Ian Park AND all MATOS data
#MATOS <- read.csv("Acoustic Data from Ian and Dec 2021 MATOS Data Combined.xlsx - EdsShad.csv",
                  #as.is = TRUE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
MATOS <- read.csv("Acoustic Data from Ian and Dec 2021 MATOS Data Combined THIS ONE.csv",
                  as.is = T, check.names =  F, fileEncoding = "UTF-8-BOM", stringsAsFactors = F)

## This is a data frame  that Ian gave us for receiver names 
sampling_stations <- read.csv("Ian Receiver Names and Locations 2021.csv", 
                              header = TRUE)

## This is another data frame from Ian that incldues river KM for all stations.
# yes there are 5 excel sheets that need to be uploaded. I dont make the rules.
RiverKM <- read.csv("Receiver RKM.csv", header = TRUE) %>% rename ("StationName" = "Station.Name")


######################## Tidying  data ##############################

# Full station name df
sampling_stations2 <- full_join(sampling_stations, RiverKM, by=c("StationName", "RKM")) %>%
  filter(!ID %in% c(561, 67,552,565,184,524,527,43,529,531,532,538,539, 522))
  # Removed duplicate stations (stations that appeared more than once. Defaulted
  # to higher RKM value for conservative distance measurments)

# Change the column name from Date and Time (UTC) to detection_timestamp_utc
# setnames(x, old, new)
setnames(AllDetects_2021_2022, "Date and Time (UTC)", "detection_timestamp_utc") 
setnames(BR1_2022, "Date and Time (UTC)", "detection_timestamp_utc") 
setnames(MATOS, "Date and Time (UTC)", "detection_timestamp_utc")

# Format date str bc it was wonky
AllDetects_2021_2022$detection_timestamp_utc <- as.character(mdy_hms(AllDetects_2021_2022$detection_timestamp_utc))
MATOS$detection_timestamp_utc <- as.character(mdy_hms(MATOS$detection_timestamp_utc))

# Merge both datasets
dtc <- full_join(BR1_2022, AllDetects_2021_2022)
dtc2 <- full_join(dtc, MATOS)


# format the timestamp column
dtc2$detection_timestamp_utc <- as.POSIXct(dtc2$detection_timestamp_utc,
                                          tz = "UTC")

# Parsing the serial number of receiver using the fxn we made earlier
dtc2$receiver_sn <- sapply(dtc2$Receiver, get_rsn)

# Changing  the names in the dtc2 dataframe so that its easier to work with
setnames(dtc2, c("Sensor Value", "Sensor Unit", "Station Name"),
         c("sensor_value", "sensor_unit", "station_name"))

# Adding Lat/Long for stations 1 and 2 into the dtc2 dataframe so that Full Join is possible
dtc2$Longitude[dtc2$station_name == "2"] <- -75.5371
dtc2$Latitude[dtc2$station_name == "2"] <- 39.73994
dtc2$Longitude[dtc2$station_name == "1"] <- -75.55351
dtc2$Latitude[dtc2$station_name == "1"] <- 39.75756

#full join on receiver serial number to add receiver data to detections
# this isnt crucial, but its good to have it 
dtc3 <- full_join(dtc2, rcv, by = c("receiver_sn" = "ins_serial_no", "station_name" = "station",
                                    "Latitude" = "deploy_lat", "Longitude" = "deploy_long"))


#subset deployments between receiver deployment and recovery (omit others)
dtc_subset <- dtc3 %>% 
  filter(detection_timestamp_utc < as.POSIXct("2023-01-01 17:11:00", 
                                                               tz = "UTC")) %>%
  filter(detection_timestamp_utc > as.POSIXct("2021-05-01 17:11:00", 
                                              tz = "UTC"))

#apply parse_tcs() to Transmitter and assign to transmitter_codespace
dtc_subset$transmitter_codespace <- sapply(dtc_subset$Transmitter, parse_tcs)

#apply parse_tid() to Transmitter and assign to transmitter_id
dtc_subset$transmitter_id <- sapply(dtc_subset$Transmitter, parse_tid)

## This join is CRUCIAl -> gets us the fish number by transmitter id space
#simple left join on codespace and id
dtc_final <- full_join(dtc_subset, fsh, by = c("transmitter_codespace" = "tag_code_space",
                                              "transmitter_id" = "tag_id_code"))

# Omit NAs -> takes dtc_subset from 45,000 to 9,400 detections
dtc_final <- dtc_final[!is.na(dtc_final$animal_id),]
#dtc_final1 <- dtc_final %>% filter(!station_name== " ")

# Add a year column
dtc_final1 <- dtc_final %>% mutate(Year = lubridate::year(detection_timestamp_utc))


## Cherry Island and New Castle were missing Lat/long and names
dtc_final1$station_name[dtc_final1$Receiver == "VR2AR-547881"] <- "Cherry Island B"
dtc_final1$Longitude[dtc_final1$station_name == "Cherry Island B"] <- -75.50951
dtc_final1$Latitude[dtc_final1$station_name == "Cherry Island B"] <- 39.70142 

dtc_final1$station_name[dtc_final1$Receiver == "VR2AR-547883"] <- "Cherry Island Range A"
dtc_final1$Longitude[dtc_final1$station_name == "Cherry Island Range A"] <- -75.51718
dtc_final1$Latitude[dtc_final1$station_name == "Cherry Island Range A"] <- 39.70366  

dtc_final1$station_name[dtc_final1$Receiver == "VR2AR-547882"] <- "New Castle Flats C"
dtc_final1$Longitude[dtc_final1$station_name == "New Castle Flats C"] <- -75.51934
dtc_final1$Latitude[dtc_final1$station_name == "New Castle Flats C"] <- 39.6712 

dtc_final1$Longitude[dtc_final1$station_name == "C&D Canal West"] <- -75.64542
dtc_final1$Latitude[dtc_final1$station_name == "C&D Canal West"] <- 39.55535

dtc_final1$Longitude[dtc_final1$station_name == "Christina River"] <- -75.53233
dtc_final1$Latitude[dtc_final1$station_name == "Christina River"] <- round(39.73251,5)

# I'm gonna rip my hair out. Not sure why coordinates are off by like 2ft IRL
# but now its this bitches problem to fix it
sampling_stations2$Y[sampling_stations2$StationName == "Christina River"] <- round(39.73251,5)
sampling_stations2$X[sampling_stations2$StationName == "LL# 2950 Delaware River Lighted Buoy 25"] <- round(-75.54714,5)
sampling_stations2$Y[sampling_stations2$StationName == "LL# 2950 Delaware River Lighted Buoy 25"] <- round(39.65608,5)
dtc_final1$Longitude[dtc_final1$station_name == "LL# 2950 Delaware River Lighted Buoy 25"] <- round(-75.54714,5)
dtc_final1$Latitude[dtc_final1$station_name == "LL# 2950 Delaware River Lighted Buoy 25"] <- round(39.65608,5)
sampling_stations2$X[sampling_stations2$StationName == "LL# 2970 Delaware River Lighted Buoy 28"] <- -75.52269
sampling_stations2$Y[sampling_stations2$StationName == "LL# 2970 Delaware River Lighted Buoy 28"] <- 39.67289
sampling_stations2$X[sampling_stations2$StationName == "LL# 2995 Delaware River Lighted Buoy 29"] <- -75.51460
sampling_stations2$Y[sampling_stations2$StationName == "LL# 2995 Delaware River Lighted Buoy 29"] <- 39.70297
sampling_stations2$X[sampling_stations2$StationName == "LL# 3205 Marcus Hook Anchorage Buoy C"] <- -75.38810
sampling_stations2$Y[sampling_stations2$StationName == "LL# 3205 Marcus Hook Anchorage Buoy C"] <- 39.81054
sampling_stations2$X[sampling_stations2$StationName == "LL# 3505 Delaware River Lighted Buoy 68"] <- -75.16659
sampling_stations2$Y[sampling_stations2$StationName == "LL# 3505 Delaware River Lighted Buoy 68"] <- 39.88011
dtc_final1$Longitude[dtc_final1$station_name == "DE River Gate 3A"] <- -75.53357
dtc_final1$Latitude[dtc_final1$station_name == "DE River Gate 3A"] <- 39.42175
dtc_final1$Longitude[dtc_final1$station_name == "DE River Gate 2A"] <- -75.51089
dtc_final1$Latitude[dtc_final1$station_name == "DE River Gate 2A"] <- 39.44077
dtc_final1$Longitude[dtc_final1$station_name == "DE River Gate 1A"] <- -75.51767
dtc_final1$Latitude[dtc_final1$station_name == "DE River Gate 1A"] <- 39.43398

# Remove duplicate
sampling_stations2 <- sampling_stations2 %>%
  filter(!StationName == "LL# 3055 Cherry Is. Flats E. Channel 2")

# lets recode dtc_final1 so that the following join is successful
dtc_final1.5<-dtc_final1 %>% mutate(station_name = recode(station_name, "LL# 3050 Cherry Is Fl East Buoy 1" ="LL# 3050 Cherry Island Flats East Channel Buoy 1",
                                                          "LL# 3055 Cherry Is. Flats E. Channel 2" = "LL# 3055 Cherry Island Flats East Channel Buoy 2",
                                                          "LL# 3065 Cherry Is Flats E Chan. Bouy 4" = "LL# 3065 Cherry Island Flats East Channel Buoy 4",
                                                          "LL#2515 Delaware River Lighted Buoy #3" = "LL# 2515 Delaware River Lighted Buoy 3",
                                                          "LL#2460 Delaware Bay Lighted Buoy #46" = "LL# 2460 Delaware Bay Main Channel Lighted Buoy 46",
                                                          "LL#2472 Delaware Bay Lighter Buoy #49" = "LL# 2472 Delaware Bay Main Channel Lighted Buoy 49",
                                                          "2021 DE Bay Gate 017" = "DE Bay Gate 17",
                                                          "LL# 3055 Cherry Island Flats East Channel Buoy 2" = "LL# 3055 Cherry Island Flats East Channel Buoy 2"))
dtc_final1.5$Longitude[dtc_final1.5$station_name == "LL# 3055 Cherry Island Flats East Channel Buoy 2"] <- -75.48903
dtc_final1.5$Latitude[dtc_final1.5$station_name == "LL# 3055 Cherry Island Flats East Channel Buoy 2"] <- 39.72470
sampling_stations2$Region[sampling_stations2$StationName == "LL# 3000 Delaware River Buoy 56"] <- "River"


# now dtc_final1.5 and sampling_stations2 are identical and can be joined
# This took me 2 hours to figure out- please dont be wrong in the future lol

full.stationz <- full_join(sampling_stations2, dtc_final1.5, by = c("StationName" ="station_name", "X" = "Longitude", 
                                  "Y" = "Latitude")) %>%
  distinct(X, Y, .keep_all = TRUE) #%>%        # distinct matches unique lats and longs 
 # mutate(Region = )

# Except these regions were missing ugh
full.stationz$Region[full.stationz$StationName == "LL# 3685 Upper DE River CB 9"] <- "River"
full.stationz$Region[full.stationz$StationName == "C&D Canal West"] <- "Canal"
full.stationz$Region[full.stationz$StationName == "Christina River"] <- "Brandywine"
full.stationz$Region[full.stationz$StationName == "New Castle Flats B"] <- "River"
full.stationz$Region[full.stationz$StationName == "Cherry Island Range A"] <- "River"
full.stationz$Region[full.stationz$StationName == "LL# 2820 Bulkhead Shoal Channel B 8"] <- "River"
full.stationz$Region[full.stationz$StationName == "New Castle Flats C"] <- "River"
full.stationz$Region[full.stationz$StationName == "C&D Canal East"] <- "Canal"
full.stationz$Region[full.stationz$StationName == "LL# 2965 Delaware River Lighted Buoy 27"] <- "River"
full.stationz$Region[full.stationz$StationName == "Cherry Island B"] <- "River"
full.stationz$Region[full.stationz$StationName == "LL# 3055 Cherry Island Flats East Channel Buoy 2"] <- "River"
full.stationz$Region[full.stationz$StationName == "C&D Canal East"] <- "Canal"
full.stationz$RKM[full.stationz$StationName == "LL# 2965 Delaware River Lighted Buoy 27"] <- 110
full.stationz$RKM[full.stationz$StationName == "New Castle Flats B"] <- 110
full.stationz$RKM[full.stationz$StationName == "New Castle Flats C"] <- 110
full.stationz$RKM[full.stationz$StationName == "Cherry Island Range A"] <- 112
full.stationz$RKM[full.stationz$StationName == "Cherry Island B"] <- 112
full.stationz$RKM[full.stationz$StationName == "LL# 3055 Cherry Island Flats East Channel Buoy 2"] <- 115
full.stationz$RKM[full.stationz$StationName == "LL# 2820 Bulkhead Shoal Channel B 8"] <- 98
full.stationz$Region[full.stationz$StationName == "LL# 2630 Artificial Is Anchorage Buoy B"] <- "River"
full.stationz$Region[full.stationz$StationName == "LL# 2470 DE Bay Main Channel Buoy 48"] <- "River"
full.stationz$Region[full.stationz$StationName == "LL# 2760 Chesapeake and Delaware Canal Light 3"] <- "Canal"




# I only need the station specific guts for the next, and final, join :)
full.stations <- full.stationz %>% 
  dplyr::select(StationName:ID) 

# Trying to do the above code in a better way - but I dont think I can
full.stations2 <- full.stations %>%
  mutate(Sation2 = case_when(RKM > 77 ~ "River",
                             RKM == 113 ~ "Trib",
                             RKM == 89 ~ "Canal",
                             RKM <= 77 ~ "Bay"))

##### TO DO ###
# add station 3000 to full_stations list. Take row from dtc
# and Mantua creek

# Ok last join. Here We add back all the guts of each station (RKM, Region, coordinates, etc) 
# back to dtc_final2, which is our FINAL dataframe (bc its used in the next 1000 lines)
dtc_final1.6 <- left_join(dtc_final1.5, full.stations,
                           by = c("station_name" ="StationName", "Longitude" = "X", 
                                  "Latitude" = "Y")) 
dtc_final1.6 <- dtc_final1.6 %>% drop_na(Transmitter) 


dtc_final1.6$Region[dtc_final1.6$station_name == "Mantua Creek Anchorage Buoy B"] <- "River"
dtc_final1.6$RKM[dtc_final1.6$station_name == "Mantua Creek Anchorage Buoy B"] <- 148
dtc_final1.6$Region[dtc_final1.6$station_name == "LL# 3000 Delaware River Buoy 56"] <- "River"
dtc_final1.6$RKM[dtc_final1.6$station_name == "LL# 3000 Delaware River Buoy 56"] <- 138
dtc_final1.6$Region[dtc_final1.6$station_name == "DCR-Cooling Water Channel-Outer"] <- "River"
dtc_final1.6$RKM[dtc_final1.6$station_name == "WEDGEPORT"] <- -1000
dtc_final1.6$RKM[dtc_final1.6$station_name == "202102_COX02"] <- -400
dtc_final1.6$Region[dtc_final1.6$station_name == "WEDGEPORT"] <- "Atlantic"
dtc_final1.6$Region[dtc_final1.6$station_name == "202102_COX02"] <- "Atlantic"
dtc_final1.6$Region[dtc_final1.6$station_name == "1"] <- "Brandywine 1"
dtc_final1.6$Region[dtc_final1.6$station_name == "2"] <- "Brandywine 2"
dtc_final1.6$Region[dtc_final1.6$station_name == "Christina River"] <- "Brandywine 3"

dtc_final1.6$RKM[dtc_final1.6$Region == "Brandywine 1"] <- 121
dtc_final1.6$RKM[dtc_final1.6$Region == "Brandywine 2"] <- 118.5
dtc_final1.6$RKM[dtc_final1.6$Region == "Brandywine 3"] <- 116.5
######################## False detections and removal of dead fish ############################
#dtc_final2 <- false_detections(dtc_final1.6, tf=43200, show_plot = TRUE)
dtc_final2 <- false_detections(dtc_final1.6, tf=10800)
#dtc_final2 <- false_detections(dtc_final1.6, tf=3600)
dtc_final2 <- dtc_final2 %>%
  filter(passed_filter == 1) %>%
  filter(!animal_id %in% c(19,17,14,23)) # REMOVING FOUR SHAD FROM ANALYSIS :)
                               
######################## Duplication Check of Data ################################
DUPES <- dtc_final2 %>%
  distinct(detection_timestamp_utc, animal_id, station_name, .keep_all = T) %>%
  group_by(detection_timestamp_utc, animal_id) %>%
  mutate(FREQ = n()) %>%
  filter(FREQ > 1) %>%
  select(detection_timestamp_utc, Year, animal_id, station_name, Latitude, Longitude, RKM, Region, Owner, FREQ) %>%
  mutate(avg.RKM = mean(RKM)) %>%
  ungroup() 



ggplot(DUPES, aes(x = detection_timestamp_utc, y=factor(FREQ), color = factor(animal_id)))+
  geom_jitter(alpha=.5, height = .1)+
  facet_wrap(~Year, scales = "free")+
  labs(y = "Frequency of Timestamp occuring")

ggplot(DUPES, aes(x = factor(animal_id), y=factor(station_name), color = (detection_timestamp_utc)))+
  geom_jitter(alpha=.5, height = .1)


DUPES2 <- dtc_final2[!duplicated(dtc_final2),] %>%
  group_by(detection_timestamp_utc, animal_id, station_name) %>%
  mutate(FREQ = n()) %>%
  filter(FREQ > 1) %>%
  select(detection_timestamp_utc, Year, animal_id, station_name, Latitude, Longitude, RKM, Region, Owner, FREQ)


##################  Map of all Receivers  ##################################

# Subset based on our brandywine stations
EH<-sampling_stations %>%
  filter(Owner == "EH")

unique(full.stations$StationName)

## Map all receivers
ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +
  geom_point(data = full.stations, aes(y= Y, x= X ), 
             size = 3, alpha= .15) +
  geom_point(data = full.stations, aes(y= Y, x= X ), 
             size = 3, shape= 1) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = c(-76.2, -74.6), ylim = c(38.5, 40.4))+
  # scale_color_manual("Sampling Location",values=c("red","blue"))+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_text_repel(data = EH, aes(x = X, y = Y, label = glatos_array), 
                  fontface = "bold",
                  nudge_x = c(-.2, .2),
                  nudge_y = c(0.15, -0.15)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(breaks = seq(38.5, 41.0, 0.5)) +
  scale_x_continuous(breaks = seq(from = -76.5, to = -74.5, by=0.5)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#efe3f3",
                               "pennsylvania" = "#f3e7e3", "maryland" = "#e7f3e3"))

ggsave("SamplingStations1.png",
       plot = last_plot(),
       width = 5,
       height = 7,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")




ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +
  geom_point(data = full.stations, aes(y= Y, x= X, color= Region ), 
             size = 3, alpha= .3) +
  geom_point(data = full.stations, aes(y= Y, x= X, color = Region ), 
             size = 3, shape= 1) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = c(-76.2, -74.6), ylim = c(38.5, 40.4))+
  scale_color_manual("Region",values=colpal)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  # geom_text_repel(data = full.stations, aes(x = X, y = Y, label = StationName), 
  #                 fontface = "bold",
  #                 nudge_x = c(-.2, .2),
  #                 nudge_y = c(0.15, -0.15)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "#EAEAEB"),
        # legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(breaks = seq(38.5, 41.0, 0.5)) +
  scale_x_continuous(breaks = seq(from = -76.5, to = -74.5, by=0.5)) +
  scale_fill_manual(values = c("delaware" = "white", "new jersey" = "white",
                               "pennsylvania" = "white", "maryland" = "white"))+
  guides(fill="none")

ggsave("Receivers3.jpeg",
       plot = last_plot(),
       width = 5,
       height = 7,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")


ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color = "black", linewidth = .05) +
  geom_point(data = full.stations, aes(y= Y, x= X, color= Region ), 
             size = 3, alpha= .3) +
  geom_point(data = full.stations, aes(y= Y, x= X, color = Region ), 
             size = 3, shape= 1) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = c(-76.2, -65), ylim = c(38.5, 45))+
  scale_color_manual("Region",values=colpal)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  # geom_text_repel(data = full.stations, aes(x = X, y = Y, label = StationName), 
  #                 fontface = "bold",
  #                 nudge_x = c(-.2, .2),
  #                 nudge_y = c(0.15, -0.15)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "#EAEAEB"),
        # legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(breaks = seq(38.5, 45.0, 0.5)) +
  scale_x_continuous(breaks = seq(from = -76.5, to = -65, by=1)) +
  scale_fill_manual(values = c("delaware" = "white", "new jersey" = "white",
                               "pennsylvania" = "white", "maryland" = "white",
                               "new york" = "white", "massachusetts" = "white",
                               "connecticut" = "white", "rhode island"= "white",
                               "maine" = "white", "new hampshire" = "white",
                               "vermont" = "white"))+
  guides(fill="none") 


ggsave("Receivers2.jpeg",
       plot = last_plot(),
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")


################ Map of transmitter pings ######################################
#install.packages("sf")
#install.packages("devtools")

# Find total detections at each location 
df <- dtc_final2 %>%
  group_by(Transmitter, station_name) %>%
  mutate(n.detects =  n())

WaywardSon <- df %>% filter(Transmitter == "A69-9007-15557") #& Year == "2022")

ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +
   coord_sf(xlim = c(-76.00, -74.9), ylim = c(38.7, 40)) +  
  #coord_sf(xlim = c(-76.00, -65.00), ylim = c(38, 45)) +
  geom_point(data = df, aes(x= Longitude, y=Latitude, color= Year, alpha = .1),size=3) +
 # facet_wrap(~Transmitter)+
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(title = "2021 - 2022 detections for Shad in DE and MATOS array")+ 
  # geom_text_repel(data = transmitter_station_detects, aes(x = Longtitude, y = Latitude, label = Station.Name), 
  # fontface = "bold", nudge_x = c(-.2, .2), nudge_y = c(0.15, -0.15)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
 # scale_y_continuous(breaks = seq(38.7, 40, 0.5)) +
  #scale_x_continuous(breaks = seq(from = -76.00, to = -74.9, by=0.5)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3"))+
  guides(alpha = "none", fill = "none") 



ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +
  coord_sf(xlim = c(-76.00, -74.9), ylim = c(38.7, 40)) +  
  #coord_sf(xlim = c(-76.00, -65.00), ylim = c(38, 45)) +
  geom_point(data = df, aes(x= Longitude, y=Latitude, color= station_name, alpha = .1),size=3) +
  # facet_wrap(~Transmitter)+
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  labs(title = "")+ 
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1),
        plot.background = element_blank()) +
  scale_fill_manual(values = c("delaware" = "White", "new jersey" = "White",
                               "pennsylvania" = "White", "maryland" = "White",
                               "new york" = "White", "rhode island" = "White",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3"))+
  guides(alpha = "none", fill = "none") 

ggsave("MappyBoi.jpeg",
       plot = last_plot(),
       width = 4,
       height =7,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")


# trying  to see all fish at once on da map -- target the fish that look sketch (14,17,23,24)

WaywardSon <- dtc_final2 %>% filter(Transmitter == "A69-9007-15557") #& Year == "2022")

ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +
  coord_sf(xlim = c(-76.00, -74.9), ylim = c(38.7, 40)) +  
  #coord_sf(xlim = c(-76.00, -65.00), ylim = c(38, 45)) +
  geom_point(data = dtc_final2, aes(x= Longitude, y=Latitude, color= detection_timestamp_utc, alpha = .1),size=3) +
  facet_grid(~Transmitter)+
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(title = "2021 - 2022 detections for Shad in DE and MATOS array")+ 
  # geom_text_repel(data = transmitter_station_detects, aes(x = Longtitude, y = Latitude, label = Station.Name), 
  # fontface = "bold", nudge_x = c(-.2, .2), nudge_y = c(0.15, -0.15)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  # scale_y_continuous(breaks = seq(38.7, 40, 0.5)) +
  #scale_x_continuous(breaks = seq(from = -76.00, to = -74.9, by=0.5)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3"))+
  guides(alpha = "none", fill = "none") 


################ Animation of Map ##############################################
AnimationMap <- ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color = "black", linewidth = .05) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 1,], aes(y= Latitude, x= Longitude, color= Region ), 
             size = 5, alpha= .3) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 1,], aes(y= Latitude, x= Longitude, color = Region ), 
             size = 5, shape= 1) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = c(-76.00, -65.00), ylim = c(38, 45))+
  scale_color_manual("Region",values=colpal)+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "#EAEAEB"),
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "white", "new jersey" = "white",
                               "pennsylvania" = "white", "maryland" = "white",
                               "new york" = "white", "massachusetts" = "white",
                               "connecticut" = "white", "rhode island"= "white",
                               "maine" = "white", "new hampshire" = "white",
                               "vermont" = "white", "virginia" = "white"))+
  guides(fill="none") +
  labs(title = "", 
       subtitle = 'Date: {previous_state}', x = '', y = '') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.2, alpha = FALSE)


animate(AnimationMap, duration = 20, fps = 5, width = 900, height = 800, renderer = gifski_renderer())
anim_save("AnimationMapFish1.gif")

################ Shape file Hell ################################################
#Practice building maps using shapefiles

install.packages("terra")
library(sp)
library(sf)
library(terra)
library(ggplot2)
library(rgdal)
NorthAmerica <- readOGR("PoliticalBoundaries_Shapefiles_NA_PoliticalDivisions_data_bound_l_boundary_l_v2.shp")

ggplot() + 
  geom_polygon(data = NorthAmerica, aes(x = long, y = lat, group = group), 
               colour = "black", fill = NA)


### ok trying shapefiles again

install.packages("raster")
library(raster)
Maracoos <- system.file("MARACOOS_2021_Area_Updated.shp", package = "raster")
Maracoos
shapefile(Maracoos)

###

library(rgdal)
my_shapefile <- readOGR("C:/Users/RER/Documents/Masters UD/MARACOOS_Area_2021-Shapefile", "MARACOOS_2021_Area_Updated")

st_read("MARACOOS_2021_Area_Updated.shp")

df<- st_read(system.file("C:/Users/RER/Documents/Masters UD/MARACOOS_Area_2021/MARACOOS_Area_2021-Shapefile/MARACOOS_2021_Area_Updated.shp"))

df<- read_sf(dsn = "C:/Users/RER/Documents/Masters UD", layer= "MARACOOS_Area_2021-Shapefile")

### Ben's Help
library(raster)
USA <- getData('GADM', country="USA", level=1)
USA %>%
  filter(USA$Name_1 == "Delaware")
DE <- USA[USA$NAME_1=="Delaware",]
plot(DE)
ggplot(data = DE) +
  geom_sf(data = DE) +
  coord_sf(xlim = c(-76.00, -74.9), ylim = c(38.7, 40)) 
  #coord_sf(xlim = c(-76.00, -65.00), ylim = c(38, 45)) +
  geom_point(data = SpecialFish, aes(x= Longitude, y=Latitude, color= Year, alpha = .1),size=3) +
  # facet_wrap(~Transmitter)+
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+

setwd("~/Masters UD/gshhg-shp-2.3.7/gshhg-shp-2.3.7/GSHHS_shp/i")
library(sf)
my_spdf<-data.frame(st_read("GSHHS_i_L1.shp"))


library(broom)

spdf_fortified <- tidy(my_spdf, region = "id")

# Plot it
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 



#################### Network Analysis ##########################################

#Make a data frame of edges (To and From) using lag of station list, in order of time
#Careful, need to restart R when there is an issue with select() fxn
edge_list<-data.frame(dtc_final2) %>% 
  arrange(animal_id, detection_timestamp_utc) %>% 
  group_by(Sex) %>%
  filter(!station_name== "") %>%
 # filter(animal_id == 3 & !station_name== "") %>%
  mutate(Last.Station = lag(station_name)) %>%
  select(station_name, Last.Station) %>%
  rename(To = station_name, From = Last.Station) %>%
  drop_na()


#create number of observations for node/link weight (by fish id, and not by fish id)
edge_list_weight <- edge_list %>% 
  group_by_all %>% 
  count

#calculating total detects for each sex - not that important
tot <- data.frame(tapply(edge_list_weight$n, INDEX = edge_list_weight$Sex, FUN = sum)) %>% 
  rename(total.detects = 1)
# male + female =/= total observations in dtc_final2 dataframe....we've lost 100 obs
sum(tot$total)
length(dtc_final2$common_name)

#Create weighted edge list by specific grouped columns 
edge_list_weight <- edge_list %>% 
  group_by(From, To)%>%
  count()

# Create a node list for each unique station name
node_list <- as.data.frame(unique(dtc_final2$station_name)) %>% 
  rename(Stations = "unique(dtc_final2$station_name)") %>%
  filter(!Stations == "") %>%
  mutate(id = 0:42)
#44

#PLot matrices of data to get a good look before network analysis
ggplot(edge_list_weight, aes(x=From, y=To, fill=log10(n)))+
  geom_tile()+
  scale_fill_gradient(low="pink", high="blue")+
 #scale_fill_viridis(discrete=FALSE, option = "magma")+
 # facet_wrap(~Sex)+
  theme_bw()+
  labs(fill="Detects (log10)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size =8, hjust=.95, color ="black"),
        axis.text.y = element_text(color="black", size =8))

  

# Create network using network() fxn
shad_network <- network(edge_list_weight,
        node_list,
        matrix.type = "edgelist",
        loops = TRUE,
        multiple =TRUE,
        ignore.eval = FALSE)
plot(shad_network, vertex.cex = 3, mode = "circle")

#create network using graph_from_data_frame() fxn (igraph package)
#Cannot have sex column in edge_list df, must only be matrix
shad_igraph <- graph_from_data_frame(d = edge_list_weight,
                                       vertices = node_list,
                                       directed = TRUE)
plot(shad_igraph,
     layout = layout_nicely,
     vertex.size = 10,
     vertex.label.cex = 0.5,
     edge.arrow.size = 0.8,
     vertex.label = NA,
     width=edge_list_weight$n)

ggsave("Network.jpeg",plot = last_plot(),
       width = 7,
       height = 5,
        units = "in",
        dpi = 300,
     path = "C:/Users/RER/Documents/Masters UD/Code")  

plot(full.graph, 
     edge.arrow.size=.05, 
     arrow.mode=1, 
     edge.width=full.network$moves$Freq,
     vertex.size=degree(full.graph, mode="all"), 
     vertex.label=NA, 
     vertex.color="lightseagreen",
     layout=l)

#create network with tbl_graph() fxn 
shad_tidy <- tbl_graph(nodes = node_list,
                         edges = edge_list_weight,
                         directed = TRUE)
# plot with ggraph
ggraph(shad_tidy) + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()
# plot again
ggraph(shad_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.8, aes(width=n)) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = Stations), repel = TRUE) +
  #labs(edge_width = "Letters") +
  theme_graph()
# plot again again
ggraph(shad_tidy, layout = "linear") + 
  geom_edge_arc(aes(width=n), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = Stations), angle = 45, hjust = 0) +
  theme_graph()


####   Must create an adjacency matrix for latitude and longitude, can use this in igraph specifications ###
## Mantel test attempt
edge_list2 <- data.frame(dtc_final2) %>% 
  arrange(animal_id, detection_timestamp_utc) %>% 
  group_by(Sex) %>%
  filter(!station_name== "") %>%
  mutate(Last.Station = lag(station_name)) %>%
#  select(station_name, Last.Station) %>%
  rename(To = station_name, From = Last.Station) %>%
 # drop_na() %>%
   ungroup() %>%
  distinct(To, Latitude, Longitude) %>% 
  left_join(edge_list, ., by = c("From" = "To"), suffix = c(".To", ".From")) #%>%
 # mutate(x.dist = Longitude.To - Longitude.From, y.dist = Latitude.To - Latitude.From) %>%
  #select(x.dist, y.dist) %>%
  #drop_na()
  #This spit out double the observations?

adj.mtx <- edge_list %>%
  ungroup() %>%
  select(From, To)


####  You can do this by year too (2021 vs 2022 movements)   ###

m <- edge_list %>%
  filter(Sex == "Male") %>%
  ungroup() %>%
  select(From, To)

f <- edge_list %>%
  filter(Sex == "Female") %>%
  ungroup() %>%
  select(From, To)

adj.mtx.m <-vegdist(get.adjacency(graph_from_data_frame(m,directed = FALSE),sparse = FALSE))
adj.mtx.f <-vegdist(get.adjacency(graph_from_data_frame(f,directed = FALSE),sparse = FALSE))
class(adj.mtx.m)

mantel(xdis = adj.mtx.m, ydis = adj.mtx.f, method = "spearman", permutations=999)

##################### All detects graph #################################

#poss.morts <- dtc_final2 %>% filter(animal_id == c("23","24", "14", "17"))
# Lets make a graph!!
ggplot(dtc_final2, aes(x=detection_timestamp_utc, y=animal_id, color=Region,
                      shape = Sex, alpha=.2))+
  geom_jitter(size=4, height = .15)+
  labs(title = "26 Tagged American shad (2021 and 2022) at all stations",
       y= "Animal ID", x="Detection Timestamp (UTC)")+
  scale_y_continuous(breaks = seq(1,26,2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) +
  guides(alpha = "none", color=guide_legend(title="Tag insertion \nmethod")) +
  facet_wrap(~Year, scales = "free") +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")), 
             colour = "red") +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")+survival_gap), 
             colour = "black") +
  #annotate("text", x=as.POSIXct(as.Date("2022-05-10 10:00:00")), y=7, 
  #label="2022 Deployment", angle=90) +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")), 
             colour = "red") +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")+survival_gap), 
             colour = "black") 

length(falses)


dtc_final2 %>%
  filter(
         Year == 2021,
         !Region == "Atlantic") %>%
  ggplot(aes(x=detection_timestamp_utc, y=RKM, 
                                                 color=Region, alpha=.2))+
  geom_point(size=2, alpha = .15)+
  geom_point(size =2, shape = 1)+
  labs(title = "", y= "River Kilometer", x="Detection Timestamp (UTC)")+
  facet_wrap(~animal_id) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  guides(alpha = "none", color=guide_legend(title="Region")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")), 
             colour = "red") +
  #geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")+survival_gap), colour = "black") +
  scale_color_manual(values=colpal)

ggsave("2021Detects4.jpeg",
       plot = last_plot(),
       width = 11,
       height = 7,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")

dtc_final2 %>%
  filter(animal_id %in% c(1,7),
         Year == 2021) %>%
  ggplot(aes(x=detection_timestamp_utc, y=RKM, 
             color=Region, alpha=.2))+
  geom_point(size=2, alpha = .15)+
  geom_point(size =2, shape = 1)+
  labs(title = "", y= "River Kilometer", x="Detection Timestamp (UTC)")+
  facet_wrap(~animal_id) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  guides(alpha = "none", color=guide_legend(title="Region")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")), 
             colour = "red") +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")+survival_gap), 
             colour = "black") +
  scale_color_manual(values=colpal)

ggsave("2021Fish1+7.jpeg",
       plot = last_plot(),
       width = 7,
       height = 4,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")


dtc_final2 %>%
  filter(animal_id %in% c(1,7, 5,10),
         Year == 2021,
         RKM > 0) %>%
  ggplot(aes(x=detection_timestamp_utc, y=RKM, 
             color=Region, alpha=.2))+
  geom_point(size=3, alpha = .15)+
  geom_point(size =3, shape = 1)+
  labs(title = "", y= "River Kilometer", x="Detection Timestamp (UTC)")+
  facet_grid(~animal_id) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust =1)) +
  guides(alpha = "none", color=guide_legend(title="Region")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")), 
             colour = "red") +
  scale_color_manual(values=colpal)

ggsave("2021Fish1+7+5+10.jpeg",
       plot = last_plot(),
       width = 10,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")







dtc_final2 %>%
  filter(animal_id %in% c(1,13, 15,22),
         Year == 2022,
         RKM > 0) %>%
  ggplot(aes(x=detection_timestamp_utc, y=RKM, 
             color=Region, alpha=.2))+
  geom_point(size=3, alpha = .15)+
  geom_point(size =3, shape = 1)+
  labs(title = "", y= "River Kilometer", x="Detection Timestamp (UTC)")+
  facet_grid(~animal_id) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust =1)) +
  guides(alpha = "none", color=guide_legend(title="Region")) +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")), 
             colour = "red") +
  scale_color_manual(values=colpal)

ggsave("2022Fish1+13+15+22.jpeg",
       plot = last_plot(),
       width = 10,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")






ggplot(dtc_final2[dtc_final2$Year == 2022,], aes(x=detection_timestamp_utc, y=RKM, color=Region,
                                                 alpha=.2))+
  geom_point(size=2, alpha = .15)+
  geom_point(size =2, shape = 1)+
  labs(title = "",
       y= "River Kilometer", x="Detection Timestamp (UTC)")+
  facet_wrap(~animal_id, scales = "free") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) +
  guides(alpha = "none", color=guide_legend(title="Region")) +
  facet_wrap(~animal_id)+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")), colour = "red") +
  #geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")+survival_gap), colour = "black")+
  scale_color_manual(values=colpal)

ggsave("2022Detects3.jpeg",
       plot = last_plot(),
       width = 11,
       height = 7,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")

length ()
#6,11,23,17,
ggplot(dtc_final2[!dtc_final2$animal_id %in% c(14,19, 23),], aes(x=Region, group=animal_id))+
  geom_bar(aes(fill=factor(animal_id)))+
  facet_wrap(~Year)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "", x = "", y = "Number of detections", fill = "Animal ID")

ggsave("Detections.jpeg",
       plot = last_plot(),
       width = 10,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")  

dtc_final2 %>% filter(animal_id == "23")


dtc_final2 %>%
  filter(animal_id %in% c(7,22,24),
         RKM > 0,
         Year == 2021) %>%
ggplot(aes(x=detection_timestamp_utc, y=RKM, color=Region))+
  geom_point(size=3, alpha = .15)+
  geom_point(size =3, shape = 1) +
  labs(title = "", y= "River Kilometer", x="Detection Timestamp (UTC)")+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust =1)) +
  guides(alpha = "none", color=guide_legend(title="Region")) +
  facet_wrap(~animal_id) +
  scale_color_manual(values=colpal)

ggsave("FB2021_1.jpeg",
       plot = last_plot(),
       width = 4.5,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")  

dtc_final2 %>%
  filter(animal_id %in% c(7,22,24),
         RKM > 0,
         Year == 2022) %>%
  ggplot(aes(x=detection_timestamp_utc, y=RKM, color=Region))+
  geom_point(size=3, alpha = .15)+
  geom_point(size =3, shape = 1) +
  labs(title = "", y= "River Kilometer", x="Detection Timestamp (UTC)")+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust =1)) +
  guides(alpha = "none", color=guide_legend(title="Region")) +
  facet_wrap(~animal_id) +
  scale_color_manual(values=colpal)

ggsave("FB2022_1.jpeg",
       plot = last_plot(),
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")  


  # geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")), 
  #            colour = "red") +
  # geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")+as.duration(hms("24:00:00"))), 
  #            colour = "black") +
  # geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")), 
  #            colour = "red") +
  # geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00", tz = UTC)+as.duration(hms("24:00:00"))), 
  #            colour = "black") 

###############  Accelerometer Data  ##############

# Filter for sensors that contain motion data
ac.df <- dtc_final2 %>% 
  filter(transmitter_codespace == "A69-9007") %>% 
  drop_na(sensor_value)

# whats the maximum speed value? 3.5 m/s^2
max(ac.df$sensor_value)

ggplot(ac.df, aes(x =detection_timestamp_utc, y= sensor_value, color = transmitter_id))+
  geom_point(size = 2)+
  geom_line()+
  facet_grid(transmitter_id~Year, scales = "free") +
  theme_bw() +
  labs(x = "Time", y = "Accelerometer speed (m/s^2)", color = "Transmitter ID") 

ggplot(ac.df, aes(x= detection_timestamp_utc, y= station_name, color=transmitter_id)) + 
  geom_point() +
  facet_wrap(~Year, scales = "free") +
  theme_bw() 

# Lets try to find the distance between two points, but its not real because river
exp1 <- data.frame(ac.df) %>% 
  filter(Year == "2021", animal_id == "1") %>%
  arrange(animal_id, detection_timestamp_utc) %>% 
  mutate(Last.Station = lag(station_name), Last.time = lag(detection_timestamp_utc)) %>%
  #  select(station_name, Last.Station) %>%
  rename(To = station_name, From = Last.Station) 
exp2 <- exp1 %>%
  ungroup() %>%
  distinct(To, Latitude, Longitude) %>% 
  left_join(exp1, ., by = c("From" = "To"), suffix = c(".To", ".From")) %>%
  arrange(detection_timestamp_utc) %>%
  mutate(x.dist = Longitude.To - Longitude.From, 
        y.dist = Latitude.To - Latitude.From,
        time.diff = abs(Last.time - detection_timestamp_utc),
        hypot.km = sqrt((x.dist*111.320*cos(Longitude.To)) ^2 + (y.dist*110.574) ^2),
        velocity.s.m = (hypot.km*1000)/(as.numeric(time.diff))) %>%
#  filter(velocity.s.m > 0 & !!all_vars(is.finite(velocity.s.m))) %>%
  #dplyr::select(Longitude.To, Longitude.From, Latitude.To, Latitude.From) %>%
  mutate(geoDist = distHaversine(cbind(Longitude.To, Latitude.To), 
                             cbind(Longitude.From,Latitude.From)))%>%
  mutate(velocity.m.s = geoDist/as.numeric(time.diff)) %>%
  filter(velocity.m.s > 0 & !!all_vars(is.finite(velocity.m.s)))


ggplot(exp2, aes(x=detection_timestamp_utc, y= velocity.m.s))+
  geom_point(aes(color=To), size =3, alpha= .7) +
  labs(x = "Detection Timestamp", y = "Sustained Velocity (m/s)", 
       title = "A69-9007-15557 in 2021") 
  scale_y_continuous(limits = c(0,0.1), breaks = seq(0,0.1,by=0.01))
 # scale_y_continuous(limits = c(0,0.1), breaks = seq(0,0.1,by=0.01))

# There is something wrong happening here lol (for year == 2021)


distHaversine(c(39.42734,
                -75.52499),
              c(38.83563,
                -75.07794))
imgonnacry <- dtc_final2 %>%
  arrange(animal_id, detection_timestamp_utc)

  library(geosphere)
  dmatrix <- exp2 %>%
    dplyr::select(Longitude.To, Longitude.From, Latitude.To, Latitude.From)


dist <- distm(cbind(dmatrix$Longitude.To, dmatrix$Latitude.To), 
              cbind(dmatrix$Longitude.From,dmatrix$Latitude.From), 
              fun = distHaversine)
?distm

# #IDEK anymore
# st.ID <- c(1:length(unique(ac.df$station_name)))
# st.df <- data.frame(cbind(st.ID, unique(ac.df$station_name))) 
# 
# AH <- left_join(ac.df, st.df, by = c(station_name == V2))


dtc_final2 %>%
  filter(animal_id %in% c(1,2)) %>%
  filter(!is.na(sensor_value)) %>%
  ggplot(aes(x=detection_timestamp_utc, y=sensor_value))+
  geom_point(alpha=.5)+
  facet_wrap(animal_id~Year, scales = "free")+
  theme_bw()+
  ylab(bquote(m/s^2)) +
  labs(title = "", x="")+
  scale_color_manual(values = colpal) +
  theme(axis.text.x = element_text(size = 16, hjust =1, angle = 45),
        text = element_text(size=20)) 


ggsave("AccelerometerData3.jpeg",
       plot = last_plot(),
       width = 10,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code") 

dtc_final2 %>%
  filter(animal_id %in% c(1,2)) %>%
  filter(!is.na(sensor_value)) %>% 
  ggplot(aes(x = Region, y = sensor_value)) +
#  geom_boxplot() +
  geom_violin(draw_quantiles = .5)+
  geom_jitter(width = .05, alpha = .5, color = "red")+
  theme_bw() +
  ylab(bquote(m/s^2)) +
  labs(title = "", x="") +
  facet_grid(animal_id~Year)


###############   Residency and STATS  ############


# Prepare data set for GLATOS syntax
detects <- dtc_final2 %>% rename("deploy_lat" ="Latitude", "deploy_long" = "Longitude")


# Read dataset as "detection Events" as a function in GLATOS
residency <- detection_events(detects, location_col = "Region", time_sep = Inf, condense = TRUE)

residency <- residency %>% mutate (res_time_min = res_time_sec/60, 
                                   res_time_hr = round(res_time_min/60,4))


# Redo with new region simplification
detects2 <- dtc_final2 %>%
  mutate(Region2 = recode(Region, "Brandywine 1" = "Brandywine", "Brandywine 2" = "Brandywine",
                         "Brandywine 3" = "Brandywine")) %>%
  mutate(Region3 = recode(Region2, "Canal" = "Other", "Atlantic" = "Other", "Bay" = "Other")) %>%
  rename("deploy_lat" ="Latitude", "deploy_long" = "Longitude")

residency3 <- detection_events(detects2, location_col = "Region2", time_sep = Inf, condense = TRUE)

residency4 <- residency3 %>% mutate (res_time_min = res_time_sec/60, 
                                   res_time_hr = round(res_time_min/60,4),
                                   res_time_hr_log = log(res_time_hr))



# Compress all detections into unique detection events (BY REGION)
residency5 <- detection_events(detects2, location_col = "Region3", 
                               time_sep = Inf, condense = TRUE)

# Make time column into hours. Also add a log transform (after shapiro test)
residency6 <- residency5 %>% mutate (res_time_min = res_time_sec/60, 
                                     res_time_hr = round(res_time_min/60,4),
                                     res_time_hr_log = log(res_time_hr))


# Testing Normality across Regions
shapiro.test(residency6$res_time_hr[residency6$location=="Brandywine"])
# NOT normal
shapiro.test(residency6$res_time_hr[residency6$location=="Other"])
# NOT normal
shapiro.test(residency6$res_time_hr[residency6$location=="River"])
# NOT normal


# Testing Normality across Regions using log transformed data
shapiro.test(residency6$res_time_hr_log[residency6$location=="Brandywine"])
# NORMAL
shapiro.test(residency6$res_time_hr_log[residency6$location=="Other"])
# NORMAL
shapiro.test(residency6$res_time_hr_log[residency6$location=="River"])
# NORMAL. PROCEED

# Levene test for equal variance using log transformed data
levene.test(residency6$res_time_hr_log, group = residency6$location)
# UNEQUAL VARIANCE. Proceed with non-parametric test.


# Kruskal Wallis test
kruskal.test(res_time_hr_log ~ location, data=residency6)
# Significant

# Dunn post hoc test to determine between group significance 
dunnTest(res_time_hr_log ~ location, data=residency6, method="bonferroni")
# barely signifcant between Brandywine and 'Other' Region





   
kruskal.test(res_time_hr_log ~ location, data=residency6)
dunnTest(res_time_hr_log ~ location, data=residency6, method="bonferroni")

   

residency %>%
  mutate(Year = lubridate::year(first_detection)) %>%
  ggplot(aes(x=location, y=res_time_hr))+
  geom_boxplot(color = "black") +
  geom_jitter(width = .1, alpha = .1, color = "red")+
  theme_bw() +
  labs(title = "", x= "", y= "Residency (hour)")+
  #facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

ggsave("Residency3.jpeg",
       plot = last_plot(),
       width = 6,
       height = 6,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code") 


residency2 %>%
  group_by(location3) %>%
  summarise(mean = mean(res_time_hr),
            sd = sd(res_time_hr))


residency4 %>%
  mutate(Year = lubridate::year(first_detection)) %>%
  ggplot(aes(x=factor(location, levels = c("Canal", "Atlantic","Bay","River", "Brandywine")), y=res_time_hr_log))+
  geom_boxplot(color = "black") +
  geom_jitter(width = .1, alpha = .1, color = "red")+
  theme_bw() +
  labs(title = "", x= "", y= "Residency (log hour)")+
  #facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

residency4 %>%
  group_by(location) %>%
  summarise(mean = mean(res_time_hr),
            sd = sd(res_time_hr))

ggsave("ResidencyX.jpeg",
       plot = last_plot(),
       width = 6,
       height = 6,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code") 



residency6 %>%
  mutate(Year = lubridate::year(first_detection)) %>%
  ggplot(aes(x=factor(location, levels = c("River", "Other", "Brandywine")), y=res_time_hr_log))+
  geom_boxplot(color = "black") +
  geom_jitter(width = .1, alpha = .1, color = "red")+
  theme_bw() +
  labs(title = "", x= "", y= "Residency (log hour)")+
  #facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        text = element_text(size = 20))

residency6 %>%
  group_by(location) %>%
  summarise(mean = mean(res_time_hr),
            sd = sd(res_time_hr))


ggsave("Residency6.jpeg",
       plot = last_plot(),
       width = 6,
       height = 6,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code") 




# Calculate residence index
RI_dtc <- residence_index(residency, calculation_method ="kessel")
Kessel <- RI_dtc %>% group_by(animal_id) %>% summarise(RI_tot = sum(residency_index))

ggplot(RI_dtc, aes(x = factor(animal_id), y=residency_index, group = location, color = location))+
  geom_jitter(width=.1, alpha = .5) +
  #geom_bar(stat = "identity", position = "stack", width = .9)+
  theme_bw() 

RI_dtc_TI <- residence_index(residency, calculation_method ="time_interval")
time_int <- RI_dtc_TI %>% group_by(animal_id) %>% summarise(RI_tot = sum(residency_index))

RI_dtc_TD <- residence_index(residency, calculation_method ="timedelta")
time_delta <- RI_dtc_TD %>% group_by(animal_id) %>% summarise(RI_tot = sum(residency_index))

RI_dtc_AG <- residence_index(residency, calculation_method ="aggregate_with_overlap")
Ag <- RI_dtc_AG %>% group_by(animal_id) %>% summarise(RI_tot = sum(residency_index))

# plot all 4 residence indexes
ggplot(Kessel, aes(x=factor(animal_id), y=RI_tot))+
  geom_col(color = "black")+
  geom_col(data = time_int, aes(x=animal_id, y=RI_tot), color = "purple")+
  geom_col(data = time_delta, aes(x=animal_id, y=RI_tot), color = "red")+
  geom_col(data = Ag, aes(x=animal_id, y=RI_tot), color = "green")


# Make an abacus plot... what the fuck is an abacus plot?
arg<-detects %>% filter(animal_id == "1" & Year == "2021") 
abacus_plot(arg, location_col = "station_name", type = "b")


residency2 %>%
  group_by(animal_id) %>%
  summarise(sum = sum(res_time_hr)) %>%
  ggplot(aes(x=factor(animal_id), y= sum))+
  geom_point()


############### Statistics V ########################  


############## T test for fish length and sex ratio  ############

# AVerage Lengths per year and sex
fsh %>%
  group_by(Deploy.Year) %>%
  summarise(TL.avg = mean(Length.m),
            TL.SD = sd(Length.m))
fsh %>%
  group_by(Sex) %>%
  summarise(TL.avg.s = mean(Length.m),
            TL.SD.s = sd(Length.m))

## Testing across years
# Test fish  for normality          # We meet normality for both years!                     
shapiro.test(fsh$Length.m[fsh$Deploy.Year==2021])
shapiro.test(fsh$Length.m[fsh$Deploy.Year==2022])
# Test for differences in variance across years     # Variances are equal too. 
levene.test(fsh$Length.m, group = fsh$Deploy.Year)
# Time for a t test to test in length differences across years
# No difference in length between years :)
t.test(fsh$Length.m[fsh$Deploy.Year==2021], fsh$Length.m[fsh$Deploy.Year==2022])

## Testing across Sexes
# Test fish lengths for normality       # We meet normality for both sexes                   
shapiro.test(fsh$Length.m[fsh$Sex== "Male"])
shapiro.test(fsh$Length.m[fsh$Sex== "Female"])
# Test for differences in variance across sexes   # Variances are equal too. 
levene.test(fsh$Length.m, group = fsh$Sex)
# Time for a t test to test in length differences across Sex
t.test(fsh$Length.m[fsh$Sex== "Male"], fsh$Length.m[fsh$Sex=="Female"])
# Hmm........ females do be larger than males...hmmm

# LEts see this bishhh
ggplot(fsh, aes(x=Deploy.Year,y=Length.m))+
  geom_boxplot(notch = TRUE)+
  theme_classic() +
  labs(x = "Tagging Year", y="Length (m)")

ggsave("LengthbyYear.png", plot = last_plot(), width = 3, height = 3,
       units = "in", dpi = 300, path = "C:/Users/RER/Documents/Masters UD/Code")

# LEts see this bishhh
ggplot(fsh, aes(x=Sex,y=Length.m))+
  geom_boxplot(notch = TRUE)+
  theme_classic() +
  labs(X = "Sex", y="Length (m)")

ggsave("LengthbySex.png", plot = last_plot(), width = 3, height = 3,
       units = "in", dpi = 300, path = "C:/Users/RER/Documents/Masters UD/Code")


############## Chi square for tagging skew  ############
fsh %>% group_by(Deploy.Year) %>% count(Sex)

fish.matrix <- matrix(c(6,5,6,9),2)
rownames(fish.matrix) <- c("2021", "2022")
colnames(fish.matrix) <- c("Male", "Female")

## Tagging skew?
chisq.test(fish.matrix)
mosaic(fish.matrix)

## Do this in ggplot please
df <- fsh %>%
  group_by(Deploy.Year, Sex) %>%
  summarise(count = n()) %>%
  mutate(Sex.count = sum(count),
         prop = count/sum(count)) %>%
  ungroup()
# OK
ggplot(df, aes(x = Deploy.Year, y = prop, width = Sex.count, fill = Sex)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  facet_grid(~Deploy.Year, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_void() 






## Death Skew?
death <- matrix(c(10,1,10,5),2)
rownames(death) <- c("Survived", "Died")
colnames(death) <- c("Male", "Female")

chisq.test(death)
epitools::oddsratio(death)
mosaic(t(death))

## post tagging mortality
mort <- matrix(c(10,2,12,4),2)
rownames(mort) <- c("Survived", "Died")
colnames(mort) <- c("2021", "2022")

chisq.test(mort)
epitools::oddsratio(mort)
mosaic(mort)

##
 

animal_counts <- dtc_final2 %>%
  group_by(station_name, animal_id) %>%
  count() 

############## Kruskal Wallis For residency Hours  ############
# Testing Normality across Regions
shapiro.test(residency$res_time_hr[residency$location=="Atlantic"])
# Atlantic violates normality (Needs more observations)
shapiro.test(residency$res_time_hr[residency$location=="Canal"])
# So does Canal (Needs more observations)
shapiro.test(residency$res_time_hr[residency$location=="River"])
# non normal
shapiro.test(residency$res_time_hr[residency$location== "Bay"])
# Also non normal
shapiro.test(residency$res_time_hr[residency$location %in% c("Bay", "Canal", "Atlantic")])
# When grouped together, still not normal.

#Lets make a staked histrogram cause fuck me in the da butt
ggplot(residency, aes(x = res_time_hr)) +
  geom_histogram(fill = "dodgerblue", color = "black",
                 closed = "left", boundary = 0, binwidth = 20) +
  facet_wrap(~location, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_bw()

# New hist with log trans
ggplot(residency, aes(x = log(res_time_hr))) +
  geom_histogram(fill = "dodgerblue", color = "black",
                 closed = "left", boundary = 0, binwidth = 1) +
  facet_wrap(~location, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_bw()


# Ok lets log transform into a new dataframe and remove infinite values
residency2 <- residency %>%
  mutate(res_time_hr_log = log(res_time_hr)) %>%
  filter(!is.infinite(res_time_hr_log)) %>%
  mutate(location2 = recode(location, "Brandywine 1" = "Brandywine", "Brandywine 2" = "Brandywine",
                            "Brandywine 3" = "Brandywine"),
         location3 = recode(location2, "Canal" = "Other", "Atlantic" = "Other", "Bay" = "Other"))

# Ok lets try this again
shapiro.test(residency2$res_time_hr_log[residency2$location %in% c("Bay", "Canal", "Atlantic")])
# Yes! Normal! Suck it data
shapiro.test(residency2$res_time_hr_log[residency2$location == "Brandywine 1"])
# Nope - not normal
shapiro.test(residency2$res_time_hr_log[residency2$location == "Brandywine 3"])
# Nope - not normal
shapiro.test(residency2$res_time_hr_log[residency2$location == "Brandywine 2"])
# NORMAL!
shapiro.test(residency2$res_time_hr_log[residency2$location == "River"])
# Normal!
shapiro.test(residency2$res_time_hr_log[residency$location %in% c("Brandywine 1", "Brandywine 2", "Brandywine 3")])
# Not # fucking # normal

# We move onto non-parametric tests across groups for a Kruskal - Wallis test
kruskal.test(res_time_hr_log ~ location2, data=residency2)
# Significant across regions

kruskal.test(res_time_hr_log ~ location, data=residency2)
# Significant across regions

kruskal.test(res_time_hr_log ~ location3, data=residency2)
# Significant across regions

brandywine_res <- residency2 %>%
  filter(location %in% c("Brandywine 1", "Brandywine 2", "Brandywine 3"))

kruskal.test(res_time_hr_log ~ location, data = brandywine_res)
# Significant 

# Lets run some Dunn post hoc tests to see which groups are diffrent from the others
dunnTest(res_time_hr_log ~ location2, data=residency2, method="bonferroni")
# Bay and River and Brandywine and River are different from each other
dunnTest(res_time_hr_log ~ location, data=brandywine_res, method="bonferroni")
# All three BRandywines are different from each other
dunnTest(res_time_hr_log ~ location3, data=residency2, method="bonferroni")
# This is the one!! 


# levene test for variance equality? 

residency2 %>%
  group_by(location3) %>%
  summarise(mean = mean(res_time_hr),
            sd = sd(res_time_hr))


residency2 %>%
  mutate(Year = lubridate::year(first_detection)) %>%
  ggplot(aes(x=factor(location2, levels = c("Canal", "Atlantic","Bay","River", "Brandywine")), y=res_time_hr_log))+
  geom_boxplot(color = "black") +
  geom_jitter(width = .1, alpha = .1, color = "red")+
  theme_bw() +
  labs(title = "", x= "", y= "Residency (log hour)")+
  #facet_wrap(~Year) +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

ggsave("Residency4.jpeg",
       plot = last_plot(),
       width = 6,
       height = 6,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code") 


########### Each fish graphs ##############

world <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

### Fish 1 ###
residency %>%
  filter(animal_id == 1, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish One Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 1) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= RKM))+
  geom_line() +
  geom_point(aes(color = Region), size = 3, alpha= .5)+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "River Kilometer", title = "Fish One Dections Over Time") +
  scale_color_manual(values = colpal)


ggsave("Fishy1.jpeg",
       plot = last_plot(),
       width = 11,
       height = 7,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")



FishOne <- ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -65.00), ylim = c(38, 45)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 1,], aes(x= Longitude, y=Latitude, color= Year, 
                                     group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none") +
  labs(title = "2021 - 2022 detections for Fish One ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(FishOne, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("FishOne.gif")


### Fish 2 ###
residency %>%
  filter(animal_id == 2, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Two Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 2) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Two Dections Over Time")

FishTwo <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 2,], aes(x= Longitude, y=Latitude, color= Year, 
                                    group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish Two ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(FishTwo, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("FishTwo.gif")


### Fish 3 ###
residency %>%
  filter(animal_id == 3, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Three Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 3) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Three Dections Over Time")

FishThree <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 3,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish Three ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(FishThree, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("FishThree.gif")



### Fish 4 ###
residency %>%
  filter(animal_id == 4, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Four Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 4) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Four Dections Over Time")

FishFour <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 4,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish Four ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(FishFour, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("FishFour.gif")


### Fish 5 ###
residency %>%
  filter(animal_id == 5, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Five Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 5) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Five Dections Over Time")

FishFive <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 5,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish Five ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(FishFive, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("FishFive.gif")



### Fish 6 ###
residency %>%
  filter(animal_id == 6, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Six Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 6) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Six Dections Over Time")




### Fish 7 ###

residency %>%
  filter(animal_id == 7, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Seven Residence Times \nZero Hrs Omitted")+
  facet_zoom(ylim = c(0, 20))

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 7) %>%
  ggplot(aes(detection_timestamp_utc, y= RKM))+
  geom_point(aes(color = Region), size =2 )+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Seven Dections Over Time")+
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")), 
             colour = "red") +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-05-11 10:00:00")+ as.duration(hms("48:00:00"))), 
             colour = "black") 

FishSeven <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 7,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish Seven ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(FishSeven, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("FishSeven.gif")


### Fish 8 ###
residency %>%
  filter(animal_id == 8, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Eight Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 8) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Eight Dections Over Time")




### Fish 9 ###
residency %>%
  filter(animal_id == 9, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Nine Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 9) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Nine Dections Over Time")



### Fish 10 ###
residency %>%
  filter(animal_id == 10, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Ten Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 10) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Ten Dections Over Time")



### Fish 11 ###
residency %>%
  filter(animal_id == 11, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Eleven Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 11) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_jitter()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Eleven Dections Over Time")

length(unique(dtc_final2 %>% filter(animal_id == 11)))

### Fish 12 ###
residency %>%
  filter(animal_id == 12, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish Twelve Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 12) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish Twelve Dections Over Time")


Fish12 <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 12,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish Twelve ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(Fish12, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("Fish12.gif")



### Fish 13 ###
residency %>%
  filter(animal_id == 13, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 13 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 13) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 13 Dections Over Time")




### Fish 14 ###
residency %>%
  filter(animal_id == 14, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 14 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 14) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_jitter()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 14 Dections Over Time")




### Fish 15 ###
residency %>%
  filter(animal_id == 15, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 15 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 15) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 15 Dections Over Time")

Fish15 <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 15,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                 group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish 15 ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(Fish15, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("Fish15.gif")



### Fish 16 ###
residency %>%
  filter(animal_id == 16, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 16 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 16) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 16 Dections Over Time")



### Fish 17 ###
residency %>%
  filter(animal_id == 17, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 17 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 17) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 17 Dections Over Time")



### Fish 18 ###
residency %>%
  filter(animal_id == 18, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 18 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 19) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 19 Dections Over Time")


### Fish 19 ###


### Fish 20 ###
residency %>%
  filter(animal_id == 20, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 20 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 20) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 20 Dections Over Time")



### Fish 21 ###
residency %>%
  filter(animal_id == 21, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 21 Residence Times \nZero Hrs Omitted")

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 21) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 21 Dections Over Time")

Fish21 <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 21,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                 group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish 21 ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(Fish21, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("Fish21.gif")



### Fish 22 ###
residency %>%
  filter(animal_id == 22, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 22 Residence Times \nZero Hrs Omitted") +
  facet_zoom(ylim = c(0, 20))

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 22) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 22 Dections Over Time")

Fish22 <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 22,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                 group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish 22 ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(Fish22, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("Fish22.gif")


### Fish 23 ###
residency %>%
  filter(animal_id == 23, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 23 Residence Times \nZero Hrs Omitted") 

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final1.6 %>%
  filter(animal_id == 23) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= station_name))+
  geom_jitter()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 23 Dections Over Time")




 ### Fish 24 ###
residency %>%
  filter(animal_id == 24, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 24 Residence Times \nZero Hrs Omitted") 

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 24) %>%
  group_by(RKM) %>%
  arrange(RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(RKM), color = Region))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 24 Dections Over Time")

Fish24 <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 24,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                 group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish 24 ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(Fish24, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("Fish24.gif")




### Fish 25 ###
residency %>%
  filter(animal_id == 25, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 25 Residence Times \nZero Hrs Omitted") 

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 25) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 25 Dections Over Time")



### Fish 26 ###
residency %>%
  filter(animal_id == 26, res_time_hr != 0) %>%
  ggplot(aes(x = location, y= res_time_hr, color = last_detection))+
  geom_jitter()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x = "", y = "Residence time (hr)", title = "Fish 26 Residence Times \nZero Hrs Omitted") 

dtc_ordered <- dtc_final2[order(dtc_final2$RKM),]

dtc_final2$station_name <- factor(dtc_final2$station_name, levels=dtc_final2$RKM)

dtc_final2 %>%
  filter(animal_id == 26) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= factor(station_name)))+
  geom_point()+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "", title = "Fish 26 Dections Over Time")

Fish26 <-  ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$animal_id == 26,], aes(x= Longitude, y=Latitude, color= Year, 
                                                                 group= detection_timestamp_utc), size = 5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "Detections for Fish 26 ", 
       subtitle = 'Date: {previous_state}', x = 'Longitude', y = 'Latitude') +
  transition_states(states = detection_timestamp_utc,
                    state_length=1) +
  ease_aes('quartic-in-out') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(Fish26, duration = 20, fps = 5, width = 800, height = 900, renderer = gifski_renderer())
anim_save("Fish26.gif")





######################################
ggplot(data = world) +
  geom_sf(data = states, aes(fill = ID)) +  
  coord_sf(xlim = c(-76.00, -74), ylim = c(38, 40.5)) +
  geom_point(data = dtc_final2[dtc_final2$station_name == "LL# 2995 Delaware River Lighted Buoy 29",], aes(x= Longitude, y=Latitude, color= Year, 
                                                                 group= detection_timestamp_utc), size = 3, color = "red") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        #legend.position = "none",
        axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c("delaware" = "#f3f1e3", "new jersey" = "#f3f1e3",
                               "pennsylvania" = "#f3f1e3", "maryland" = "#f3f1e3",
                               "new york" = "#f3f1e3", "rhode island" = "#f3f1e3",
                               "massachusetts" = "#f3f1e3", "connecticut" = "#f3f1e3",
                               "maine" = "#f3f1e3", "new hampshire" = "#f3f1e3",
                               "vermont" = "#f3f1e3", "virginia" = "#f3f1e3")) +
  guides(alpha = "none", fill = "none", color = "none") +
  labs(title = "LL# 2995 Delaware River Lighted Buoy 29")

view(fsh)
length(dtc_final2$common_name)
       




write.csv(full.stations, "C:/Users/RER/Documents/Masters UD/Code/Stations.csv")



########
dtc_final2 %>% filter(animal_id == 17) %>%
  ggplot(aes(x=detection_timestamp_utc, y = RKM))+
#  geom_jitter(alpha=.5)+
  geom_point(size=4, alpha=.2, aes(color = Region))+
  geom_line()+
  geom_vline(xintercept = as.POSIXct(as.Date("2022-05-10 10:00:00")+as.duration(hms("24:00:00"))), 
             colour = "black") 
  facet_wrap(~Year, scales = "free")

############### Movement Rate/ Total Distance Moved/ Time at large #############
testing <- dtc_final2 %>%
  group_by(animal_id) %>%
  arrange(detection_timestamp_utc, .by_group = TRUE) %>%
  mutate(Mvmt.m = (abs(RKM - lag(RKM)))*1000,
         Time.diff = as.numeric(detection_timestamp_utc - lag(detection_timestamp_utc)),
         Move_rate = Mvmt.m/Time.diff,
         last.station = lag(station_name)) 
  
testing %>%
  ungroup() %>%
  filter(!is.infinite(Move_rate),
         !is.na(Move_rate)) %>%
  summarise(avg.rate = mean(Move_rate),
            sd.rate = sd(Move_rate))

testing %>%
  group_by(station_name) %>%
  filter(is.infinite(Move_rate)) %>%
  count

ggplot(testing, aes(x=animal_id, y= Move_rate, color = Region), alpha = .5)+
  geom_jitter(width = .15) +
  labs(y = "Movement Rate (m/s)", x = "Animal ID") +
  theme_bw()


# Total distance Traveled 
testing2 <- testing %>%
  group_by(animal_id) %>%
  filter(!is.na(Mvmt.m)) %>%
  filter(!is.infinite(Move_rate)) %>%
  # Time at large
  summarise(Total.move.km = sum(Mvmt.m)/1000,
            Time.At.Large = as.numeric(as.duration(max(detection_timestamp_utc) - min(detection_timestamp_utc))/604800),
            avg.rate = mean(Move_rate, na.rm = T)) 


testing2 %>%
  pivot_longer(!animal_id, names_to = "Variable", values_to = "Value") %>%
  filter(Variable == "Time.At.Large") %>%
  mutate(Variable = recode(Variable, "Time.At.Large" = 'Time at large',
                           "Total.move.km" = "Total movement")) %>%
  ggplot(aes(x=factor(Variable), y=Value))+
  geom_boxplot()+
  geom_jitter(alpha=.3, size = 3, color = "red", width = .1)+
  theme_bw()+
  labs(x="", y="Weeks") +
  theme(text=element_text(size = 20))

ggsave("Time.at.large.png",
       plot = last_plot(),
       width = 3,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")

testing2 %>%
  pivot_longer(!animal_id, names_to = "Variable", values_to = "Value") %>%
  filter(Variable == "Total.move.km") %>%
  mutate(Variable = recode(Variable, "Time.At.Large" = 'Time at large',
                           "Total.move.km" = "Total movement")) %>%
  ggplot(aes(x=factor(Variable), y=Value))+
  geom_boxplot()+
  geom_jitter(alpha=.3, size = 3, color = "red", width = .1)+
  theme_bw()+
  labs(x="", y="Kilometers") +
  theme(text=element_text(size = 20))


ggsave("Total.mvmt.png",
       plot = last_plot(),
       width = 3,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")






testing2 %>%
  summarise(mean.time = mean(Time.At.Large/604800),
            sd.time = sd(Time.At.Large/604800),
            mean.move = mean(Total.move.km),
            sd.move = sd(Total.move.km))


# Graph time moved and distance moved
ggplot(testing2, aes(y=Total.move.km, x=Time.At.Large/(604800), color = factor(animal_id)))+
  geom_point(size = 3, alpha = .5)+
  labs(x= "Time at Large (weeks)", y = "Total Movement (Km)", color = "Shad ID")+
  theme_bw()+
  stat_smooth(method = "lm", level = 0, color = "grey")


ggsave("TimevsMove.png",
       plot = last_plot(),
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")


summary(lm(Total.move.km ~ Time.At.Large, data = testing2))

cor(testing2$Time.At.Large, testing2$Total.move.km)
  
rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(testing2$Time.At.Large, testing2$Total.move.km)
#[1] 0.8560185


################# Fixing the RKM and movement ####################

Locale <- unique(dtc_final2 %>%
  mutate(Locale = paste(station_name, RKM)) %>%
  select(Locale))

BR <- c("BR1", "BR2", "BR3")

as.matrix(Locale, Locale)


data.frame(Locale, "BR1", "BR2", "BR3")

BR1= c(0, 2.5, 4.5, 10, 10, 13, 15, 11, )

triangle <- dtc_final2 %>%
  group_by(station_name) %>%
  distinct(station_name, .keep_all = T) %>%
  mutate(Mouth = abs(RKM - 113)) %>%
  select(station_name, Mouth) %>%
  mutate(BR1 = 8 + Mouth,
         BR2 = 5.5 + Mouth,
         BR3 = 3.5 + Mouth)

triangle2 <- triangle[-c(1,2,3),]

deargod <- triangle2 %>% ungroup () %>%
  add_row(station_name = "BR1", Mouth= 8, BR1 = 0, BR2 = 2.5, BR3 = 4.5) %>%
  add_row(station_name = "BR2", Mouth= 5.5, BR1 = 2.5, BR2 = 0, BR3 = 2) %>%
  add_row(station_name = "BR3", Mouth= 3.5, BR1 = 4.5, BR2 = 2, BR3 = 0)


data.matrix(deargod)


# defining data in the vector
x <- c(1:1849)

# defining row names and column names
rown <- c("row_1", "row_2", "row_3")
coln <- c("col_1", "col_2", "col_3", "col_4")

# creating matrix
m <- matrix(x, nrow = 43, byrow = TRUE, 
            dimnames = list(Locale, Locale))
Locale <- as.(Locale)
length(Locale)
# print matrix
print(m)

# print class of m
class(m)

# Get a 43 x 43 matrix
# fill in distance for all
# fix three diagonals with BR1 - Br3 by hand
# Compress matrix to df
# rename columns as "from" and "to"
# left join with the testing dataframe
# recalcualte distacnes swam

############################
dtc_final2 %>%
  filter(animal_id == 1) %>%
  ggplot(aes(x= detection_timestamp_utc, y=RKM))+
  geom_point()

RI <- dtc_final2 %>%
  filter(animal_id == 1, RKM == -400) %>%
  filter(detection_timestamp_utc == min(detection_timestamp_utc)) %>%
  select(detection_timestamp_utc)
  
bay <- dtc_final2 %>%
  filter(animal_id == 1, RKM == 0) %>%
  filter(detection_timestamp_utc == max(detection_timestamp_utc)) %>%
  select(detection_timestamp_utc)

(400*1000)/((as.numeric(RI - bay))*(24*60*60))






dtc_final2 %>%
  filter(animal_id == 17) %>%
  group_by(RKM) %>%
  arrange(-RKM) %>%
  ggplot(aes(detection_timestamp_utc, y= RKM))+
  geom_line() +
  geom_point(aes(color = Region), size = 3, alpha= .5)+
  facet_wrap(~Year, scales = "free") +
  theme_bw() +
  labs(x = "", y = "River Kilometer", title = "") +
  scale_color_manual(values = colpal)


ggsave("Fishy17.jpeg",
       plot = last_plot(),
       width = 11,
       height = 7,
       units = "in",
       dpi = 300,
       path = "C:/Users/RER/Documents/Masters UD/Code")
