# Interactive mapping -----------------------------------------------------
# This code can process the tracking data of a user and make it viewable and editable on an interactive map
# The code uses imports 4 other functions

#Set user number to analyse
usernumber <- 22

#Load libraries
library(tidyverse)
library(sp)
library(sf)
library(leaflet)
library(htmltools)
library(mapview)
library(mapedit)

#Load external functions
source("functions/TrackDistTime.R")
source("functions/data_cleaning.R")
source("functions/do_kalman.R")
source("functions/create_map.R")

# Import and clean data ---------------------------------------------------
user <- paste0("tracks_user",usernumber) 
user_data <- paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/",user,".csv") %>%
  read.csv(header=TRUE, sep=",")


#Add information to user_data, filter for unlikely values, apply Kalman filter and recalculate inter-point values
user_data_clean <- user_data %>% 
  TrackDistTime() %>%
  data_cleaning() %>%
  do_kalman() %>%
  TrackDistTime()


#Create simple features object of the Kalman-filtered dataset
coordinates(user_data_clean) <- ~lon_kal+lat_kal
data_sf <- st_as_sf(user_data_clean) %>% st_set_crs(4326)


# Start edit-session ------------------------------------------------------
#Define daynumber to analyse
days <- unique(user_data_clean$date)
day <- days[as.numeric(unlist(strsplit(readline(paste("Which of the",length(days),"days should be analysed?")), ",")))]

# View and edit data:
edits <- create_map(data_sf, day) %>%
  editMap(day)


# Update edited data ------------------------------------------------------
#Delete deleted points
data_sf <- data_sf[!(data_sf$id %in% edits$deleted$layerId),] 

#Update coordinates of edited points
if(length(edits$edited)>0){st_geometry(data_sf[data_sf$id %in% edits$edited$layerId,]) <- st_geometry(edits$edited)}

#Recalculate interpoint values
data_sf <- TrackDistTime(data_sf)


#save edited data for the particular day
filename <- paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/", user, "_", day, ".csv")
st_write(data_sf[data_sf$date==day,], filename,  layer_options = "GEOMETRY=AS_XY", delete_dsn = TRUE)
