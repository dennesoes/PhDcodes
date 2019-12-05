#Plan car trips to obtain another time estimate
#Use car trips table
#Change save file in the end --> Include user number

library(tidyverse)
library(jsonlite)
library(geosphere)

car_trips <- read.csv("Z:/Trackdata/cartrips/_cartrips.csv")

bing_key="xxx"  #Use of Bing Maps API
root <- "http://dev.virtualearth.net/REST/v1/Routes?"

# Create matrix with car routes -------------------------------------------
route_mat <- matrix(nrow = nrow(car_trips), ncol = 6, data = NA)
colnames(route_mat) <- c("tripleg_id", "date", "time", "distance", "traveltime", "timetraffic")

for(i in 1:nrow(car_trips)){
  # Get coordinates of origin and destination location
  tripleg_id <- car_trips$tripleg_id[i]
  date <- car_trips$date[i] %>% as.POSIXlt(format="%d/%m/%Y") %>% format("%Y-%m-%d")
  time <- car_trips$time_start[i] %>% as.POSIXlt(format="%H:%M:%S") %>% format("%H:%M")
  origin_lon <- car_trips$origin_lon[i]
  origin_lat <- car_trips$origin_lat[i]
  destination_lon <- car_trips$destination_lon[i]
  destination_lat <- car_trips$destination_lat[i]
  
  # Compose URL and send request to API
  u <- paste0(root,
              "wp.0=",origin_lat,",",origin_lon,
              "&wp.1=",destination_lat,",",destination_lon,
              "&optimize=timeWithTraffic",
              "&dateTime=",date," ",time,"&key=",bing_key) 
  n <- try(fromJSON(URLencode(u)))
  
  # Fill matrix with information about car routes
  if(!class(n)=="try-error"){ #tests whether there is actually a list of routes
    route <- n$resourceSets$resources[[1]]

    route_mat[i,1] <- tripleg_id
    route_mat[i,2] <- date
    route_mat[i,3] <- time
    route_mat[i,4] <- route$travelDistance
    route_mat[i,5] <- route$travelDuration
    route_mat[i,6] <- route$travelDurationTraffic
  }
}


# Save matrix with info about car routes
filename <- paste0("Z:/Trackdata/cartrips/_car_replanned.csv")
write.csv(route_mat, filename, row.names = F)
