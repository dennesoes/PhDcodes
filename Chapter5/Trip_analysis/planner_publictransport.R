# Plan car trips by public transport

# Load libraries
library(tidyverse)
library(jsonlite)
library(geosphere)

# Load data
data_tracks <- 
  list.files(path = "//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",
             pattern = "^tracks_user(.*)csv$") %>%
  map_df(~read.csv(paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",.x))[,c(-5,-14, -21, -27, -33)]) %>% # Delete columns with logical values due to problems with conversion while binding rows
  mutate(POSIXct = as.POSIXct(POSIXct, tz=Sys.timezone()))

data_trips <- read_csv("Z:/Trackdata/trips/trips.csv")

# Set API keys and variables
app_id="xxx"
app_key="xxx"
by_at="at"
root <- "https://transportapi.com/v3/uk/public/journey/from/"


# Select all car trips ----------------------------------------------------
car_trips <- na.omit(as.data.frame(data_trips[data_trips$mode_main=="Car" & 
                          data_trips$from_incomplete==0 & 
                          data_trips$to_incomplete==0,]))
rownames(car_trips)<-NULL

# Plan public transport trip for each car trip
for(i in nrow(car_trips)){
  # Determine coordinates of origin and destination
  car_trips$origin_lon[car_trips$trip_id==i] <- data_tracks$X[data_tracks$id == car_trips[car_trips$trip_id==i,"from_id"][1]]
  car_trips$origin_lat[car_trips$trip_id==i] <- data_tracks$Y[data_tracks$id == car_trips[car_trips$trip_id==i,"from_id"][1]] 
  car_trips$destination_lon[car_trips$trip_id==i] <- data_tracks$X[data_tracks$id == car_trips[car_trips$trip_id==i,"to_id"][length(car_trips[car_trips$trip_id==i,"to_id"])]]
  car_trips$destination_lat[car_trips$trip_id==i] <- data_tracks$Y[data_tracks$id == car_trips[car_trips$trip_id==i,"to_id"][length(car_trips[car_trips$trip_id==i,"to_id"])]]  

  # Define date and time for trip planning
  nextdays <- seq(Sys.Date(), Sys.Date()+6, by="day") #Determine the next days in the current week
  date <- nextdays[weekdays(nextdays)==car_trips$day[car_trips$trip_id==i][1]] # Determine the date of the next weekday which is the same day as the trip day
  time <- substr(car_trips$time_start[car_trips$trip_id==i][1], 1, 5)
  
  #Compose URL and send request to API
  u <- paste0(root,
              "lonlat:",car_trips$origin_lon[car_trips$trip_id==i][1],",",car_trips$origin_lat[car_trips$trip_id==i][1],
              "/to/lonlat:",car_trips$destination_lon[car_trips$trip_id==i][1],",",car_trips$destination_lat[car_trips$trip_id==i][1],
              "/", by_at,"/",date,"/",time,".json?app_id=",app_id,"&app_key=",app_key) 
  n <- try(fromJSON(URLencode(u))$routes)
  if(!class(n)=="try-error"&length(n)!=0){ #tests whether there is actually a list of routes
    #select first fastest route
    route_number_fast <- min((seq(1:length(n$duration))*(n$duration == min(n$duration)))[n$duration == min(n$duration)])
    d <- n[map_dbl(n$route_parts, nrow)==min(map_dbl(n$route_parts, nrow)),]
    route_number_direct <- as.numeric(row.names(d[d$duration == min(d$duration),])[1])
    
    #calculate duration of the trip
    total_duration_fast <- 60*as.numeric(strsplit(n$duration[route_number_fast], ":")[[1]][1]) + 
      as.numeric(strsplit(n$duration[route_number_fast], ":")[[1]][2])
    total_duration_direct <- 60*as.numeric(strsplit(n$duration[route_number_direct], ":")[[1]][1]) + 
      as.numeric(strsplit(n$duration[route_number_direct], ":")[[1]][2])
    
    #select route parts
    route_parts_fast <- n$route_parts[[route_number_fast]]
    route_parts_direct <- n$route_parts[[route_number_direct]]
    
    #convert duration
    duration_fast <- strsplit(route_parts_fast$duration,":")
    for(j in 1:length(duration_fast)){ #time per stage in seconds
      duration_fast[[j]] <- 3600*as.numeric(duration_fast[[j]][1])+60*as.numeric(duration_fast[[j]][2])+as.numeric(duration_fast[[j]][3])
    }
    route_parts_fast$duration <- unlist(duration_fast)/60 #time in minutes
    
    duration_direct <- strsplit(route_parts_direct$duration,":")
    for(j in 1:length(duration_direct)){ #time per stage in seconds
      duration_direct[[j]] <- 3600*as.numeric(duration_direct[[j]][1])+60*as.numeric(duration_direct[[j]][2])+as.numeric(duration_direct[[j]][3])
    }
    route_parts_direct$duration <- unlist(duration_direct)/60 #time in minutes
    
    
    #Calculate distance and save file with route coordinates for each trip stage
    for(k in 1:length(route_parts_fast$coordinates)){
      #Calculate distance of trip stage
      if(length(route_parts_fast$coordinates[[k]])==4){
        route_parts_fast$distance[[k]] <- sum(distGeo(c(route_parts_fast$coordinates[[k]][1,1],route_parts_fast$coordinates[[k]][1,2]),
                                                 c(route_parts_fast$coordinates[[k]][2,1],route_parts_fast$coordinates[[k]][2,2])))
      }else{
          route_parts_fast$distance[[k]] <- sum(distGeo(route_parts_fast$coordinates[[k]]))
      }
      #save file with coordinates for trip stage
      filename <- paste0("Z:/Trackdata/cartrips/routes/car_pt_",i,"_fast_",k,".csv")
      write.table(route_parts_fast$coordinates[[k]], filename, sep=",", row.names = F, col.names = F)
    }
    
    #save file with trip stage information
    filename <- paste0("Z:/Trackdata/cartrips/car_pt_",i,"_fast.csv")
    write.csv(route_parts_fast[,-9], filename)
    
    
    for(k in 1:length(route_parts_direct$coordinates)){
      #Calculate distance of trip stage
      if(length(route_parts_direct$coordinates[[k]])==4){
        route_parts_direct$distance[[k]] <- sum(distGeo(c(route_parts_direct$coordinates[[k]][1,1],route_parts_direct$coordinates[[k]][1,2]),
                                                      c(route_parts_direct$coordinates[[k]][2,1],route_parts_direct$coordinates[[k]][2,2])))
      }else{
        route_parts_direct$distance[[k]] <- sum(distGeo(route_parts_direct$coordinates[[k]]))
      }
      #save file with coordinates for trip stage
      filename <- paste0("Z:/Trackdata/cartrips/routes/car_pt_",i,"_direct_",k,".csv")
      write.table(route_parts_direct$coordinates[[k]], filename, sep=",", row.names = F, col.names = F)
    }
    
    #save file with trip stage information
    filename <- paste0("Z:/Trackdata/cartrips/car_pt_",i,"_direct.csv")
    write.csv(route_parts_direct[,-9], filename)
    
    
    #Add information about travel alternative to car_trips table (fastest and most direct)
    car_trips$PT_totaltime_fast[car_trips$trip_id==i] <- total_duration_fast
    car_trips$PT_walktime_fast[car_trips$trip_id==i] <- sum(route_parts_fast$duration[route_parts_fast$mode=="foot"])
    car_trips$PT_walkdist_fast[car_trips$trip_id==i] <- sum(route_parts_fast$distance[route_parts_fast$mode=="foot"])
    car_trips$PT_transfers_fast[car_trips$trip_id==i] <- nrow(route_parts_fast[route_parts_fast$mode!="foot",])-1
    car_trips$PT_modes_fast[car_trips$trip_id==i] <- toString(route_parts_fast$mode[route_parts_fast$mode!="foot"])
    
    car_trips$PT_totaltime_direct[car_trips$trip_id==i] <- total_duration_direct
    car_trips$PT_walktime_direct[car_trips$trip_id==i] <- sum(route_parts_direct$duration[route_parts_direct$mode=="foot"])
    car_trips$PT_walkdist_direct[car_trips$trip_id==i] <- sum(route_parts_direct$distance[route_parts_direct$mode=="foot"])
    car_trips$PT_transfers_direct[car_trips$trip_id==i] <- nrow(route_parts_direct[route_parts_direct$mode!="foot",])-1
    car_trips$PT_modes_direct[car_trips$trip_id==i] <- toString(route_parts_direct$mode[route_parts_direct$mode!="foot"])
  }
}

# Write new file
write.csv(car_trips, "Z:/Trackdata/cartrips/_cartrips.csv", row.names = F)

