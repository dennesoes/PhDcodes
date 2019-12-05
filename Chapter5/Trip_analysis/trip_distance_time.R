#Calculate the distance and time of each trip
trip_distance_time <- function(data_trips, data_tracks){
  library(geosphere)
  
  # Initialise variables
  data_trips$distance <- NA
  data_trips$time <- NA
  data_trips$time_start <- NA
  data_trips$time_finish <- NA
  data_trips$date <- NA
  data_trips$day <- NA
  
  #Calculate length of a trip (distance and time)
  for(i in 1:nrow(data_trips)){
    from_id <- data_trips$from_id[i]
    to_id <- data_trips$to_id[i]

    if(data_trips$from_incomplete[i]==0 & 
       data_trips$to_incomplete[i]==0 & 
       sum(data_tracks$id==to_id)!=0 & 
       sum(data_tracks$id==from_id)!=0){
      
      #list all tracking points between origin and destination
      route <- data_tracks[data_tracks$id %in% c(from_id:to_id),c("X","Y")]
      
      #calculate distance between all points
      if(nrow(route)>2){ #route needs to contain more than 2 points to calculate distGeo
        data_trips$distance[i] <- round(sum(distGeo(route)))
      }
      if(nrow(route)==2){
        data_trips$distance[i] <- round(distGeo(c(as.numeric(route[1,1]),as.numeric(route[1,2])),
                                                c(as.numeric(route[2,1]),as.numeric(route[2,2]))))
      }
      
      #calculate time that the trip took
      time <- (data_tracks$time_[data_tracks$id==to_id]-data_tracks$time_[data_tracks$id==from_id])/1000 #seconds
      data_trips$time[i] <- round(time/60, digits = 2) #minutes
      
      #Start and end times
      data_trips$time_start[i] <- format(as.POSIXct(data_tracks$time_[data_tracks$id==from_id]/1000, 
                                                    tz=Sys.timezone(), origin = "1970-01-01"), "%H:%M:%S")
      data_trips$time_finish[i] <- format(as.POSIXct(data_tracks$time_[data_tracks$id==to_id]/1000, 
                                                     tz=Sys.timezone(), origin = "1970-01-01"), "%H:%M:%S")
      data_trips$date[i] <- format(as.POSIXct(data_tracks$time_[data_tracks$id==from_id]/1000, 
                                              tz=Sys.timezone(), origin = "1970-01-01"), "%d-%m-%Y")
      data_trips$day[i] <- format(as.POSIXct(data_tracks$time_[data_tracks$id==from_id]/1000, 
                                             tz=Sys.timezone(), origin = "1970-01-01"), "%A")
    }
  } 
  data_trips
}
