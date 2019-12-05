# TrackDistTime -----------------------------------------------------------
# Function to calculate distances, speed, time and angle between two GPS coordinates
# Inputs: track_data (file with lonlat locations)
# Outputs: track_data (file with lonlat locations, enriched with new columns)

library(geosphere)
library(dplyr)

TrackDistTime <- function(user_data){
  
  #Calculate the difference in distance and time with the previous location point
  
  user_data %>%
    mutate( 
      # Calculate geographical distance between two lon-lat points and check if this is a finite number
      distance = distGeo(cbind(lag(lon_),lag(lat_)),cbind(lon_, lat_)),
      distance = ifelse(is.finite(distance),distance, NA),
      
      # Calculate difference in time between two points and the average speed and acceleration
      timediff = (time_ - lag(time_))/1000,
      speed_mps = distance / timediff,
      speed_mps = ifelse(is.finite(speed_mps),speed_mps, NA),
      speed_kmph = round(speed_mps * 3.6, digits=1), 
      acceleration = abs((speed_mps - lag(speed_mps))/timediff),
      
      # Convert time stamp to human readable date and time
      POSIXct = as.POSIXct(time_/1000, tz=Sys.timezone(), origin = "1970-01-01"),
      date = format(POSIXct, "%d-%m-%Y"),
      time_BST = format(POSIXct, "%H:%M:%S"),
      
      # Calculate the angle with the preceding and subsequent location point and indicate if it is acute
      bearing1 = bearing(cbind(lon_,lat_), cbind(lag(lon_),lag(lat_))),
      bearing2 = bearing(cbind(lon_,lat_), cbind(lead(lon_),lead(lat_))),
      angle = abs(bearing2 - bearing1),
      angle_acute = if_else(angle <= 15 | angle >= 345, 1, 0)
    )
  
}