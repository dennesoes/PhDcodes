# Merge all personal trip tables into one file and add information 
# (respondent id, trip id, main mode, neighbourhood, attitudinal cluster)

# Load libraries
library(tidyverse)

# Load and merge trip files for all users
trips <-  list.files(path = "//its-rds/2017/tightmr-transport-walking/Trackdata/trips/processed/",
             pattern = "^trips_processed_user(.*)csv$") %>%
  map_df(~read_csv(paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/trips/processed/",.x)))

# Add respondent_id to the table
user_id <- c(1, 2, 3, 4, 5, 15, 22, 23, 24, 25, 26, 
              31, 32, 33, 34, 35, 36, 37, 38, 39, 
              40, 41, 42, 43, 44, 45, 46, 48, 49, 
              50, 51, 52, 53, 54, 56, 57, 
              60, 61, 62, 64, 65, 69, 
              70, 73, 74, 76)
respondent_id <- c(2188, 1230, 1112, 2135, 2188, 1218, 1150, 2174, 1107, 1183, 1110,
                  1147, 1146, 1013, 1041, 1108, 2190, 2073, 2164, 1206,
                  2022, 1129, 2189, 1099, 2141, 1102, 1009, 2222, 1096,
                  2211, 2077, 2145, 1198, 2187, 1015, 1212,
                  1238, 1215, 1227, 1093, 1050, 1024,
                  1231, 2086, 2051, 1196)
users <- data.frame(user_id, respondent_id)
trips <- left_join(trips, users, by="user_id")

# Give unique id to each trip and trip leg
trips$tripleg_id <- seq(1:nrow(trips))

for(i in 1:nrow(trips)){
  if(i==1){
    trips$trip_id[i] <- 1
  }
  else{
    if(trips$chain[i]==1){
      trips$trip_id[i] <- trips$trip_id[i-1]
    }
    else{
      trips$trip_id[i] <- trips$trip_id[i-1]+1
    }
  }
}


# add neighbourhoods and attitudinal clusters
trips$neighbourhood <- NA 
trips$neighbourhood[trips$respondent_id >= 2000] <- "Bournville"
trips$neighbourhood[trips$respondent_id < 2000] <- "Moseley"

hclust_attitude_inclCycle <- read.csv("Z:/Clustering/hclust_attitude_inclCycle.csv")
hclust_attitude_inclCycle <- rename(hclust_attitude_inclCycle, respondent_id="ID", hclust_attitude_inclCycle="cluster")
trips <- left_join(trips, hclust_attitude_inclCycle)


# Define main mode
trips$mode_main <- 0
for(i in unique(trips$trip_id)){
  if(any(!is.na(trips[trips$trip_id==i,"mode"]))){
    if(any(trips[trips$trip_id==i,"mode"] == "Cycle")){
      trips[trips$trip_id==i,"mode_main"] <- "Cycle"
    }
    if(any(trips[trips$trip_id==i,"mode"] == "Car")){
      trips[trips$trip_id==i,"mode_main"] <- "Car"
    }
    if(any(trips[trips$trip_id==i,"mode"] == "Bus")){
      trips[trips$trip_id==i,"mode_main"] <- "Bus"
    }
    if(any(trips[trips$trip_id==i,"mode"] == "Tram")){
      trips[trips$trip_id==i,"mode_main"] <- "Tram"
    }
    if(any(trips[trips$trip_id==i,"mode"] == "Train")){
      trips[trips$trip_id==i,"mode_main"] <- "Train"
    }
  }
}


# Save trips file
write.csv(trips, "Z:/Trackdata/trips/trips.csv", row.names = F)






