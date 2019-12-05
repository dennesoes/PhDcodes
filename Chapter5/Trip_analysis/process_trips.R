# Process the manually annotated trip data using the track files: compute trip distance and time

# Load library
library(tidyverse)

# Set user
user <- 70

# Load data
data_tracks <- 
  list.files(path = "//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",
                pattern = paste0("^tracks_user", user, "_(.*)csv$")) %>%
  map_df(~read.csv(paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",.x))) %>%
  mutate(POSIXct = as.POSIXct(POSIXct, tz=Sys.timezone()))

data_trips <- read_csv(paste0("Z:/Trackdata/trips/trips_user",user,".csv"), col_names = F)
names(data_trips) <- c("tripleg_id", "from_id", "to_id", "mode_id", "dest_id", "from_incomplete", "to_incomplete", "user_id", "chain","origin_id")
data_trips$user_id <- user
modes_table <- read_csv("Z:/Trackdata/trips/modes.csv", col_names = F)
names(modes_table) <- c("mode_id", "mode")
dest_table <- read_csv(paste0("Z:/Trackdata/trips/dest_user",user,".csv"), col_names = F)
names(dest_table) <- c("dest_id", "destination")

#Combine data
data_trips <- data_trips %>%
  filter(user_id == user) %>%
  left_join(modes_table) %>%
  left_join(dest_table, by=c("origin_id"="dest_id")) %>% 
  rename(origin = destination) %>%
  left_join(dest_table) %>%
  select(-mode_id, -dest_id, -origin_id) %>% 
  as.data.frame()


# Calculate distance and time of each trip
source("Trip_analysis/trip_distance_time.R")
data_trips <- trip_distance_time(data_trips, data_tracks)

rownames(data_trips) <- NULL

#Save new trip file
write.csv(data_trips, paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/trips/processed/trips_processed_user",user,".csv"), row.names = F)
