# Analyse the public transport trips and related walks

# Load libraries
library(tidyverse)

# Load data
trips <- read.csv("Z:/Trackdata/trips/trips.csv")

pt_convenience <- read.csv("Z:/Spatial/BusDepartures/convenience_pt.csv")
trips <- left_join(trips, pt_convenience, by = c("respondent_id"="RespondentID"))


# Define and filter the PT trips
pt_triplegs <- as.numeric(unlist(trips[trips$mode %in% c("Bus", "Train", "Tram"),"tripleg_id"]))
pt_trips <- unique(as.numeric(unlist(trips[trips$tripleg_id %in% pt_triplegs, "trip_id"]))) # All trips that involve public transport
pt_trips_df <- trips[trips$trip_id %in% pt_trips,]


# Walk access and egress
# Define which walks are access, egress or transfer walks and whether they are at the home-side
pt_trips_df$access <- F
pt_trips_df$egress <- F
pt_trips_df$transfer <- F
pt_trips_df$homebound <- F
pt_trips_df[pt_trips_df$mode == "Walk"&
              (grepl("Station", pt_trips_df$destination, ignore.case = T) | grepl("Stop", pt_trips_df$destination, ignore.case = T)),
            "access"] <- T
pt_trips_df[pt_trips_df$mode == "Walk"&
              (grepl("Station", pt_trips_df$origin, ignore.case = T) | grepl("Stop", pt_trips_df$origin, ignore.case = T)),
            "egress"] <- T
pt_trips_df[pt_trips_df$access == T & pt_trips_df$egress == T, "transfer"] <- T
pt_trips_df[pt_trips_df$transfer == T, c("access", "egress")] <- F
pt_trips_df[grepl("Home", pt_trips_df$origin, ignore.case = T)|grepl("Home", pt_trips_df$destination, ignore.case = T),"homebound"] <- T
pt_trips_df$captive <- F
pt_trips_df[pt_trips_df$user_id %in% c(4, 51, 52, 74, 76),"captive"] <- T

# Table with summary statistics about walks to/from PT per PT mode
pt_trips_df %>% 
  filter(access == T | egress == T) %>%
  group_by(mode_main, captive) %>%
  summarise("Mean distance" = mean(distance, na.rm=T), 
            "sd distance" = sd(distance, na.rm=T), 
            "Minimum distance" = min(distance, na.rm = T),
            "Maximum distance" = max(distance, na.rm = T),
            "Mean time" = mean(time, na.rm = T), 
            "sd time" = sd(time, na.rm = T),
            "n" = n())

# Scatterplot access vs egress distance
full_trips <- pt_trips_df %>% filter(mode == "Walk", transfer == F, !is.na(distance)) %>% group_by(trip_id) %>% count() %>% filter(n == 2) %>% select(trip_id) %>% unlist() %>% as.numeric()
pt_trips_df %>% filter(mode == "Walk", trip_id %in% full_trips, transfer == F) %>% select(trip_id, distance, access, egress) %>%
  mutate(access_dist = ifelse(access == T, distance, 0), egress_dist = ifelse(egress == T, distance, 0)) %>% group_by(trip_id) %>%
  summarise(access_dist = sum(access_dist), egress_dist = sum(egress_dist)) %>%
  ggplot(aes(access_dist, egress_dist)) +
  geom_point() +
  labs(x = "Access distance", y = "Egress distance") + 
  theme_bw() +
  coord_fixed(xlim = c(0,2000), ylim = c(0, 2000))



# Histograms
hist_data <- pt_trips_df %>% 
  filter(access == T | egress == T) 

# Bus and train cumulative distribution plot
hist_data %>%
  ggplot(aes(x=distance, color = mode_main)) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Bus" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Bus" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data, hist_data$mode_main=="Bus" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Train" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Train" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data,hist_data$mode_main=="Train" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size =1) +
  labs(#title = "Cumulative distribution of walk distances to/from bus and train",
    x="Distance (m)",
    y="Cumulative percentage of walks") +
  geom_vline(xintercept=400, linetype=2) + 
  geom_vline(xintercept=800, linetype=2) +
  theme(legend.title = element_blank())

# Bus and train cumulative distribution plot, per neighbourhood
hist_data %>%
  ggplot(aes(x=distance, color = neighbourhood, linetype = mode_main)) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Moseley"& !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data,hist_data$mode_main=="Train" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size =1) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size =1) +
  labs(#title = "Cumulative distribution of walk distances to/from bus and train",
    x="Distance (m)",
    y="Cumulative percentage of walks") +
  #geom_vline(xintercept=400, linetype=2) + 
  #geom_vline(xintercept=800, linetype=2) +
  theme(legend.title = element_blank())


# Unique PT-walks (from_id of all unique pt-walk triplegs)
unique_ptlegs <- c(32988, 33099, 36831, 37116, 
                   63951, 64163, 77612, 80667, 89708, 90371,
                   7361, 8006, 8459, 
                   10666, 11287, 
                   8014, 
                   54099, 59953,
                   55969, 56444, 
                   9055, 28727, 37230, 37391, 40044, 40321,
                   118222, 118274, 118376, 119256, 119763, 33403, 60011, 81636, 89812, 101783,
                   33111, 33431, 63703, 63865, 56625, 56676,
                   49301, 50835, 52226, 53353, 54882, 54945,
                   108664, 108728, 113610, 113618, 113689, 114059, 114080, 114101, 114302, 114376, 114409, 114644, 115107, 116788, 116825)

hist_data <- pt_trips_df %>% 
  filter(from_id %in% unique_ptlegs) 

hist_data %>%
  filter(cluster_factor6==T) %>% 
  group_by(mode_main) %>%
  summarise("Mean distance" = mean(distance, na.rm=T), 
            "sd distance" = sd(distance, na.rm=T), 
            "Minimum distance" = min(distance, na.rm = T),
            "Maximum distance" = max(distance, na.rm = T),
            "Mean time" = mean(time, na.rm = T), 
            "sd time" = sd(time, na.rm = T),
            "n" = n())

hist_data %>%
  ggplot(aes(x=distance, color = neighbourhood, linetype = mode_main)) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Moseley"& !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data,hist_data$mode_main=="Train" & hist_data$neighbourhood=="Moseley" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size =1) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data, hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data = subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)), 
           aes(y=(nrow(subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))-
                    cumsum(..count..))/nrow(subset(hist_data, hist_data$mode_main=="Train" & hist_data$neighbourhood=="Bournville" & !is.na(hist_data$distance)))*100), 
           binwidth = 1, geom="step", size =1) +
  labs(#title = "Cumulative distribution of walk distances to/from bus and train",
    x="Distance (m)",
    y="Cumulative percentage of walks") +
  #geom_vline(xintercept=400, linetype=2) + 
  #geom_vline(xintercept=800, linetype=2) +
  theme(legend.title = element_blank())

# Test differences
# - bus and train
t.test(hist_data$distance[hist_data$mode_main=="Bus"], hist_data$distance[hist_data$mode_main=="Train"])

# - areas for bus
t.test(hist_data$distance[hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Moseley"],
       hist_data$distance[hist_data$mode_main=="Bus" & hist_data$neighbourhood=="Bournville"])

# - areas for train
t.test(hist_data$distance[hist_data$mode_main=="Train" & hist_data$neighbourhood=="Moseley"],
       hist_data$distance[hist_data$mode_main=="Train" & hist_data$neighbourhood=="Bournville"])


#Clusters based on perceptions / attitudes
hist_data %>%
  group_by(mode_main, homebound) %>%
  summarise("Mean distance" = mean(distance, na.rm=T), 
            "sd distance" = sd(distance, na.rm=T), 
            "Minimum distance" = min(distance, na.rm = T),
            "Maximum distance" = max(distance, na.rm = T),
            "Mean time" = mean(time, na.rm = T), 
            "sd time" = sd(time, na.rm = T),
            "n" = n())

# - Home for bus
t.test(hist_data$distance[hist_data$mode_main=="Bus" & hist_data$homebound==T],
       hist_data$distance[hist_data$mode_main=="Bus" & hist_data$homebound==F])

# - Home for train
t.test(hist_data$distance[hist_data$mode_main=="Train" & hist_data$homebound==T],
       hist_data$distance[hist_data$mode_main=="Train" & hist_data$homebound==F])

# Plot on map
library(leaflet)
library(sp)
library(sf)

data_tracks <- 
  list.files(path = "//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",
             pattern = "^tracks_user(.*)csv$") %>%
  map_df(~read.csv(paste0("//its-rds/2017/tightmr-transport-walking/Trackdata/Location_users/processed/",.x))[,c(-5,-14, -21, -27, -33)]) %>% # Delete columns with logical values due to problems with conversion while binding rows
  mutate(POSIXct = as.POSIXct(POSIXct, tz=Sys.timezone()))

hist_data$lon <- NA
hist_data$lat <- NA
for(i in 1:nrow(hist_data)){
  hist_data$lon[i] <- data_tracks$X[data_tracks$id == hist_data$from_id[i]]
  hist_data$lat[i] <- data_tracks$Y[data_tracks$id == hist_data$from_id[i]]
}
coordinates(hist_data) <- ~lon+lat
map_data <- st_as_sf(hist_data) %>% st_set_crs(4326)

pal <- colorNumeric(palette = c("yellow", "black"), domain = c(0, 1500), na.color = "black")

leaflet(data = map_data) %>%
  addTiles() %>%
  addCircleMarkers(radius = 7, weight = 2, color = "red", fillOpacity = 0.8, fillColor = ~pal(distance))




# Linkplace

linkplace_walks <- read.csv("Z:/Spatial/LinkPlace/linkplace_walks.csv")
pt_trips_df_lp <- left_join(pt_trips_df, linkplace_walks)

pt_trips_df_lp %>%
  filter(mode == "Walk") %>%
  ggplot(aes(index, distance)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

cor.test(~ index + distance, pt_trips_df_lp, method = "pearson") #significance of relation between pleasantness index and distance

#With time
pt_trips_df_lp %>%
  filter(mode == "Walk") %>%
  ggplot(aes(index, distance)) +
  geom_point(aes(colour = ((substr(time_start, 1, 2) == 20 & (substr(time_start, 4, 5) >= 30)) | (substr(time_start, 1, 2) >= 21)))) + 
  geom_smooth(method = "lm") +
  theme_bw()


# Nearest stops
distance_stops <- read.csv("Z:/Spatial/BusDepartures/distance_closest_stop.csv")
pt_trips_df_d <- pt_trips_df %>% left_join(distance_stops, by=c("respondent_id" = "RespondentID"))

pt_trips_df_d %>%
  filter(access == T | egress == T, mode_main == "Bus") %>%
  group_by(respondent_id) %>%
  summarise(min(distance_closest))
  
  
  