library(tidyverse)
library(scales)
library(RColorBrewer)

# Read data with nearest stops per respondent
raw_stops <- read.csv("//its-rds/2017/tightmr-transport-walking/Spatial/Home_based_analysis/Homes_50busstops.csv")
unique_stops <- unique(raw_stops$stop_id)

# Read GTFS data
data_stops <- read.table("//its-rds/2017/tightmr-transport-walking/Spatial/TfWM/tfwm_gtfs/stops.txt", header = T, sep = ",")
data_calendar <- read.table("//its-rds/2017/tightmr-transport-walking/Spatial/TfWM/tfwm_gtfs/calendar.txt", header = T, sep = ",")
data_routes <- read.table("//its-rds/2017/tightmr-transport-walking/Spatial/TfWM/tfwm_gtfs/routes.txt", header = T, sep = ",")
data_trips <- read.table("//its-rds/2017/tightmr-transport-walking/Spatial/TfWM/tfwm_gtfs/trips.txt", header = T, sep = ",")
data_trips$trip_id <- as.character(data_trips$trip_id)
data_stoptimes <- read.table("//its-rds/2017/tightmr-transport-walking/Spatial/TfWM/tfwm_gtfs/stop_times.txt", header = T, sep = ",")
data_stoptimes$trip_id <- as.character(data_stoptimes$trip_id)

# Get the number of buses to stop on Mondays
services <- data_calendar$service_id[data_calendar$monday==1]

trips <- data_trips$trip_id[data_trips$service_id %in% services]
routes <- data_trips$route_id[data_trips$service_id %in% services]

stop_freq <- data.frame(stop_id = character(0), n=numeric(0), r=numeric(0))

for(s in unique_stops){
  n <- data_stoptimes %>% 
    filter(stop_id == s,
           trip_id %in% trips) 
  t <- data_trips %>%
    filter(trip_id %in% n$trip_id)
  stop_freq <- rbind(stop_freq, c(s, nrow(n), length(unique(t$route_id))))
}
colnames(stop_freq) <- c("stop_id", "n", "routes")

# Join the frequency of the stop with the nearest_stop table
nearest_stops <- left_join(raw_stops, stop_freq)


# Calculate some measures
nearest_stops %>%
  filter(n >= 6) %>% group_by(RespondentID) %>% 
  summarise(min = min(Total_Length), max = max(Total_Length), avg = mean(Total_Length), n()) %>% View()

nearest_stops %>%
  filter(n >= 6) %>% group_by(RespondentID) %>% 
  summarise(min = min(Total_Length)) %>% 
  mutate(neighbourhood = ifelse(RespondentID < 2000, "Moseley", 
                                 ifelse(RespondentID < 3000, "Bournville", 
                                        ifelse(RespondentID < 4000, "Castle Vale", "Small Heath")))) %>% 
  filter(neighbourhood %in% c("Moseley", "Bournville")) %>%
  ggplot(aes(min))+
  geom_histogram() +
  facet_wrap(~neighbourhood) +
  labs(x = "Minimum distance (m)",
       y = "Count") +
  theme_bw()

# Calculate a convenience score per bus stop per user
nearest_stops <- nearest_stops %>%
  mutate(frequency = ifelse(n <= 36, 1, 
                            ifelse(n <= 96, 2,
                                   ifelse(n <= 192, 3, 4))),
         distance = ifelse(Total_Length < 200, 4,
                           ifelse(Total_Length < 500, 3,
                                  ifelse(Total_Length < 800, 2, 1))),
         convenience = ifelse(frequency == 4 & distance == 4, 5,
                              ifelse(frequency == 4 & distance == 3, 4,
                              ifelse(frequency == 4 & distance == 2, 3,
                              ifelse(frequency == 4 & distance == 1, 2,
                              ifelse(frequency == 3 & distance == 4, 5,
                              ifelse(frequency == 3 & distance == 3, 3,
                              ifelse(frequency == 3 & distance == 2, 3,
                              ifelse(frequency == 3 & distance == 1, 2,
                              ifelse(frequency == 2 & distance == 4, 4,
                              ifelse(frequency == 2 & distance == 3, 3, 
                              ifelse(frequency == 2 & distance == 2, 2,
                              ifelse(frequency == 1 & distance == 4, 3,
                              ifelse(frequency == 1 & distance == 3, 2, 1))))))))))))),
         neighbourhood = ifelse(RespondentID < 2000, "Moseley",
                                ifelse(RespondentID < 3000, "Bournville",
                                ifelse(RespondentID < 4000, "Castle Vale", "Small Heath")))
         )



write.csv(nearest_stops, "Z:/Spatial/BusDepartures/nearest_stops.csv", row.names = F)

# Histogram of convenience distribution
nearest_stops %>%
  filter(n >= 6) %>%
  ggplot(aes(convenience)) +
  geom_histogram(binwidth = 1)

# List of best stops
best_stops <- nearest_stops %>%
  filter(n >= 6, Total_Length <= 1000) %>%
  group_by(RespondentID) %>%
  filter(convenience == max(convenience)) %>% 
  filter(routes == max(routes)) %>% 
  filter(Total_Length == min(Total_Length))
  
write_csv(best_stops, "Z:/Spatial/BusDepartures/best_stops.csv")


# Overview of convenience levels of bus stops within 1000m
nearest_stops %>%
  filter(n >= 6) %>%
  mutate(convenience = factor(convenience))%>%
  ggplot(aes(x = neighbourhood, fill = convenience)) +
  geom_bar(position = "fill")  +
  scale_y_continuous(labels = scales::percent)

# Overview of convenience levels of "best" bus stops within 1000m
best_stops %>%
  mutate(convenience = factor(convenience))%>%
  ggplot(aes(x = neighbourhood, fill = convenience)) +
    geom_bar(position = "fill")  +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=brewer.pal(5, "Blues"), name="Convenience score")

# Overview of walk distances to "best" bus stops within 1000m
best_stops %>%
  ggplot(aes(x = neighbourhood, y = Total_Length)) +
  geom_boxplot()

