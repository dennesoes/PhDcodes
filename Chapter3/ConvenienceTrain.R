library(tidyverse)

# Read data with nearest stops per respondent
raw_stops <- read.csv("//its-rds/2017/tightmr-transport-walking/Spatial/Home_based_analysis/Homes_5stations.csv")
raw_stops <- raw_stops %>% separate(Name, into = c("Name", "Station"), sep = " - ")
nearest_stops <- raw_stops %>%
  mutate(neighbourhood = ifelse(RespondentID < 2000, "Moseley",
                                ifelse(RespondentID < 3000, "Bournville",
                                       ifelse(RespondentID < 4000, "Castle Vale", "Small Heath")))
  )

best_station_convenience <- nearest_stops %>%
  filter(FacilityRank == 1) %>% 
  mutate(Convenience = ifelse(Station %in% c("Bournville", "Kings Norton") & Total_Length < 400, 4,
                          ifelse(Station %in% c("Bournville", "Kings Norton") & Total_Length < 800, 3,
                              ifelse(Station %in% c("Bournville", "Kings Norton") & Total_Length < 1500, 2,
                                  ifelse(Station %in% c("Bournville", "Kings Norton") & Total_Length > 1500, 1,
                          ifelse(Station == "Small Heath" & Total_Length < 400, 3,
                              ifelse(Station == "Small Heath" & Total_Length < 800, 2,
                                  ifelse(Station == "Small Heath" & Total_Length < 1500, 2,
                                      ifelse(Station == "Small Heath" & Total_Length > 1500, 1, NA))))))))
         )
best_station_convenience %>% select(RespondentID, Total_Length, Convenience) %>% write.csv("Z:/Spatial/BusDepartures/best_stations.csv", row.names = F)



# List of distances to closest stations
nearest_stops %>%
  filter(FacilityRank == 1) %>% 
  group_by(neighbourhood, Station) %>%
  summarise(min = min(Total_Length), max = max(Total_Length), avg = mean(Total_Length), n()) %>% View()


# Calculate overall PT convenience (accounting for bus and train)
best_stops <- read.csv("Z:/Spatial/BusDepartures/best_stops.csv")
best_stops <- left_join(best_stops, best_station_convenience, by = "RespondentID")
best_stops <- best_stops %>% rename(Convenience_bus = convenience, Convenience_train = Convenience, 
                                    Distance_bus = Total_Length.x, Distance_train = Total_Length.y)
best_stops <- best_stops %>% mutate(Convenience_pt = ifelse(is.na(Convenience_train), Convenience_bus, 
                                                            ifelse(Convenience_train < Convenience_bus, Convenience_bus, Convenience_train)),
                                    Convenience_pt_dist = ifelse(is.na(Convenience_train), Distance_bus, 
                                                                 ifelse(Convenience_train < Convenience_bus, Distance_bus, Distance_train)))


best_stops %>% select(RespondentID, Convenience_bus, Distance_bus, Convenience_train, Distance_train, Convenience_pt, Convenience_pt_dist) %>%
  write.csv("Z:/Spatial/BusDepartures/convenience_pt.csv", row.names = F)
