## Analysis of the car trips, planned as PT

# Load libraries
library(tidyverse)

# Load data
car_trips <- read.csv("Z:/Trackdata/cartrips/_cartrips.csv")
car_trips_car <- car_trips %>% filter(mode == "Car", !is.na(PT_walkdist_direct))

# Determine which trips are unique trips
multiple_OD <- list()
k <- 1
users <- NULL
for(i in 1:length(unique(car_trips_car$user_id))){
  OD_matrix <- as.data.frame(table(car_trips_car$origin[car_trips_car$user_id==unique(car_trips_car$user_id)[i]], 
                                   car_trips_car$destination[car_trips_car$user_id==unique(car_trips_car$user_id)[i]]))
  OD_matrix <- OD_matrix[OD_matrix$Freq>1,]
  if(nrow(OD_matrix) > 0){
    origins <- NULL
    destinations <- NULL
    
    for(j in 1:nrow(OD_matrix)){
      origins <- c(origins, as.character(OD_matrix[j,1]))
      destinations <- c(destinations, as.character(OD_matrix[j,2]))
    }
    multiple_OD[[k]] <- list(origins = origins, destinations = destinations)
    users <- c(users, unique(car_trips_car$user_id)[i])
    k <- k+1
  }
} # multiple_OD contains all non-unique trips (origin-destination pairs per user)
car_trips_car$unique <- T #set all trips to unique
for(i in 1:length(users)){ #falsify the variable in case there are multiple trips
  for(j in 1:length(multiple_OD[[i]]$origins)){
    car_trips_car[car_trips_car$user_id == users[i] & 
                    car_trips_car$origin == multiple_OD[[i]]$origins[[j]] &
                    car_trips_car$destination == multiple_OD[[i]]$destinations[[j]],]$unique <- F
    car_trips_car[car_trips_car$user_id == users[i] & 
                    car_trips_car$origin == multiple_OD[[i]]$origins[[j]] &
                    car_trips_car$destination == multiple_OD[[i]]$destinations[[j]],]$unique[1] <- T #set one of the trips to unique
  }
}
# Filter to unique trips only
car_trips_car <- car_trips_car %>% filter(unique == T)



# Overview of distribution of walk distances
car_trips_car %>%
  ggplot(aes(x=PT_walkdist_direct)) +
  stat_bin(aes(y=(nrow(car_trips_car)-cumsum(..count..))/nrow(car_trips_car)*100), 
           binwidth = 1, geom="step", size=1) +
  labs(#title = "Cumulative distribution of walk distances to/from bus and train",
    x="Distance (m)",
    y="Cumulative percentage of walks") +
  theme(legend.title = element_blank())


# Per neighbourhood
car_trips_car$neighbourhood <- NA
car_trips_car$neighbourhood[car_trips_car$respondent_id < 2000] <- "Moseley"
car_trips_car$neighbourhood[car_trips_car$respondent_id >= 2000] <- "Bournville"

car_trips_car %>%
  ggplot(aes(x=PT_walkdist_fast, colour=as.factor(neighbourhood))) +
  stat_bin(data=car_trips_car[car_trips_car$neighbourhood=="Moseley",],
           aes(y=(nrow(car_trips_car[car_trips_car$neighbourhood=="Moseley",])-cumsum(..count..))/nrow(car_trips_car[car_trips_car$neighbourhood=="Moseley",])*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=car_trips_car[car_trips_car$neighbourhood=="Bournville",],
           aes(y=(nrow(car_trips_car[car_trips_car$neighbourhood=="Bournville",])-cumsum(..count..))/nrow(car_trips_car[car_trips_car$neighbourhood=="Bournville",])*100), 
           binwidth = 1, geom="step", size=1) +
  labs(#title = "Cumulative distribution of walk distances to/from bus and train",
    x="PT-walk distance",
    y="Cumulative percentage of walks") +
  theme(legend.title = element_blank())

t.test(car_trips_car$PT_walkdist_direct[car_trips_car$neighbourhood=="Moseley"],
       car_trips_car$PT_walkdist_direct[car_trips_car$neighbourhood=="Bournville"])

car_trips_car$PT_pttime_fast <- car_trips_car$PT_totaltime_fast - car_trips_car$PT_walktime_fast
car_trips_car[car_trips_car$PT_pttime_fast==0 & !is.na(car_trips_car$PT_pttime_fast),]$PT_pttime_fast <- NA
car_trips_car$PT_pttime_direct <- car_trips_car$PT_totaltime_direct - car_trips_car$PT_walktime_direct
car_trips_car[car_trips_car$PT_pttime_direct==0 & !is.na(car_trips_car$PT_pttime_direct),]$PT_pttime_direct <- NA


car_trips_car %>%
  ggplot(aes(x=PT_walktime_fast/PT_pttime_fast, colour=as.factor(neighbourhood))) +
  stat_bin(data=subset(car_trips_car, car_trips_car$neighbourhood=="Moseley" & !is.na(car_trips_car$PT_pttime_direct)),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$neighbourhood=="Moseley"& !is.na(car_trips_car$PT_pttime_direct)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$neighbourhood=="Moseley"& !is.na(car_trips_car$PT_pttime_direct)))*100), 
           binwidth = 0.01, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car, car_trips_car$neighbourhood=="Bournville"& !is.na(car_trips_car$PT_pttime_direct)),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$neighbourhood=="Bournville"& !is.na(car_trips_car$PT_pttime_direct)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$neighbourhood=="Bournville"& !is.na(car_trips_car$PT_pttime_direct)))*100), 
           binwidth = 0.01, geom="step", size=1) +
  labs(x="Ratio walk time / PT time",
    y="Cumulative percentage of walks") +
  theme(legend.title = element_blank())

# Test difference between two groups
t.test(car_trips_car$PT_walktime_direct[car_trips_car$neighbourhood=="Moseley"]/car_trips_car$PT_pttime_direct[car_trips_car$neighbourhood=="Moseley"],
       car_trips_car$PT_walktime_direct[car_trips_car$neighbourhood=="Bournville"]/car_trips_car$PT_pttime_direct[car_trips_car$neighbourhood=="Bournville"])

## Attitudinal clusters
hclust_attitude_inclCycle <- read.csv("Z:/Clustering/hclust_attitude_inclCycle.csv")
hclust_attitude_inclCycle <- rename(hclust_attitude_inclCycle, respondent_id="ID", hclust_attitude_inclCycle="cluster")
car_trips_car <- left_join(car_trips_car, hclust_attitude_inclCycle)

car_trips_car %>% 
  group_by(hclust_attitude_inclCycle) %>%
  summarise("Mean distance" = mean(PT_walkdist_fast, na.rm=T), 
            "sd distance" = sd(PT_walkdist_fast, na.rm=T), 
            "Minimum distance" = min(PT_walkdist_fast, na.rm = T),
            "Maximum distance" = max(PT_walkdist_fast, na.rm = T),
            "Mean time" = mean(PT_walktime_fast, na.rm = T), 
            "sd time" = sd(PT_walktime_fast, na.rm = T),
            "n" = n())

car_trips_car %>% 
  group_by(hclust_attitude_inclCycle) %>%
  summarise("Mean distance" = mean(PT_walkdist_direct, na.rm=T), 
            "sd distance" = sd(PT_walkdist_direct, na.rm=T), 
            "Minimum distance" = min(PT_walkdist_direct, na.rm = T),
            "Maximum distance" = max(PT_walkdist_direct, na.rm = T),
            "Mean time" = mean(PT_walktime_direct, na.rm = T), 
            "sd time" = sd(PT_walktime_direct, na.rm = T),
            "n" = n())

car_trips_car %>%
  ggplot(aes(x=PT_walkdist_fast, colour=as.factor(hclust_attitude_inclCycle))) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1 & 
                                !is.na(car_trips_car$PT_walkdist_fast)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1 & 
                                                                                                     !is.na(car_trips_car$PT_walkdist_fast)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2 & 
                                !is.na(car_trips_car$PT_walkdist_fast)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2 & 
                                                                                                     !is.na(car_trips_car$PT_walkdist_fast)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==3),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==3 & 
                                !is.na(car_trips_car$PT_walkdist_fast)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==3 & 
                                                                                                     !is.na(car_trips_car$PT_walkdist_fast)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==4),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==4 & 
                                !is.na(car_trips_car$PT_walkdist_fast)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==4 & 
                                                                                                     !is.na(car_trips_car$PT_walkdist_fast)))*100), 
           binwidth = 1, geom="step", size=1) +
  labs(x="PT-walk distance", y="Cumulative percentage of walks") +
  theme(legend.title = element_blank())

car_trips_car %>%
  ggplot(aes(x=PT_walkdist_direct, colour=as.factor(hclust_attitude_inclCycle))) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1 & 
                                !is.na(car_trips_car$PT_walkdist_direct)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1 & 
                                                                                                          !is.na(car_trips_car$PT_walkdist_direct)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2 & 
                                !is.na(car_trips_car$PT_walkdist_direct)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2 & 
                                                                                                          !is.na(car_trips_car$PT_walkdist_direct)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==3),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==3 & 
                                !is.na(car_trips_car$PT_walkdist_direct)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==3 & 
                                                                                                          !is.na(car_trips_car$PT_walkdist_direct)))*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==4),
           aes(y=(nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==4 & 
                                !is.na(car_trips_car$PT_walkdist_direct)))-cumsum(..count..))/nrow(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==4 & 
                                                                                                          !is.na(car_trips_car$PT_walkdist_direct)))*100), 
           binwidth = 1, geom="step", size=1) +
  labs(x="PT-walk distance", y="Cumulative percentage of walks") +
  theme(legend.title = element_blank())


t.test(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1)$PT_walkdist_fast, 
       subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2)$PT_walkdist_fast)

t.test(subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==1)$PT_walkdist_direct, 
       subset(car_trips_car, car_trips_car$hclust_attitude_inclCycle==2)$PT_walkdist_direct)

# PT convenience ----------------------------------------------------------

pt_convenience <- read.csv("Z:/Spatial/BusDepartures/convenience_pt.csv")
car_trips_car <- left_join(car_trips_car, pt_convenience, by = c("respondent_id" = "RespondentID"))
car_trips_car %>% 
  group_by(Convenience_pt) %>%
  summarise("Mean distance" = mean(PT_walkdist_direct, na.rm=T), 
            "sd distance" = sd(PT_walkdist_direct, na.rm=T), 
            "Minimum distance" = min(PT_walkdist_direct, na.rm = T),
            "Maximum distance" = max(PT_walkdist_direct, na.rm = T),
            "Mean time" = mean(PT_walktime_direct, na.rm = T), 
            "sd time" = sd(PT_walktime_direct, na.rm = T),
            "n" = n())

car_trips_car %>% 
  group_by(Convenience_pt, neighbourhood) %>%
  summarise("Mean distance" = mean(PT_walkdist_direct, na.rm=T), 
            "sd distance" = sd(PT_walkdist_direct, na.rm=T), 
            "Minimum distance" = min(PT_walkdist_direct, na.rm = T),
            "Maximum distance" = max(PT_walkdist_direct, na.rm = T),
            "Mean time" = mean(PT_walktime_direct, na.rm = T), 
            "sd time" = sd(PT_walktime_direct, na.rm = T),
            "n" = n())




# PT use ------------------------------------------------------------------

## PT-users vs non-PT-users
pt_users <- read.csv("Z:/Clustering/pt_users.csv")
pt_users <- rename(pt_users, respondent_id="ID")
car_trips_car <- left_join(car_trips_car, pt_users)

car_trips_car %>% 
  group_by(PT_use_reg) %>%
  summarise("Mean distance" = mean(PT_walkdist_direct, na.rm=T), 
            "sd distance" = sd(PT_walkdist_direct, na.rm=T), 
            "Minimum distance" = min(PT_walkdist_direct, na.rm = T),
            "Maximum distance" = max(PT_walkdist_direct, na.rm = T),
            "Mean time" = mean(PT_walktime_direct, na.rm = T), 
            "sd time" = sd(PT_walktime_direct, na.rm = T),
            "n" = n())

car_trips_car %>% 
  group_by(PT_use) %>%
  summarise("Mean distance" = mean(PT_walkdist_fast, na.rm=T), 
            "sd distance" = sd(PT_walkdist_fast, na.rm=T), 
            "Minimum distance" = min(PT_walkdist_fast, na.rm = T),
            "Maximum distance" = max(PT_walkdist_fast, na.rm = T),
            "Mean time" = mean(PT_walktime_fast, na.rm = T), 
            "sd time" = sd(PT_walktime_fast, na.rm = T),
            "n" = n())


t.test(subset(car_trips_car, car_trips_car$PT_use_reg == T)$PT_walkdist_fast, subset(car_trips_car, car_trips_car$PT_use_reg == F)$PT_walkdist_fast)
t.test(subset(car_trips_car, car_trips_car$PT_use == T)$PT_walkdist_fast, subset(car_trips_car, car_trips_car$PT_use == F)$PT_walkdist_fast)

t.test(subset(car_trips_car, car_trips_car$PT_use_reg == T)$PT_walkdist_direct, subset(car_trips_car, car_trips_car$PT_use_reg == F)$PT_walkdist_direct)
t.test(subset(car_trips_car, car_trips_car$PT_use == T)$PT_walkdist_direct, subset(car_trips_car, car_trips_car$PT_use == F)$PT_walkdist_direct)






# Car time ----------------------------------------------------------------

car_trips_car$avg_speed <- car_trips_car$distance*(60/1000)/car_trips_car$time 
car_trips_car$PT_car_timeratio <- car_trips_car$PT_totaltime_fast/car_trips_car$time
car_trips_car_15 <- car_trips_car %>% filter(avg_speed > 15)
  
car_trips_car_15 %>% ggplot(aes(x=PT_car_timeratio, colour=as.factor(hclust_attitude_inclCycle))) +
  stat_bin(data=subset(car_trips_car_15,car_trips_car_15$hclust_attitude_inclCycle==1 & !is.na(car_trips_car_15$PT_car_timeratio)),
           aes(y=(nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==1 & !is.na(car_trips_car_15$PT_car_timeratio)))-cumsum(..count..))/nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==1 & !is.na(car_trips_car_15$PT_car_timeratio)))*100), 
           binwidth = 0.05, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==2 & !is.na(car_trips_car_15$PT_car_timeratio)),
           aes(y=(nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==2 & !is.na(car_trips_car_15$PT_car_timeratio)))-cumsum(..count..))/nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==2 & !is.na(car_trips_car_15$PT_car_timeratio)))*100), 
           binwidth = 0.05, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==3 & !is.na(car_trips_car_15$PT_car_timeratio)),
           aes(y=(nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==3 & !is.na(car_trips_car_15$PT_car_timeratio)))-cumsum(..count..))/nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==3 & !is.na(car_trips_car_15$PT_car_timeratio)))*100), 
           binwidth = 0.05, geom="step", size=1) +
  stat_bin(data=subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==4 & !is.na(car_trips_car_15$PT_car_timeratio)),
           aes(y=(nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==4 & !is.na(car_trips_car_15$PT_car_timeratio)))-cumsum(..count..))/nrow(subset(car_trips_car_15, car_trips_car_15$hclust_attitude_inclCycle==4 & !is.na(car_trips_car_15$PT_car_timeratio)))*100), 
           binwidth = 0.05, geom="step", size=1) +
  scale_x_continuous(limits = c(0, 10)) +
  labs(#title = "Cumulative distribution of walk distances to/from bus and train",
    x="PT-car time ratio",
    y="Cumulative percentage of walks") +
  theme(legend.title = element_blank())
  
car_trips_car_15 %>% filter(!is.na(PT_car_timeratio), !is.na(PT_walktime_fast), PT_walktime_fast/PT_totaltime_fast<1) %>% 
  ggplot(aes(x=PT_car_timeratio, y=(PT_walktime_fast/PT_totaltime_fast)*100, colour = as.factor(neighbourhood))) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Ratio PT time vs car time",
       y = "Proportion walk time of total PT time",
       colour = "Neighbourhood")



# Planned car time --------------------------------------------------------

car_trips_planned <- read.csv("Z:/Trackdata/cartrips/_car_replanned.csv")

car_trips_time <- left_join(car_trips_car, car_trips_planned, by = c("tripleg_id" = "tripleg_id")) %>%
  mutate(traveltime = traveltime/60,
         timetraffic = timetraffic/60)

# PT total time vs car time in traffic
car_trips_time  %>%
  ggplot() +
  geom_point(aes(PT_totaltime_direct, timetraffic, colour = factor(PT_use_reg))) +
  geom_abline() + 
  theme_bw() +
  scale_x_continuous(expand = expand_scale(add = c(0, 0))) +
  scale_y_continuous(expand = expand_scale(add = c(0,0))) +
  labs(x = "Total travel time by PT",
       y = "Travel time by car") +
  theme(legend.position = "none")


# Boxplot ratio total PT time vs car time in traffic
car_trips_time %>%
  ggplot(aes(x = factor(hclust_attitude_inclCycle), y = PT_walktime_fast/timetraffic)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "PT Convenience score", 
       y = "Ratio PT-walk time and car travel")

car_trips_time %>%
  ggplot(aes(x = neighbourhood, y = traveltime)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Neighbourhood", 
       y = "Car travel time without traffic (min)")


# Statistical test
t.test(PT_totaltime_direct/timetraffic ~ neighbourhood, data = car_trips_time)



car_trips_time <- mutate(car_trips_time, PTwalkcarratio = PT_walktime_direct/timetraffic) 
car_trips_time %>%
  ggplot(aes(x=PTwalkcarratio, colour=as.factor(neighbourhood))) +
  stat_bin(data=car_trips_time[car_trips_time$neighbourhood=="Moseley",],
           aes(y=(nrow(car_trips_time[car_trips_time$neighbourhood=="Moseley",])-cumsum(..count..))/nrow(car_trips_time[car_trips_time$neighbourhood=="Moseley",])*100), 
           binwidth = 1, geom="step", size=1) +
  stat_bin(data=car_trips_time[car_trips_time$neighbourhood=="Bournville",],
           aes(y=(nrow(car_trips_time[car_trips_time$neighbourhood=="Bournville",])-cumsum(..count..))/nrow(car_trips_time[car_trips_time$neighbourhood=="Bournville",])*100), 
           binwidth = 1, geom="step", size=1) +
  labs(#title = "Cumulative distribution of walk distances to/from bus and train",
    x="PT-walk distance/timetraffic",
    y="Cumulative percentage of walks") +
  theme(legend.title = element_blank())


