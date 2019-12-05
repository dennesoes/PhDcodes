# data_cleaning -----------------------------------------------------------
# Function to filter data points with unlikely values
# delete points with small angles and high speeds and high acceleration
# and iterate calculations
# Can be problematic for high speed train journeys

data_cleaning <- function(user_data){
  n1 <- nrow(user_data)+1
  while(nrow(user_data) < n1){
    n1 <- nrow(user_data)
    user_data <- filter(user_data, (angle_acute!=1 | is.na(angle_acute)) & 
                          (speed_kmph<175 | is.na(speed_kmph)) & 
                          (acceleration<5 | is.na(acceleration)) & 
                          (timediff>0 | is.na(timediff)) & 
                          (distance>0 | is.na(distance)))
    user_data <- TrackDistTime(user_data)
  }
  user_data
}