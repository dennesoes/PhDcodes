# Kalman filter -----------------------------------------------------------
#Apply Kalman filter to a dataset
#Input: tracking data of one user
#Output: cleaned tracking data of one user (with two new columns for longitude and latitude)

do_kalman <- function(user_data){
  #initializing variables
  count <- nrow(user_data) # amount of data points in the day
  z <- cbind(user_data$lon_,user_data$lat_) #measurements
  
  #Allocate space:
  xhat <- matrix(rep(0,2*count),ncol =2) #a posteri estimate at each step
  P <- array(0,dim=c(2,2,count))  #a posteri error estimate
  xhatminus <- matrix(rep(0,2*count),ncol =2) #a priori estimate
  Pminus <- array(0,dim=c(2,2,count)) #a priori error estimate
  K <- array(0,dim=c(2,2,count)) #gain
  
  #Initializing matrices
  A <-diag(2)
  H<-diag(2)
  R<-function(k) diag(2)* user_data$accuracy_[k]^2#estimate of measurement variance
  Q<-function(k) diag(2)* as.numeric(user_data$timediff[k])^1.5# the process variance
  
  #initialise guesses:
  xhat[1,] <- z[1,]
  P[,,1] <- diag(2)
  
  for (k in 2:count){
    user_data$timediff[k] <- (user_data$time_[k] - user_data$time_[k-1])/1000
    
    #time update
    #project state ahead
    xhatminus[k,] <- A %*% xhat[k-1,] #+ B %*% u[k-1]
    
    #project error covariance ahead
    Pminus[,,k] <- A %*% P[,,k-1] %*%  t(A) + (Q(k))
    
    #measurement update
    # kalman gain
    K[,,k] <- Pminus[,,k] %*% t(H)/ (H %*% Pminus[,,k] %*% t(H) + R(k))
    
    #what if NaaN?
    K[,,k][which(is.nan(K[,,k]))]<-0
    
    # update estimate with measurement
    xhat[k,] <-  xhatminus[k,] + K[,,k] %*% (z[k,] - H %*% xhatminus[k,])
    #update error covariance
    P[,,k] = (diag(2) - K[,,k]%*% H) %*% Pminus[,,k]
  }
  colnames(xhat) <- c("lon_kal", "lat_kal")
  cbind(user_data, xhat)
}

