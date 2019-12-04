#Regression
library(tidyverse)
library(scales)
library(rms)

setwd("C:/Users/dxv632/OneDrive - University of Birmingham/_PhD/_Parts/Questionnaire/Affect-SocioDemo")

# Read data ---------------------------------------------------------------
data <- read.csv(file="//its-rds/2017/tightmr-transport-walking/Data_analysis2.csv", header=TRUE, sep=",")
data$ID <- as.factor(data$ID)
data <- rename(data,Respondent=ID, Attit_Birm_car=Attit_Birm_drive,Attit_Birm_cycle=Attit_Birm_cycling,
               Use_car=Use_drive,Use_cycle=Use_cycling,Attit_train=Attit_Train,Attit_bus=Attit_Bus)
data_convenience_PT <- read.csv("Z:/Spatial/BusDepartures/convenience_pt.csv")

data <- data %>%
  mutate(Respondent = as.numeric(as.character(Respondent))) %>%
  left_join(data_convenience_PT, by=c("Respondent" = "RespondentID"))



# Create positivity variables --------------------------------------------------------

#If at least two ratings were given, and at least two ratings were positive, the person is positive.
for(i in 1:nrow(data)){
  ifelse(sum(is.na(data[i, 20:22]))<3, 
         ifelse(sum(data[i,20:22]>4)>1,data$Positive_walk[i] <- 1, data$Positive_walk[i] <- 0),
         data$Positive_walk[i] <- NA)
  ifelse(sum(is.na(data[i, 26:28]))<3, 
         ifelse(sum(data[i,26:28]>4)>1,data$Positive_car[i] <- 1, data$Positive_car[i] <- 0),
         data$Positive_car[i] <- NA)
  ifelse(sum(is.na(data[i, 32:34]))<3, 
         ifelse(sum(data[i,32:34]>4)>1,data$Positive_train[i] <- 1, data$Positive_train[i] <- 0),
         data$Positive_train[i] <- NA)
  ifelse(sum(is.na(data[i, 38:40]))<3, 
         ifelse(sum(data[i,38:40]>4)>1,data$Positive_cycle[i] <- 1, data$Positive_cycle[i] <- 0),
         data$Positive_cycle[i] <- NA)
  ifelse(sum(is.na(data[i, 44:46]))<3, 
         ifelse(sum(data[i,44:46]>4)>1,data$Positive_bus[i] <- 1, data$Positive_bus[i] <- 0),
         data$Positive_bus[i] <- NA)
}



# Prepare dataset for regression ------------------------------------------

reg.data <- data %>%
  select(Respondent, Neighbourhood, Gender, Ethnicity, Income, 
         Avail_car, Avail_Pt, Avail_cycle, Convenience_bus, Convenience_train,
         Positive_walk, Positive_car, Positive_cycle, Positive_train, Positive_bus
  ) %>%
  mutate(Deprivation = fct_collapse(Neighbourhood, Deprived = c("Castle Vale", "Small Heath"), Nondeprived = c("Moseley", "Bournville")),
         Convenience_train = ifelse(is.na(Convenience_train), 0, Convenience_train),
         Convenience_bus1 = fct_collapse(factor(Convenience_bus), "0" = c("1", "2"), "1" = c("3", "4", "5")),
         Convenience_bus2 = fct_collapse(factor(Convenience_bus), "0" = c("1", "2", "3"), "1" = c("4", "5")),
         Convenience_train1 = fct_collapse(factor(Convenience_train), "0" = "0", "1" = c("1", "2", "3", "4")),
         Convenience_train2 = fct_collapse(factor(Convenience_train), "0" = c("0", "1", "2"), "1" = c("3", "4")),
         Gender = fct_recode(factor(Gender), NULL = "0", Male = "1", Female = "2"),
         Income1 = fct_recode(factor(Income), NULL = "0", "0" = "1", "1" = "2", "1" = "3", "1" = "4"),
         Income2 = fct_recode(factor(Income), NULL = "0", "0" = "1", "0" = "2", "1" = "3", "1" = "4"),
         Income3 = fct_recode(factor(Income), NULL = "0", "0" = "1", "0" = "2", "0" = "3", "1" = "4"),
         Ethnicity = fct_collapse(factor(Ethnicity), NULL = "0", White = "4", Nonwhite = c("1", "2", "3", "5")),
         Avail_car = fct_collapse(factor(Avail_car), NULL = "0", Available = c("1", "2"), "Not Available" = c("3", "4")),
         Avail_cycle = fct_collapse(factor(Avail_cycle), NULL = "0", Available = c("1", "2"), "Not Available" = c("3", "4")),
         Avail_Pt = fct_collapse(factor(Avail_Pt), NULL = "0", Available = c("1", "2", "3"), "Not Available" = c("4", "5"))
  )


# Model with clustered error terms ----------------------------------------
dd <- datadist(reg.data)
options(datadist="dd")

posmodes <- paste0("Positive_", c("walk", "car", "train", "cycle", "bus"))

for(p in posmodes){
  model <- lrm(get(p) ~ Deprivation + 
                Convenience_bus1 + Convenience_bus2 +
                Convenience_train1 + Convenience_train2 +
                Avail_Pt + 
                Gender + 
                Income1 + Income2 + Income3 +
                Ethnicity + Avail_cycle +
                Avail_car,
              data=reg.data, x=T, y=T)
  
  modelres <- robcov(model, cluster = reg.data$Neighbourhood)
  mres_mat <- matrix(data = NA, ncol = 7, nrow = length(modelres$coefficients)+1)
  colnames(mres_mat) <- c("Variable", "Beta", "SE", "p-value", "Odds ratio", "OR 95% CI lower", "OR 95% CI upper")
  SE <- sqrt(diag(modelres$var))
  mres_mat[1:nrow(mres_mat)-1,1] <- names(modelres$coefficients)
  mres_mat[1:nrow(mres_mat)-1,2] <- modelres$coefficients
  mres_mat[1:nrow(mres_mat)-1,3] <- SE
  mres_mat[1:nrow(mres_mat)-1,4] <- 2*(1-pnorm(abs(modelres$coefficients/SE)))
  mres_mat[1:nrow(mres_mat)-1,5] <- exp(modelres$coefficients)
  mres_mat[1:nrow(mres_mat)-1,6] <- exp(modelres$coefficients-1.96*SE)
  mres_mat[1:nrow(mres_mat)-1,7] <- exp(modelres$coefficients+1.96*SE)
  mres_mat[nrow(mres_mat),1] <- paste("N = ", modelres$stats[[1]])
  mres_mat[nrow(mres_mat),2] <- paste("Nagelkerke R2 = ", modelres$stats[[10]])
  mres_mat[nrow(mres_mat),3] <- paste("df = ", modelres$stats[[4]])
  mres_mat[nrow(mres_mat),4] <- paste("Model chisq = ", modelres$stats[[3]])
  mres_mat[nrow(mres_mat),5] <- paste("p = ", modelres$stats[[5]])
  mres_mat[nrow(mres_mat),6] <- AIC(modelres)
  mres_mat[nrow(mres_mat),7] <- ""
  
  file.name <- paste0("Models/Chapter/Model_",p,".csv")
  write.csv(mres_mat, file = file.name, row.names = F)
}


