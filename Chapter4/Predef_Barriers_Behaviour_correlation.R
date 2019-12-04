# Compute correlations between ratings of predefined barriers and behaviour (frequency of mode use)
library(tidyverse)

# Load data
data <- read.csv(file="//its-rds/2017/tightmr-transport-walking/Data_analysis2.csv", header=TRUE, sep=",")

# Define barrier columns
Walk_barriers <- paste0("Bar_walk",seq(1:11))
Bus_barriers <- paste0("Bar_bus", seq(1:9))
Train_barriers <- paste0("Bar_train", seq(1:9))
Barriers <- c(Walk_barriers, Bus_barriers, Train_barriers)
Modes <- paste0("Use_", c("walk", "cycling", "drive", "pass", "taxi", "tram", "bus", "train"))

# Select relevant data
analysis_data <- data %>%
  select(ID, Modes, Barriers)

# Correlation tests between barrier experience and mode use
cor_mat <- matrix(data=NA, ncol=4, nrow=length(Barriers)*8)
colnames(cor_mat) <- c("usage", "Barrier","Kendall's Tau", "p")
for(m in 1:length(Modes)){
  for(b in 1:length(Barriers)){
    kendall_result <- cor.test(analysis_data[,Modes[m]], analysis_data[,Barriers[b]], method = "kendall")
    cor_mat[(m-1)*length(Barriers)+b,1] <- Modes[m]
    cor_mat[(m-1)*length(Barriers)+b,2] <- Barriers[b]
    cor_mat[(m-1)*length(Barriers)+b,3] <- kendall_result$estimate
    cor_mat[(m-1)*length(Barriers)+b,4] <- kendall_result$p.value
  }
}

#Result
write.csv(cor_mat, "C:/Users/dxv632/OneDrive - University of Birmingham/_PhD/_Parts/Questionnaire/PredefinedBarriers/Barriers_Behaviour_correlation.csv", row.names = F)

