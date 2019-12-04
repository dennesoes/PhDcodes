#Tests of relationships between associations and other factors 
##with FactoMineR package
library(tidyverse)
library(reshape2)
library(scales)
library(flipDimensionReduction)
library(r2d3)
setwd("~/_PhD/_Parts/Analysis/AssociationAnalysis")

#read data
as_data <- read.csv(file="//its-rds/2017/tightmr-transport-walking/AssociationData/AssociationData3_csv.csv", header=TRUE, sep=",")
data <- read.csv(file="//its-rds/2017/tightmr-transport-walking/Data_analysis2.csv", header=TRUE, sep=",")
data$ID <- as.factor(data$ID)
as_data$Respondent <- as.factor(as_data$Respondent)
data <- rename(data,Respondent=ID, Attit_Birm_car=Attit_Birm_drive,Attit_Birm_cycle=Attit_Birm_cycling,Use_car=Use_drive,Use_cycle=Use_cycling,Attit_train=Attit_Train,Attit_bus=Attit_Bus)

#calculate valence means
data$Valence_walk_avg <- rowMeans(data[,20:22],na.rm=TRUE)
data$Valence_car_avg <- rowMeans(data[,26:28],na.rm=TRUE)
data$Valence_train_avg <- rowMeans(data[,32:34],na.rm=TRUE)
data$Valence_cycle_avg <- rowMeans(data[,38:40],na.rm=TRUE)
data$Valence_bus_avg <- rowMeans(data[,44:46],na.rm=TRUE)
data$Valence_total <- rowSums(data[,c(20:22,26:28,32:34,38:40,44:46)])
data$Valence_avgtotal <- rowSums(data[,c("Valence_walk_avg", "Valence_car_avg", "Valence_train_avg", "Valence_cycle_avg", "Valence_bus_avg")])


#Create codes
modes <- list("walk", "car", "train", "cycle", "bus")
neighbourhoods <- list("Moseley","Bournville","Castle Vale","Small Heath")
valence_modes <- paste0("Valence_",modes,"_avg")
codes_feelings <- c(101,102,104,106,121,123,109,110,111,118,112,103,113)
codes_practice <- c(124, 107, 917)
codes_environment <- c(201,202,205,206,210,207,208,209)
codes_money <- c(301,302,304)
codes_social <- c(401, 403, 613)
codes_opportunities <- c(501,502,505,506,507)
codes_concerns <- c(601,602,603,604,605,606,607,608,610,611,612,615,616,617,618,619,620)
codes_health <- c(701,702,703,705,706,707)
codes_facilities <- c(802,803,804,805,806,808,812,909,814,913)
codes_convenience <- c(902,920,903,904,905,906,907,908,910,916,918,919)
codes_safety <- c(1001,1002,1004)
codes <- c(0, codes_feelings, codes_practice, codes_environment, codes_money, codes_social, codes_opportunities, codes_concerns, codes_health, codes_facilities, codes_convenience, codes_safety)

#Category variables
data <- within(data,{
  Bar_walk1_cat <- NA
  Bar_walk1_cat[Bar_walk1==1|Bar_walk1==2|Bar_walk1==3]<-"Disagree"
  Bar_walk1_cat[Bar_walk1==4]<-"Neutral"
  Bar_walk1_cat[Bar_walk1==5|Bar_walk1==6|Bar_walk1==7]<-"Agree"
  Bar_walk2_cat <- NA
  Bar_walk2_cat[Bar_walk2==1|Bar_walk2==2|Bar_walk2==3]<-"Disagree"
  Bar_walk2_cat[Bar_walk2==4]<-"Neutral"
  Bar_walk2_cat[Bar_walk2==5|Bar_walk2==6|Bar_walk2==7]<-"Agree"
  Bar_walk3_cat <- NA
  Bar_walk3_cat[Bar_walk3==1|Bar_walk3==2|Bar_walk3==3]<-"Disagree"
  Bar_walk3_cat[Bar_walk3==4]<-"Neutral"
  Bar_walk3_cat[Bar_walk3==5|Bar_walk3==6|Bar_walk3==7]<-"Agree"
  Bar_walk4_cat <- NA
  Bar_walk4_cat[Bar_walk4==1|Bar_walk4==2|Bar_walk4==3]<-"Disagree"
  Bar_walk4_cat[Bar_walk4==4]<-"Neutral"
  Bar_walk4_cat[Bar_walk4==5|Bar_walk4==6|Bar_walk4==7]<-"Agree"
  Bar_walk5_cat <- NA
  Bar_walk5_cat[Bar_walk5==1|Bar_walk5==2|Bar_walk5==3]<-"Disagree"
  Bar_walk5_cat[Bar_walk5==4]<-"Neutral"
  Bar_walk5_cat[Bar_walk5==5|Bar_walk5==6|Bar_walk5==7]<-"Agree"
  Bar_walk6_cat <- NA
  Bar_walk6_cat[Bar_walk6==1|Bar_walk6==2|Bar_walk6==3]<-"Disagree"
  Bar_walk6_cat[Bar_walk6==4]<-"Neutral"
  Bar_walk6_cat[Bar_walk6==5|Bar_walk6==6|Bar_walk6==7]<-"Agree"
  Bar_walk7_cat <- NA
  Bar_walk7_cat[Bar_walk7==1|Bar_walk7==2|Bar_walk7==3]<-"Disagree"
  Bar_walk7_cat[Bar_walk7==4]<-"Neutral"
  Bar_walk7_cat[Bar_walk7==5|Bar_walk7==6|Bar_walk7==7]<-"Agree"
  Bar_walk8_cat <- NA
  Bar_walk8_cat[Bar_walk8==1|Bar_walk8==2|Bar_walk8==3]<-"Disagree"
  Bar_walk8_cat[Bar_walk8==4]<-"Neutral"
  Bar_walk8_cat[Bar_walk8==5|Bar_walk8==6|Bar_walk8==7]<-"Agree"
  Bar_walk9_cat <- NA
  Bar_walk9_cat[Bar_walk9==1|Bar_walk9==2|Bar_walk9==3]<-"Disagree"
  Bar_walk9_cat[Bar_walk9==4]<-"Neutral"
  Bar_walk9_cat[Bar_walk9==5|Bar_walk9==6|Bar_walk9==7]<-"Agree"
  Bar_walk10_cat <- NA
  Bar_walk10_cat[Bar_walk10==1|Bar_walk10==2|Bar_walk10==3]<-"Disagree"
  Bar_walk10_cat[Bar_walk10==4]<-"Neutral"
  Bar_walk10_cat[Bar_walk10==5|Bar_walk10==6|Bar_walk10==7]<-"Agree"
  Bar_walk11_cat <- NA
  Bar_walk11_cat[Bar_walk11==1|Bar_walk11==2|Bar_walk11==3]<-"Disagree"
  Bar_walk11_cat[Bar_walk11==4]<-"Neutral"
  Bar_walk11_cat[Bar_walk11==5|Bar_walk11==6|Bar_walk11==7]<-"Agree"
  
  Bar_bus1_cat <- NA
  Bar_bus1_cat[Bar_bus1==1|Bar_bus1==2|Bar_bus1==3]<-"Disagree"
  Bar_bus1_cat[Bar_bus1==4]<-"Neutral"
  Bar_bus1_cat[Bar_bus1==5|Bar_bus1==6|Bar_bus1==7]<-"Agree"
  Bar_bus2_cat <- NA
  Bar_bus2_cat[Bar_bus2==1|Bar_bus2==2|Bar_bus2==3]<-"Disagree"
  Bar_bus2_cat[Bar_bus2==4]<-"Neutral"
  Bar_bus2_cat[Bar_bus2==5|Bar_bus2==6|Bar_bus2==7]<-"Agree"
  Bar_bus3_cat <- NA
  Bar_bus3_cat[Bar_bus3==1|Bar_bus3==2|Bar_bus3==3]<-"Disagree"
  Bar_bus3_cat[Bar_bus3==4]<-"Neutral"
  Bar_bus3_cat[Bar_bus3==5|Bar_bus3==6|Bar_bus3==7]<-"Agree"
  Bar_bus4_cat <- NA
  Bar_bus4_cat[Bar_bus4==1|Bar_bus4==2|Bar_bus4==3]<-"Disagree"
  Bar_bus4_cat[Bar_bus4==4]<-"Neutral"
  Bar_bus4_cat[Bar_bus4==5|Bar_bus4==6|Bar_bus4==7]<-"Agree"
  Bar_bus5_cat <- NA
  Bar_bus5_cat[Bar_bus5==1|Bar_bus5==2|Bar_bus5==3]<-"Disagree"
  Bar_bus5_cat[Bar_bus5==4]<-"Neutral"
  Bar_bus5_cat[Bar_bus5==5|Bar_bus5==6|Bar_bus5==7]<-"Agree"
  Bar_bus6_cat <- NA
  Bar_bus6_cat[Bar_bus6==1|Bar_bus6==2|Bar_bus6==3]<-"Disagree"
  Bar_bus6_cat[Bar_bus6==4]<-"Neutral"
  Bar_bus6_cat[Bar_bus6==5|Bar_bus6==6|Bar_bus6==7]<-"Agree"
  Bar_bus7_cat <- NA
  Bar_bus7_cat[Bar_bus7==1|Bar_bus7==2|Bar_bus7==3]<-"Disagree"
  Bar_bus7_cat[Bar_bus7==4]<-"Neutral"
  Bar_bus7_cat[Bar_bus7==5|Bar_bus7==6|Bar_bus7==7]<-"Agree"
  Bar_bus8_cat <- NA
  Bar_bus8_cat[Bar_bus8==1|Bar_bus8==2|Bar_bus8==3]<-"Disagree"
  Bar_bus8_cat[Bar_bus8==4]<-"Neutral"
  Bar_bus8_cat[Bar_bus8==5|Bar_bus8==6|Bar_bus8==7]<-"Agree"
  Bar_bus9_cat <- NA
  Bar_bus9_cat[Bar_bus9==1|Bar_bus9==2|Bar_bus9==3]<-"Disagree"
  Bar_bus9_cat[Bar_bus9==4]<-"Neutral"
  Bar_bus9_cat[Bar_bus9==5|Bar_bus9==6|Bar_bus9==7]<-"Agree"
  
  Bar_train1_cat <- NA
  Bar_train1_cat[Bar_train1==1|Bar_train1==2|Bar_train1==3]<-"Disagree"
  Bar_train1_cat[Bar_train1==4]<-"Neutral"
  Bar_train1_cat[Bar_train1==5|Bar_train1==6|Bar_train1==7]<-"Agree"
  Bar_train2_cat <- NA
  Bar_train2_cat[Bar_train2==1|Bar_train2==2|Bar_train2==3]<-"Disagree"
  Bar_train2_cat[Bar_train2==4]<-"Neutral"
  Bar_train2_cat[Bar_train2==5|Bar_train2==6|Bar_train2==7]<-"Agree"
  Bar_train3_cat <- NA
  Bar_train3_cat[Bar_train3==1|Bar_train3==2|Bar_train3==3]<-"Disagree"
  Bar_train3_cat[Bar_train3==4]<-"Neutral"
  Bar_train3_cat[Bar_train3==5|Bar_train3==6|Bar_train3==7]<-"Agree"
  Bar_train4_cat <- NA
  Bar_train4_cat[Bar_train4==1|Bar_train4==2|Bar_train4==3]<-"Disagree"
  Bar_train4_cat[Bar_train4==4]<-"Neutral"
  Bar_train4_cat[Bar_train4==5|Bar_train4==6|Bar_train4==7]<-"Agree"
  Bar_train5_cat <- NA
  Bar_train5_cat[Bar_train5==1|Bar_train5==2|Bar_train5==3]<-"Disagree"
  Bar_train5_cat[Bar_train5==4]<-"Neutral"
  Bar_train5_cat[Bar_train5==5|Bar_train5==6|Bar_train5==7]<-"Agree"
  Bar_train6_cat <- NA
  Bar_train6_cat[Bar_train6==1|Bar_train6==2|Bar_train6==3]<-"Disagree"
  Bar_train6_cat[Bar_train6==4]<-"Neutral"
  Bar_train6_cat[Bar_train6==5|Bar_train6==6|Bar_train6==7]<-"Agree"
  Bar_train7_cat <- NA
  Bar_train7_cat[Bar_train7==1|Bar_train7==2|Bar_train7==3]<-"Disagree"
  Bar_train7_cat[Bar_train7==4]<-"Neutral"
  Bar_train7_cat[Bar_train7==5|Bar_train7==6|Bar_train7==7]<-"Agree"
  Bar_train8_cat <- NA
  Bar_train8_cat[Bar_train8==1|Bar_train8==2|Bar_train8==3]<-"Disagree"
  Bar_train8_cat[Bar_train8==4]<-"Neutral"
  Bar_train8_cat[Bar_train8==5|Bar_train8==6|Bar_train8==7]<-"Agree"
  Bar_train9_cat <- NA
  Bar_train9_cat[Bar_train9==1|Bar_train9==2|Bar_train9==3]<-"Disagree"
  Bar_train9_cat[Bar_train9==4]<-"Neutral"
  Bar_train9_cat[Bar_train9==5|Bar_train9==6|Bar_train9==7]<-"Agree"
  
  Use_walk_cat <- NA
  Use_walk_cat[Use_walk==1|Use_walk==2]<-"Never/rarely"
  Use_walk_cat[Use_walk==3]<-"Sometimes"
  Use_walk_cat[Use_walk==4]<-"Regular"
  Use_walk_cat[Use_walk==5]<-"Frequent"
  Use_car_cat <- NA
  Use_car_cat[Use_car==1|Use_car==2]<-"Never/rarely"
  Use_car_cat[Use_car==3]<-"Sometimes"
  Use_car_cat[Use_car==4]<-"Regular"
  Use_car_cat[Use_car==5]<-"Frequent"
  Use_train_cat <- NA
  Use_train_cat[Use_train==1]<-"Never"
  Use_train_cat[Use_train==2]<-"Rarely"
  Use_train_cat[Use_train==3]<-"Sometimes"
  Use_train_cat[Use_train==4|Use_train==5]<-"Frequent"
  Use_cycle_cat <- NA
  Use_cycle_cat[Use_cycle==1]<-"Never"
  Use_cycle_cat[Use_cycle==2|Use_cycle==3]<-"Rarely/sometimes"
  Use_cycle_cat[Use_cycle==4|Use_cycle==5]<-"Regular/frequent"
  Use_bus_cat <- NA
  Use_bus_cat[Use_bus==1]<-"Never"
  Use_bus_cat[Use_bus==2]<-"Rarely"
  Use_bus_cat[Use_bus==4|Use_bus==3|Use_bus==5]<-"Sometimes/regular"
  
  Gender[Gender==0]<-NA
  Age_cat <- NA
  Age_cat[Year_birth>=1952 & Year_birth <= 1967]<-1
  Age_cat[Year_birth>=1968 & Year_birth <= 1977]<-2
  Age_cat[Year_birth>=1978 & Year_birth <= 1987]<-3
  Age_cat[Year_birth>=1988 & Year_birth <= 2000]<-4
  Age <- 2017-Year_birth
  Age[Age==2017] <- NA
  HH_size_cat <- NA
  HH_size_cat[HH_size==1]<-"Single"
  HH_size_cat[HH_size==2]<-"Two people"
  HH_size_cat[HH_size==3]<-"Three people"
  HH_size_cat[HH_size==4|HH_size==5|HH_size==6|HH_size==7]<-"Four or more people"
  HH_u12_cat <- NA
  HH_u12_cat[HH_u12==0]<-"No children"
  HH_u12_cat[HH_u12==1]<-"One child"
  HH_u12_cat[HH_u12==2|HH_u12==3]<-"Two or more"
  Income_cat <- NA
  Income_cat[Income==1]<-"to £15k"
  Income_cat[Income==2]<-"£15-25k"
  Income_cat[Income==3]<-"£25-50k"
  Income_cat[Income==4]<-"over £50k"
  Income_cap <- Income/HH_size
  Income_cap_cat <- NA
  Income_cap_cat[Income_cap<1 & Income_cap>0] <- "low"
  Income_cap_cat[Income_cap>=1 & Income_cap <= 1.5] <- "medium"
  Income_cap_cat[Income_cap>=2 & Income_cap <= 4] <- "high"
  Ethnicity_cat <- NA
  Ethnicity_cat[Ethnicity==4]<-"White"
  Ethnicity_cat[Ethnicity==1|Ethnicity==2|Ethnicity==3|Ethnicity==5]<-"Non-white"
  Deprivation <- NA
  Deprivation[Neighbourhood=="Moseley"|Neighbourhood=="Bournville"]<-"Low deprivation"
  Deprivation[Neighbourhood=="Castle Vale"|Neighbourhood=="Small Heath"]<-"High deprivation"
  TrainAccess <- NA
  TrainAccess[Neighbourhood=="Moseley"|Neighbourhood=="Castle Vale"]<-"Bus only"
  TrainAccess[Neighbourhood=="Bournville"|Neighbourhood=="Small Heath"]<-"Train and bus"
})
data$Use_walk_cat <- as.factor(data$Use_walk_cat)
data$Use_walk_cat <- factor(data$Use_walk_cat, c("Never/rarely","Sometimes","Regular","Frequent"))
data$Use_car_cat <- as.factor(data$Use_car_cat)
data$Use_car_cat <- factor(data$Use_car_cat, c("Never/rarely","Sometimes","Regular","Frequent"))
data$Use_train_cat <- as.factor(data$Use_train_cat)
data$Use_train_cat <- factor(data$Use_train_cat, c("Never","Rarely","Sometimes","Frequent"))
data$Use_cycle_cat <- as.factor(data$Use_cycle_cat)
data$Use_cycle_cat <- factor(data$Use_cycle_cat, c("Never","Rarely/sometimes","Regular/frequent"))
data$Use_bus_cat <- as.factor(data$Use_bus_cat)
data$Use_bus_cat <- factor(data$Use_bus_cat, c("Never","Rarely","Sometimes/regular"))
data$HH_size_cat <- as.factor(data$HH_size_cat)
data$HH_size_cat <- factor(data$HH_size_cat, c("Single","Two people", "Three people", "Four or more people"))
data$HH_u12_cat <- as.factor(data$HH_u12_cat)
data$HH_u12_cat <- factor(data$HH_u12_cat, c("No children", "One child", "Two or more"))
data$Income_cat <- as.factor(data$Income_cat)
data$Income_cat <- factor(data$Income_cat, c("to £15k","£15-25k", "£25-50k", "over £50k"))
data$Income_cap_cat <- as.factor(data$Income_cap_cat)
data$Income_cap_cat <- factor(data$Income_cap_cat, c("low","medium", "high"))
data$Ethnicity_cat <- as.factor(data$Ethnicity_cat)
data$Deprivation <- as.factor(data$Deprivation)
data$TrainAccess <- as.factor(data$TrainAccess)
data$Age_cat <- as.factor(data$Age_cat)
data$Avail_car <- as.factor(data$Avail_car)
data$Avail_cycle <- as.factor(data$Avail_cycle)
data$Avail_Pt <- as.factor(data$Avail_Pt)
data$Bar_walk1_cat <- as.factor(data$Bar_walk1_cat)
data$Bar_walk2_cat <- as.factor(data$Bar_walk2_cat)
data$Bar_walk3_cat <- as.factor(data$Bar_walk3_cat)
data$Bar_walk4_cat <- as.factor(data$Bar_walk4_cat)
data$Bar_walk5_cat <- as.factor(data$Bar_walk5_cat)
data$Bar_walk6_cat <- as.factor(data$Bar_walk6_cat)
data$Bar_walk7_cat <- as.factor(data$Bar_walk7_cat)
data$Bar_walk8_cat <- as.factor(data$Bar_walk8_cat)
data$Bar_walk9_cat <- as.factor(data$Bar_walk9_cat)
data$Bar_walk10_cat <- as.factor(data$Bar_walk10_cat)
data$Bar_walk11_cat <- as.factor(data$Bar_walk11_cat)
data$Bar_walk1_cat <- factor(data$Bar_walk1_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk2_cat <- factor(data$Bar_walk2_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk3_cat <- factor(data$Bar_walk3_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk4_cat <- factor(data$Bar_walk4_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk5_cat <- factor(data$Bar_walk5_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk6_cat <- factor(data$Bar_walk6_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk7_cat <- factor(data$Bar_walk7_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk8_cat <- factor(data$Bar_walk8_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk9_cat <- factor(data$Bar_walk9_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk10_cat <- factor(data$Bar_walk10_cat, c("Disagree","Neutral","Agree"))
data$Bar_walk11_cat <- factor(data$Bar_walk10_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus1_cat <- as.factor(data$Bar_bus1_cat)
data$Bar_bus2_cat <- as.factor(data$Bar_bus2_cat)
data$Bar_bus3_cat <- as.factor(data$Bar_bus3_cat)
data$Bar_bus4_cat <- as.factor(data$Bar_bus4_cat)
data$Bar_bus5_cat <- as.factor(data$Bar_bus5_cat)
data$Bar_bus6_cat <- as.factor(data$Bar_bus6_cat)
data$Bar_bus7_cat <- as.factor(data$Bar_bus7_cat)
data$Bar_bus8_cat <- as.factor(data$Bar_bus8_cat)
data$Bar_bus9_cat <- as.factor(data$Bar_bus9_cat)
data$Bar_bus1_cat <- factor(data$Bar_bus1_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus2_cat <- factor(data$Bar_bus2_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus3_cat <- factor(data$Bar_bus3_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus4_cat <- factor(data$Bar_bus4_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus5_cat <- factor(data$Bar_bus5_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus6_cat <- factor(data$Bar_bus6_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus7_cat <- factor(data$Bar_bus7_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus8_cat <- factor(data$Bar_bus8_cat, c("Disagree","Neutral","Agree"))
data$Bar_bus9_cat <- factor(data$Bar_bus9_cat, c("Disagree","Neutral","Agree"))
data$Bar_train1_cat <- as.factor(data$Bar_train1_cat)
data$Bar_train2_cat <- as.factor(data$Bar_train2_cat)
data$Bar_train3_cat <- as.factor(data$Bar_train3_cat)
data$Bar_train4_cat <- as.factor(data$Bar_train4_cat)
data$Bar_train5_cat <- as.factor(data$Bar_train5_cat)
data$Bar_train6_cat <- as.factor(data$Bar_train6_cat)
data$Bar_train7_cat <- as.factor(data$Bar_train7_cat)
data$Bar_train8_cat <- as.factor(data$Bar_train8_cat)
data$Bar_train9_cat <- as.factor(data$Bar_train9_cat)
data$Bar_train1_cat <- factor(data$Bar_train1_cat, c("Disagree","Neutral","Agree"))
data$Bar_train2_cat <- factor(data$Bar_train2_cat, c("Disagree","Neutral","Agree"))
data$Bar_train3_cat <- factor(data$Bar_train3_cat, c("Disagree","Neutral","Agree"))
data$Bar_train4_cat <- factor(data$Bar_train4_cat, c("Disagree","Neutral","Agree"))
data$Bar_train5_cat <- factor(data$Bar_train5_cat, c("Disagree","Neutral","Agree"))
data$Bar_train6_cat <- factor(data$Bar_train6_cat, c("Disagree","Neutral","Agree"))
data$Bar_train7_cat <- factor(data$Bar_train7_cat, c("Disagree","Neutral","Agree"))
data$Bar_train8_cat <- factor(data$Bar_train8_cat, c("Disagree","Neutral","Agree"))
data$Bar_train9_cat <- factor(data$Bar_train9_cat, c("Disagree","Neutral","Agree"))

#merge data
com_data <- merge(x=as_data, y=data, by="Respondent")
com_data$Code <- as.factor(com_data$Code)

#Import function to perform correspondence analysis
source("C:/Users/dxv632/OneDrive - University of Birmingham/_PhD/_Parts/Analysis/AssociationAnalysis/Analysis_function.R")



#Correlation table for behaviour-affect
cor_mat <- matrix(data=NA, ncol=6, nrow=length(modes)^2)
colnames(cor_mat) <- c("usage", "mode","Kendall's Tau", "p", "Spearman's Rho", "p")
for(m in 1:5){
  modeuse <- paste0("Use_",modes[[m]])
  for(k in 1:5){
    spearman_result <- cor.test(com_data[com_data$Mode==modes[[k]],"Valence"], as.numeric(com_data[com_data$Mode==modes[[k]], modeuse]), method="spearman")
    kendall_result <- cor.test(com_data[com_data$Mode==modes[[k]],"Valence"], as.numeric(com_data[com_data$Mode==modes[[k]], modeuse]), method="kendall")
    cor_mat[(m-1)*5+k,1] <- modeuse
    cor_mat[(m-1)*5+k,2] <- modes[[k]]
    cor_mat[(m-1)*5+k,3] <- kendall_result$estimate
    cor_mat[(m-1)*5+k,4] <- kendall_result$p.value
    cor_mat[(m-1)*5+k,5] <- spearman_result$estimate
    cor_mat[(m-1)*5+k,6] <- spearman_result$p.value
  }
}
write.csv(cor_mat, file = "cor_mat.csv")

#Correlation table for behaviour-affect based on average valence instead of per association
cor_mat2 <- matrix(data=NA, ncol=6, nrow=length(modes)^2)
colnames(cor_mat2) <- c("usage", "mode","Kendall's Tau", "p", "Spearman's Rho", "p")
for(m in 1:5){
  modeuse <- paste0("Use_",modes[[m]])
  for(k in 1:5){
    modeval <- paste0("Valence_",modes[[k]],"_avg")
    spearman_result <- cor.test(data[,modeval], as.numeric(data[, modeuse]), method="spearman")
    kendall_result <- cor.test(data[,modeval], as.numeric(data[, modeuse]), method="kendall")
    cor_mat2[(m-1)*5+k,1] <- modeuse
    cor_mat2[(m-1)*5+k,2] <- modes[[k]]
    cor_mat2[(m-1)*5+k,3] <- kendall_result$estimate
    cor_mat2[(m-1)*5+k,4] <- kendall_result$p.value
    cor_mat2[(m-1)*5+k,5] <- spearman_result$estimate
    cor_mat2[(m-1)*5+k,6] <- spearman_result$p.value
  }
}
write.csv(cor_mat2, file = "cor_mat2.csv")

#Correlation table for affect based on average valence instead of per association
cor_mat3 <- matrix(data=NA, ncol=6, nrow=length(modes)^2)
colnames(cor_mat3) <- c("usage", "mode","Kendall's Tau", "p", "Spearman's Rho", "p")
for(m in 1:5){
  modeval <- paste0("Valence_",modes[[m]],"_avg")
  for(k in 1:5){
    modeval2 <- paste0("Valence_",modes[[k]],"_avg")
    spearman_result <- cor.test(data[,modeval], data[, modeval2], method="spearman")
    kendall_result <- cor.test(data[,modeval], data[, modeval2], method="kendall")
    cor_mat3[(m-1)*5+k,1] <- modes[[m]]
    cor_mat3[(m-1)*5+k,2] <- modes[[k]]
    cor_mat3[(m-1)*5+k,3] <- kendall_result$estimate
    cor_mat3[(m-1)*5+k,4] <- kendall_result$p.value
    cor_mat3[(m-1)*5+k,5] <- spearman_result$estimate
    cor_mat3[(m-1)*5+k,6] <- spearman_result$p.value
  }
}
write.csv(cor_mat3, file = "cor_mat3.csv")


