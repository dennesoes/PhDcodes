#Analysis of predefined barriers
library(tidyverse)
library(reshape2)
library(scales)
library(RColorBrewer)

# Load data
data <- read.csv(file="//its-rds/2017/tightmr-transport-walking/Data_analysis2.csv", header=TRUE, sep=",")
data$ID <- as.factor(data$ID)
data <- rename(data,Respondent=ID, Attit_Birm_car=Attit_Birm_drive,Attit_Birm_cycle=Attit_Birm_cycling,
               Use_car=Use_drive,Use_cycle=Use_cycling,Attit_train=Attit_Train,Attit_bus=Attit_Bus)

# Define barrier columns
Walk_barriers <- paste0("Bar_walk",seq(1:11))
Bus_barriers <- paste0("Bar_bus", seq(1:9))
Train_barriers <- paste0("Bar_train", seq(1:9))
Barriers <- c(Walk_barriers, Bus_barriers, Train_barriers)


# Kruskal Wallis tests ----------------------------------------------------
library(FSA)
library(rcompanion)

# Prepare matrix for test results
kruskal_mat <- matrix(data = NA, nrow = length(Barriers), ncol = 17)
colnames(kruskal_mat) <- c("Barrier", "BO med", "MO med", "CV med", "SH med", "H", "p", 
                           "BO-CV", "BO-MO", "CV-MO", "BO-SH", "CV-SH", "MO-SH", 
                           "BO", "MO", "CV",  "SH")

# Do the tests for each barrier
for(b in 1:length(Barriers)){
  
  # Kruskal-Wallis test
  kruskal_result <- kruskal.test(get(Barriers[b]) ~ Neighbourhood, data = data)
  
  # Post-hoc Dunn test for multiple comparisons
  DT <- dunnTest(get(Barriers[b]) ~ Neighbourhood, data = data, method = "bh") # Method bh adjusts p-values for multiple comparisons
  if(any(DT$res$P.adj < 0.05)){
    CL <- cldList(P.adj ~ Comparison, data = DT$res, threshold = 0.05)
  }else{
    CL$Letter <- NA
  }
  
  # Place results in the result matrix
  kruskal_mat[b,1] <- Barriers[b]
  kruskal_mat[b,2] <- median(unlist(select(subset(data, data$Neighbourhood=="Bournville"), Barriers[b])), na.rm = T)
  kruskal_mat[b,3] <- median(unlist(select(subset(data, data$Neighbourhood=="Moseley"), Barriers[b])), na.rm = T)
  kruskal_mat[b,4] <- median(unlist(select(subset(data, data$Neighbourhood=="Castle Vale"), Barriers[b])), na.rm = T)
  kruskal_mat[b,5] <- median(unlist(select(subset(data, data$Neighbourhood=="Small Heath"), Barriers[b])), na.rm = T)
  kruskal_mat[b,6] <- kruskal_result$statistic
  kruskal_mat[b,7] <- kruskal_result$p.value
  kruskal_mat[b,8] <- DT$res$P.adj[1]
  kruskal_mat[b,9] <- DT$res$P.adj[2]
  kruskal_mat[b,10] <- DT$res$P.adj[3]
  kruskal_mat[b,11] <- DT$res$P.adj[4]
  kruskal_mat[b,12] <- DT$res$P.adj[5]
  kruskal_mat[b,13] <- DT$res$P.adj[6]
  kruskal_mat[b,14] <- ifelse(!is.na(CL$Letter[1]), levels(CL$Letter)[CL$Letter[1]], NA)
  kruskal_mat[b,15] <- ifelse(!is.na(CL$Letter[1]), levels(CL$Letter)[CL$Letter[3]], NA)
  kruskal_mat[b,16] <- ifelse(!is.na(CL$Letter[1]), levels(CL$Letter)[CL$Letter[2]], NA)
  kruskal_mat[b,17] <- ifelse(!is.na(CL$Letter[1]), levels(CL$Letter)[CL$Letter[4]], NA)
  
}

# Save result matrix
write.csv(kruskal_mat, "C:/Users/dxv632/OneDrive - University of Birmingham/_PhD/_Parts/Questionnaire/PredefinedBarriers/Kruskal/Kruskal_Dunn_results.csv", row.names = F)



# Boxplots for all individual barriers ------------------------------------

## Walk barriers

# Get overview of mean ratings to define order for plot
data %>% 
  select(starts_with("Bar_walk")) %>%
  gather(Barrier, Rating) %>%
  group_by(Barrier) %>%
  summarise(Rating = mean(Rating, na.rm=T)) %>%
  arrange(desc(Rating))

# Create boxplot for ratings per barrier, in descending order
data %>% 
  select(starts_with("Bar_walk")) %>%
  gather(Barrier, Rating) %>%
  ggplot(aes(x = Barrier, y=Rating)) +
  geom_boxplot(fill = "#dddddd") +
  scale_y_continuous(limits = c(0, 7), expand = expand_scale(add = c(-1, 0))) +
  scale_x_discrete(limits = c("Bar_walk4", "Bar_walk8", "Bar_walk7", "Bar_walk2", "Bar_walk11", "Bar_walk5", "Bar_walk9", "Bar_walk6", "Bar_walk1", "Bar_walk3", "Bar_walk10"),
                   labels = c("I feel unsafe when I walk during the day", 
                              "I am too lazy to walk", 
                              "I often feel too tired to walk", 
                              "I do not feel safe in traffic when I walk", 
                              "I'd like to have more places to rest", 
                              "The walking routes are unpleasant", 
                              "I won't walk when it is raining", 
                              "The pavements are of bad quality", 
                              "Walking often takes too much time", 
                              "I feel unsafe when I walk at night", 
                              "I won't walk when I need to carry some luggage")) +
  coord_flip() +
  theme_bw() +
  labs(x = "Rating") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14))


## Bus barriers

data %>% 
  select(starts_with("Bar_bus")) %>%
  gather(Barrier, Rating) %>%
  group_by(Barrier) %>%
  summarise(Rating = mean(Rating, na.rm=T)) %>%
  arrange(desc(Rating))

data %>% 
  select(starts_with("Bar_bus")) %>%
  gather(Barrier, Rating) %>%
  ggplot(aes(x = Barrier, y=Rating)) +
  geom_boxplot(fill = "#dddddd") +
  scale_y_continuous(limits = c(0, 7), expand = expand_scale(add = c(-1, 0))) +
  scale_x_discrete(limits = c("Bar_bus9", "Bar_bus1", "Bar_bus7", "Bar_bus3", "Bar_bus8", "Bar_bus4", "Bar_bus6", "Bar_bus5", "Bar_bus2"),
                   labels = c("I suffer from travel sickness on the bus",
                              "The distance to the bus stop is too far",
                              "The bus system is too complicated",
                              "I do not feel safe on buses",
                              "I don't like the other people on the bus",
                              "I won't take the bus if I need to carry some luggage",
                              "Buses are too expensive",
                              "Buses are often too crowded for me",
                              "Travelling by bus takes too much time for my trips")) +
  coord_flip() +
  theme_bw() +
  labs(x = "Rating") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14))



## Train barriers

data %>% 
  select(starts_with("Bar_train")) %>%
  gather(Barrier, Rating) %>%
  group_by(Barrier) %>%
  summarise(Rating = mean(Rating, na.rm=T)) %>%
  arrange(desc(Rating))

data %>% 
  select(starts_with("Bar_train")) %>%
  gather(Barrier, Rating) %>%
  ggplot(aes(x = Barrier, y=Rating)) +
  geom_boxplot(fill = "#dddddd") +
  scale_y_continuous(limits = c(0, 7), expand = expand_scale(add = c(-1, 0))) +
  scale_x_discrete(limits = c("Bar_train9", "Bar_train3", "Bar_train4", "Bar_train7", "Bar_train8", "Bar_train2", "Bar_train5", "Bar_train1", "Bar_train6"),
                   labels = c("I suffer from travel sickness on the train",
                              "I do not feel safe on trains and at stations",
                              "I won't take the train if I need to carry some luggage",
                              "The railway system is too complicated",
                              "I don't like the other people on the train",
                              "The distance to the bus stop is too far",
                              "Travelling by train takes too much time for my trips",
                              "Trains are often too crowded for me",
                              "Trains are too expensive")) +
  coord_flip() +
  theme_bw() +
  labs(x = "Rating") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14))



# Logistic regression model for ratings -----------------------------------------------
library(rms)

# Load data about public transport convenience
data_convenience_PT <- read.csv("Z:/Spatial/BusDepartures/convenience_pt.csv")

# Select relevant data and dummy code some of the variables
reg_data <- data %>%
  mutate(Respondent = as.numeric(as.character(Respondent))) %>%
  left_join(data_convenience_PT, by = c("Respondent" = "RespondentID")) %>%
  select(Respondent, Neighbourhood, Income, Avail_car, Gender, Ethnicity, Avail_cycle, 
         Avail_Pt, Convenience_bus, Convenience_train, starts_with("Bar_")) %>%
  mutate_at(vars(starts_with("Bar_")), ~fct_collapse(factor(.), "Agree" = c("5", "6", "7"), 
                                                     "Disagree" = c("1", "2", "3", "4"))) %>%
  mutate(Deprivation = fct_collapse(factor(Neighbourhood), Deprived = c("Small Heath", "Castle Vale"), 
                                 Nondeprived = c("Moseley", "Bournville")),
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

dd <- datadist(reg_data)
options(datadist="dd")

# Prepare result storage for all barriers
totalresult <- data.frame()

# Create a logistic regression model for each barrier
for(b in Barriers){
  # Compute model
  model <- lrm(get(b) ~ Deprivation + 
                 Convenience_bus1 + Convenience_bus2 +
                 Convenience_train1 + Convenience_train2 +
                 Avail_Pt + 
                 Gender + 
                 Income1 + Income2 + Income3 +
                 Ethnicity + Avail_cycle +
                 Avail_car,
               data=reg_data, x=T, y=T)
  
  # Get results using clustered error terms
  modelres <- robcov(model, cluster = reg_data$Neighbourhood)
  
  # Prepare results matrix
  mres_mat <- matrix(data = NA, ncol = 8, nrow = length(modelres$coefficients)+1)
  colnames(mres_mat) <- c("Barrier", "Variable", "Beta", "SE", "p-value", "Odds ratio", "OR 95% CI lower", "OR 95% CI upper")
  
  # Fill in results matrix
  SE <- sqrt(diag(modelres$var))
  mres_mat[1:nrow(mres_mat)-1,1] <- b
  mres_mat[1:nrow(mres_mat)-1,2] <- names(modelres$coefficients)
  mres_mat[1:nrow(mres_mat)-1,3] <- modelres$coefficients
  mres_mat[1:nrow(mres_mat)-1,4] <- SE
  mres_mat[1:nrow(mres_mat)-1,5] <- round(2*(1-pnorm(abs(modelres$coefficients/SE))), 3)
  mres_mat[1:nrow(mres_mat)-1,6] <- round(exp(modelres$coefficients), 2)
  mres_mat[1:nrow(mres_mat)-1,7] <- round(exp(modelres$coefficients-1.96*SE), 2)
  mres_mat[1:nrow(mres_mat)-1,8] <- round(exp(modelres$coefficients+1.96*SE), 2)
  mres_mat[nrow(mres_mat),1] <- b
  mres_mat[nrow(mres_mat),2] <- paste("N = ", modelres$stats[[1]])
  mres_mat[nrow(mres_mat),3] <- paste("Nagelkerke R2 = ", modelres$stats[[10]])
  mres_mat[nrow(mres_mat),4] <- paste("df = ", modelres$stats[[4]])
  mres_mat[nrow(mres_mat),5] <- paste("Model chisq = ", modelres$stats[[3]])
  mres_mat[nrow(mres_mat),6] <- paste("p = ", modelres$stats[[5]])
  mres_mat[nrow(mres_mat),7] <- AIC(modelres)
  mres_mat[nrow(mres_mat),8] <- ""
  
  # Add results of barrier model to the overall results table for all barriers
  totalresult <- rbind(totalresult, mres_mat)
}

# Write model results into file
file.name <- paste0("C:/Users/dxv632/OneDrive - University of Birmingham/_PhD/_Parts/Questionnaire/PredefinedBarriers/Logistic_regression/Chapter/all_models.csv")
write.csv(totalresult, file = file.name, row.names = F)


# Plots of logistic regression results ------------------------------------

# Walk barriers
totalresult %>%
  mutate(Odds = as.numeric(as.character(Odds)),
         OddsLow = as.numeric(as.character(OddsLow)),
         OddsUpper = as.numeric(as.character(OddsUpper)),
         OddsUpper = ifelse(OddsUpper > 10, 10, OddsUpper), #Limit to ten to reduce plot size for outliers
         pvalue = ifelse(as.numeric(as.character(pvalue))<0.05, "<0.05", ">0.05")) %>%
  filter(Barrier %in% Walk_barriers) %>%
  ggplot(aes(x = Odds, y = Barrier)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = OddsUpper, xmin = OddsLow), size = .5, height = 
                     .2, color = "gray50") +
    geom_point(aes(colour = pvalue), size = 2.5) +
  facet_wrap(~ Variable, ncol = 5) +
    scale_x_continuous(limits = c(0,10), expand = expand_scale(add = c(0, 0))) +
  theme_bw()+
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    ylab("") +
    xlab("Odds ratio") +
  scale_y_discrete(limits = c("Bar_walk4", "Bar_walk8", "Bar_walk7", "Bar_walk2", "Bar_walk11", "Bar_walk5", "Bar_walk9", "Bar_walk6", "Bar_walk1", "Bar_walk3", "Bar_walk10"),
                   labels = c("I feel unsafe when I walk during the day", 
                              "I am too lazy to walk", 
                              "I often feel too tired to walk", 
                              "I do not feel safe in traffic when I walk", 
                              "I'd like to have more places to rest", 
                              "The walking routes are unpleasant", 
                              "I won't walk when it is raining", 
                              "The pavements are of bad quality", 
                              "Walking often takes too much time", 
                              "I feel unsafe when I walk at night", 
                              "I won't walk when I need to carry some luggage"))

# Train barriers
totalresult %>%
  mutate(Odds = as.numeric(as.character(Odds)),
         OddsLow = as.numeric(as.character(OddsLow)),
         OddsUpper = as.numeric(as.character(OddsUpper)),
         OddsUpper = ifelse(OddsUpper > 10, 10, OddsUpper),
         pvalue = ifelse(as.numeric(as.character(pvalue))<0.05, "<0.05", ">0.05")) %>%
  filter(Barrier %in% Train_barriers) %>%
  ggplot(aes(x = Odds, y = Barrier)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = OddsUpper, xmin = OddsLow), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(aes(colour = pvalue), size = 2.5) +
  facet_wrap(~ Variable, ncol = 5) +
  scale_x_continuous(limits = c(0,10), expand = expand_scale(add = c(0, 0))) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  ylab("") +
  xlab("Odds ratio") +
  scale_y_discrete(limits = c("Bar_train9", "Bar_train3", "Bar_train4", "Bar_train7", "Bar_train8", "Bar_train2", "Bar_train5", "Bar_train1", "Bar_train6"),
                   labels = c("I suffer from travel sickness on the train",
                              "I do not feel safe on trains and at stations",
                              "I won't take the train if I need to carry some luggage",
                              "The railway system is too complicated",
                              "I don't like the other people on the train",
                              "The distance to the bus stop is too far",
                              "Travelling by train takes too much time for my trips",
                              "Trains are often too crowded for me",
                              "Trains are too expensive"))

# Bus barriers
totalresult %>%
  mutate(Odds = as.numeric(as.character(Odds)),
         OddsLow = as.numeric(as.character(OddsLow)),
         OddsUpper = as.numeric(as.character(OddsUpper)),
         OddsUpper = ifelse(OddsUpper > 10, 10, OddsUpper),
         pvalue = ifelse(as.numeric(as.character(pvalue))<0.05, "<0.05", ">0.05")) %>%
  filter(Barrier %in% Bus_barriers) %>%
  ggplot(aes(x = Odds, y = Barrier)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = OddsUpper, xmin = OddsLow), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(aes(colour = pvalue), size = 2.5) +
  facet_wrap(~ Variable, ncol = 5) +
  scale_x_continuous(limits = c(0,10), expand = expand_scale(add = c(0, 0))) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  ylab("") +
  xlab("Odds ratio") +
  scale_y_discrete(limits = c("Bar_bus9", "Bar_bus1", "Bar_bus7", "Bar_bus3", "Bar_bus8", "Bar_bus4", "Bar_bus6", "Bar_bus5", "Bar_bus2"),
                   labels = c("I suffer from travel sickness on the bus",
                              "The distance to the bus stop is too far",
                              "The bus system is too complicated",
                              "I do not feel safe on buses",
                              "I don't like the other people on the bus",
                              "I won't take the bus if I need to carry some luggage",
                              "Buses are too expensive",
                              "Buses are often too crowded for me",
                              "Travelling by bus takes too much time for my trips"))
