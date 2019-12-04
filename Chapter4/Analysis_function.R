#================Analysis function=======================
#Creates
#- Chi-square matrix for relationships between variable and 'free associations'
#- Correspondence analysis moonplot

analysis_moon <- function(v){
  library(flipDimensionReduction)
  library(r2d3)
  
  #com_data$Code <- as.factor(com_data$Code_name)
  mat <- matrix(data=NA, nrow = length(modes), ncol = 6)
  colnames(mat) <- c("mode","Chi-sq","df","p", "simulated p", "Number of codes")
  com_data[,v] <- as.factor(com_data[,v])
  inertias <- list()
  for(i in 1:length(modes)){
    data_mode <- subset(com_data, com_data$Mode==modes[[i]] & com_data$Code!="0") #select data with the mode
    data_mode$Code <- as.factor(as.character(data_mode$Code)) #Recalibrate factor to new number of factors
    mytable <- table(data_mode$Code, data_mode[,v])#Create cross-table
    x <- apply(mytable[,1:length(levels(com_data[,v]))], MARGIN = 1, sum) 
    x <- x>20 #select cases with more than 20 observations
    ct <- chisq.test(mytable[which(x),])#Chi square test for independence
    cts <- chisq.test(mytable[which(x),], simulate.p.value = TRUE, B=10000)
    mat[i,1] <- modes[[i]]
    mat[i,2] <- ct$statistic
    mat[i,3] <- ct$parameter
    mat[i,4] <- ct$p.value
    mat[i,5] <- cts$p.value
    mat[i,6] <- sum(x)
    if((cts$p.value<0.05) && (length(levels(com_data[,v]))>2)){ #do correspondence analysis if significant relationship and number of categories are more than two (at least two dimensions)
      data_mode_sub <- subset(data_mode, data_mode$Code %in% rownames(mytable[which(x),]))
      data_mode_sub$Code <- as.factor(as.character(data_mode_sub$Code)) #Recalibrate factor to new number of factors
      catable <- table(data_mode_sub[,v], data_mode_sub$Code)
      #correspondence analysis
      res.ca <- CorrespondenceAnalysis(catable,
                                       normalization = "Row principal",
                                       output = "Moonplot")
      filename <- paste0("C:/Users/dxv632/OneDrive - University of Birmingham/_PhD/_Parts/Analysis/AssociationAnalysis/Plots/CA_moon_",v,"_",modes[[i]],".png")
      print(modes[[i]])
      print(res.ca)
      # save_d3_png(print(res.ca), filename,
      #              width = 400,
      #              height = 400,
      #              delay = 0.5)
      inertias[[i]] <- summary(res.ca$original)$scree
    }
  }
  print(mat)
}