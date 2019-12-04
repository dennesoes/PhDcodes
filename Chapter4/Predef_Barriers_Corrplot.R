# Create a correlation plot for correlations between all ratings of predefined barriers
library(corrplot)

data <- read.csv(file="//its-rds/2017/tightmr-transport-walking/Data_analysis2.csv", header=TRUE, sep=",")

corrplot(cor(na.omit(data[,84:112]), method = "kendall"), method="square", type="lower", diag=F,
         tl.pos="n", addgrid.col="#808080")

