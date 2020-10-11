rm(list=ls())

#setwd("directory where data file is saved")
fulldata <- read.csv("ames.csv") #Read in all 1460 observations

fulldata <- fulldata[,-1] #Drop ID variable

set.seed(1) #Insert your own project number here
#Example: If your project number is 40, run the line set.seed(40)

my_data <- fulldata[sample(1:nrow(fulldata), 1300, replace=FALSE), ]

write.csv(my_data, 'my_ames.csv')