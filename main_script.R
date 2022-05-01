if(!require("pacman")) install.packages("pacman")
library("pacman")
library('tidyverse')
library('tidyselect')
p_load("ggplot2")
p_load("knitr")
p_load("caret")
p_load("raster")
p_load("scales")
p_load("Boruta")
p_load("randomForest")
p_load("DMwR")
p_load("e1071")
p_load("ROSE")
p_load("ROCR")
p_load("dplyr")
p_load("corrplot")
p_load("RCurl")
p_load("Matrix") 
p_load("stats")
p_load("car")
p_load("MASS")
p_load("fBasics")
p_load("reshape2")
p_load("data.table")

# Overview of SECOM dataset
# Getting SECOM dataset
secom.data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data")
secom.label<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data")
colnames(secom.data)<-paste("Feature", 1:ncol(secom.data), sep = "_")
colnames(secom.label)<-c("Status", "Timestamp")
secom<-cbind(secom.label,secom.data)
sum(is.na(secom))

#we create an empty data frame to run our loop to identify
#NA values for each Feature, we will add as well mean, median
#first quartile, third quartile, std.dev. and percentage of 
#NA'S per feature
NA_Table <- data.frame()


for(i in colnames(secom)){
  #i = "Status"
  a <- subset(secom, select = i)
  a <- a[, i]
  b <- sum(is.na(a))
  print(i)
  print(b)
  View(i)
  
# Caclulating percentage of null values per feature
  if(b != 0) {
    perc_null <- (b/length(a))*100
    perc_null <- round(perc_null, digits = 2)
  } else {
    perc_null = 0
  }
  
  print(perc_null)
  
  
# Descriptives (mean, median, stdve, q1, q3, 3s rules
  if(is.numeric(a)){
    m <- mean(a,na.rm = T)
    m <- round(m, digits = 2)
    st <- sd(a, na.rm = T)
    st <- round(st, digits = 2)
    q1 <- quantile(a,0.25, na.rm = T)
    q3 <- quantile(a,0.75, na.rm =T)
  } else {
    m <- NA
    st <- NA
    q1 <- NA
    q3 <- NA
  }
  print(m)
  print(st)
  print(q1)
  print(q3)
  
  c <- data.frame(i, b, perc_null,m,st,q1,q3)
  NA_Table <- rbind(NA_Table,c)
  
}

# remove(NA_Table)

# Testing Function + Outliers
NA_Table <- data.frame()

for(i in colnames(secom)){
  a <- subset(secom, select = i)
  b <- sum(is.na(a))
  
  
  print(outliers)
  print(i)
  print(b)
  c <- data.frame(i, b,outliers)
  NA_Table <- rbind(NA_Table,c)
}

# Outlier count

FindOutliers <- function(secom.data) {
  flag<- ifelse(test = scale(secom.data) > 3 | scale(secom.data)< -3, yes = 1, no = 0)
  result <- which(flag == 1)
  length(result)
}
outliers <- sapply(secom.data, FindOutliers)
outliers <- data.frame(outliers)

# The SECOM dataset includes 1567 rows (observations) with 590 columns 
# representing 590 features/signals collected from sensors, together with 
# the labels representing pass (-1) / fail (1) yield for in house line testing 
# and associated date time stamp.  
# Challenges of SECOM dataset and Data Preparation Steps
# 1. Split the dataset into Training and Test set
# data frame of Frequency of Pass and Fail

secom.status<-data.frame(table(secom$Status,dnn = c("Status")))

# Bar chart of Frequency of Pass and Fail
par(las=2)
secom.barplot.1<-barplot(table(secom$Status),horiz = TRUE,names.arg = c("Pass","Fail"), col = c("limegreen","azure3"), xlim = c(0,1600),main = "Frequency of Pass and Fail")
text(secom.barplot.1,x = table(secom$Status),labels = table(secom$Status), pos = 4)

# Split the dataset with respect to class variables proportions (ratio 14:1)
## generates indexes for randomly splitting the data into training and test sets
secom.train_index<-createDataPartition(secom$Status, times = 1,p = 0.8, list = FALSE) # to put 80% of data to training set

## define the training and test sets by using above index
secom.training<-secom[secom.train_index,]
secom.data.train<-secom.training[,-c(1,2)]
secom.test<-secom[-secom.train_index,]
#Done