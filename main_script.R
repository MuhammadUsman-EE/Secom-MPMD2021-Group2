if(!require("pacman")) install.packages("pacman")
library("pacman")
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
kable(head(secom[,1:8],15))
dim(secom.data)
View(secom)
sum(is.na(secom))

#we create an empty data frame to run our loop to identify
#NA values for each Feature, we will add as well mean, median
#first quartile, third quartile, std.dev. and percentage of 
#NA'S per feature
NA_Table <- data.frame()
for(i in colnames(secom)){
  a <- subset(secom, select = i)
  b <- sum(is.na(a))
  print(i)
  print(b)
  c <- data.frame(i, b)
  NA_Table <- rbind(NA_Table,c)
}

# The SECOM dataset includes 1567 rows (observations) with 590 columns 
#representing 590 features/signals collected from sensors, together with 
#the labels representing pass (-1) / fail (1) yield for in house line testing 
#and associated date time stamp.  
# Challenges of SECOM dataset and Data Preparation Steps
## 1. Split the dataset into Training and Test set
# data frame of Frequency of Pass and Fail
secom.status<-data.frame(table(secom$Status,dnn = c("Status")))

# Bar chart of Frequency of Pass and Fail
par(las=2)
secom.barplot.1<-barplot(table(secom$Status),horiz = TRUE,names.arg = c("Pass","Fail"), col = c("limegreen","azure3"), xlim = c(0,1600),main = "Frequency of Pass and Fail")
text(secom.barplot.1,x = table(secom$Status),labels = table(secom$Status), pos = 4)
<<<<<<< HEAD
=======
secom.barplot.1

##commenting from new branch
##testing Gazal Branch
>>>>>>> 35f439692adc35b14c0831fbe0125c4a41099023
secom.barplot.1