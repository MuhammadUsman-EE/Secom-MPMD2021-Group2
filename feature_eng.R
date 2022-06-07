#New Script for phase 2
# === PREPARE SYSTEM
# set language to English
Sys.setenv(LANG = "en")

# Checking if pacman library installed or not
if (!require("pacman")) install.packages("pacman")

# load pacman
library(pacman)


p_load('tidyverse')
p_load('tidyselect')
p_load("ggplot2")
p_load("dplyr")
p_load("corrplot")
p_load("reshape2")
p_load("data.table")
p_load("caret")
p_load('reshape2')
p_load("outliers")
p_load("DescTools")


# Importing SECOM dataset - Directly from Online Repository
secom.data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data")
secom.label<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data")
colnames(secom.data)<-paste("Feature", 1:ncol(secom.data), sep = "_")
colnames(secom.label)<-c("Status", "Timestamp")
secom<-cbind(secom.label,secom.data)
sum(is.na(secom))
nrow(secom)

# Split the dataset with respect to class variables proportions (ratio 14:1)
## generates indexes for randomly splitting the data into training and test sets
# Setting seed so that the data is replicable.
set.seed(12)
# Spliting data - Training Data 80% and Test Data 20%
secom.train_index<-createDataPartition(secom$Status, times = 1,p = 0.8, list = FALSE)
## define the training and test sets by using above index
secom.training<-secom[secom.train_index,]
#dopping timestamp
secom.training <- secom.training %>% select(-Timestamp)
secom.test<-secom[-secom.train_index,]

secom.training.label <- secom.training$Status
secom.training <- secom.training %>% select(-Status)

#check characteristics 
table(secom.training.label)
table(secom.test$Status)


#drop columns that have one unique value
nzv <- nearZeroVar(secom.training)
filteredDescr <- secom.training[, -nzv]
#secom training has 590, filtered 463

#drop features with 45% or more missing values 
filterednan <- filteredDescr[, -which(colMeans(is.na(filteredDescr)) > 0.45)]
length(filterednan)

#3S boundaries shift
# Scale the selected column
#####Outlier Identification or treatment#####

findoutliers <- function(filterednan) {
  # Find the outliers in the scaled column
  flag <- ifelse(test = scale(filterednan) > 3 |  scale(filterednan) < -3, yes = 1, no = 0)
  result <- which(flag == 1)
  length(result)
}
outliers <- sapply(filterednan, findoutliers)
outliers <- data.frame(outliers)
print(outliers)
sum(outliers)
View(outliers)

#bringing the outliers into the 3s boundary
outlier_replacement <- apply(filterednan, FUN = Winsorize, MARGIN = 2, probs = c(0.001, 0.999), na.rm = TRUE)
length(outlier_replacement)
length(outliers)

#test if there are still outliers 
outlier_test <- sapply(outlier_replacement, findoutliers)
outlier_test <- data.frame(outlier_test)
sum(outlier_test)
