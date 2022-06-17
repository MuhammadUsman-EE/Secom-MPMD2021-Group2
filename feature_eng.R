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
p_load("plotROC")
p_load("mlbench")
p_load("MLeval")
p_load("Boruta")
p_load("glmnet")


# Importing SECOM dataset - Directly from Online Repository
secom.data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data")
secom.label<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data")
colnames(secom.data)<-paste("Feature", 1:ncol(secom.data), sep = "_")
colnames(secom.label)<-c("Status", "Timestamp")
secom<-cbind(secom.label,secom.data)
sum(is.na(secom))
nrow(secom)

secom$Status[which(secom$Status == "-1")] = "No"
secom$Status[which(secom$Status == "1")] = "Yes"
secom$Status <- as.factor(secom$Status)

# Split the dataset with respect to class variables proportions (ratio 14:1)
## generates indexes for randomly splitting the data into training and test sets
# Setting seed so that the data is replicable.
set.seed(12)
# Spliting data - Training Data 80% and Test Data 20%
secom.train_index<-createDataPartition(secom$Status, times = 1,p = 0.8, list = FALSE)
## define the training and test sets by using above index
secom.training<-secom[secom.train_index,]
#dropping timestamp
secom.test<-secom[-secom.train_index,]

secom.training.label <- secom.training$Status
secom.training.Timestamp <- secom.training$Timestamp
secom.training <- secom.training %>% select(-c(Status, Timestamp))

#check characteristics 
table(secom.training.label)
table(secom.test$Status)
dim(secom.training)

#drop columns that have one unique value
nzv <- nearZeroVar(secom.training)
filteredDescr <- secom.training[, -nzv]
dim(filteredDescr)

#secom training has 590, filtered 463

nzv2 <-nearZeroVar(secom.training,freqCut= 95/5,uniqueCut=10)
nzv2<-secom.training[, -nzv]


#drop features with 50% or more missing values 
filterednan <- filteredDescr[, -which(colMeans(is.na(filteredDescr)) > 0.45)]
length(filterednan)

# Replace outliers with 3S boundaries

outlier_replaced = data.frame(matrix(NA, nrow = nrow(filterednan)))  


for(column_name in colnames(filterednan)) {
  selected_col <- subset(filterednan, select = column_name)
  selected_col <- selected_col[, column_name]
  mean_col <- mean(selected_col, na.rm = T)
  sd_col <- sd(selected_col, na.rm = T)
  
  
  c <- ifelse(selected_col > (mean_col + (3*sd_col)), mean_col + (3*sd_col), selected_col)
  c <- ifelse(c < (mean_col - (3*sd_col)), mean_col - (3*sd_col), selected_col)
  
  c <- as.data.frame(c)
  colnames(c) <- column_name
  
  outlier_replaced <- outlier_replaced %>% add_column(c)
  
}

outlier_replaced <- outlier_replaced %>% select(-c(1))

# Scaling features using Min Max Scaling method (0 - 1)
#process <- preProcess(outlier_replaced, method=c("range"))
#df <- predict(process, outlier_replaced)

df <- outlier_replaced


# Calling Functions from other files
source("knn_imput.R")
source("mice_imput.R")
source("boruta.R")
source("rfe.R")
source('lasso.R')
source("df_balance.R")

# KNN Imputation
secom_imputed <- knn_impute(df)

#MICE Imputation
secom_imputed <- mice_impute(df)
load(df_mice2.rda)

# Boruta Feature Selection
secom_features_selected <- boruta_select(secom.imputed, secom.training.label)

# RFE Features Selection
secom_features_selected <- rfe_select(secom.imputed, secom.training.label)

# Lasso Feature Selection
secom_features_selected <- lasso_select(secom.imputed, secom.training.label)

# Logistic Regression - 5 - Fold Results
log_model_results <- log_model_run(features_selected = secom_features_selected, y = secom.training.label)
res <- evalm(log_model_results)

# Data balancing using ROSE
secom_rose_balancing <- secom_balance.rose(secom_features_selected, secom.training.label, 0.5)
table(secom_rose_balancing$Status)

# Data balacning using SMOTE
secom_smote_balancing <- secom_balance.smote(secom_features_selected, secom.training.label, 14, 1)
table(secom_smote_balancing$Status)








