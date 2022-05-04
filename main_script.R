library('tidyverse')
library('tidyselect')
library("ggplot2")
library("dplyr")
library("corrplot")
library("reshape2")
library("data.table")
library("caret")

# Overview of SECOM dataset
# Getting SECOM dataset
secom.data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data")
secom.label<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data")
colnames(secom.data)<-paste("Feature", 1:ncol(secom.data), sep = "_")
colnames(secom.label)<-c("Status", "Timestamp")
secom<-cbind(secom.label,secom.data)
sum(is.na(secom))
nrow(secom)


Select_null_percentage <- final_column_descriptives$Percentage_NA
Filter_percentage_55 <- Select_null_percentage[Select_null_percentage > 55]  
length(Filter_percentage_55)


#we create an empty data frame to run our loop to identify
#NA values for each Feature, we will add as well mean, median
#first quartile, third quartile, std.dev., percentage of 
#NA'S per feature and unique values per feature
final_column_descriptives <- data.frame()


for(column_name in colnames(secom)){
  
  # Selecting Only the current column in a loop
  selected_col <- subset(secom, select = column_name)
  # Convert column dataframe to vector
  selected_col <- selected_col[, column_name]
  # Count total NA's of the selected column
  SUM_NA <- sum(is.na(selected_col))

  
# Caclulating percentage of NULL values if NULL exist
  if(SUM_NA != 0) {
    Percentage_NA <- (SUM_NA/length(selected_col))*100
    Percentage_NA <- round(Percentage_NA, digits = 2)
  } else {
    Percentage_NA = 0
  }
  

# Descriptives (mean, median, stdve, q1, q3, 3s rules
  if(is.numeric(selected_col)){
    Mean <- mean(selected_col,na.rm = T)
    Mean <- round(Mean, digits = 2)
    Median <- median(selected_col, na.rm = T)
    Median <- round(Median, digits = 2)
    Standard_Deviation <- sd(selected_col, na.rm = T)
    Standard_Deviation <- round(Standard_Deviation, digits = 2)
    Q1 <- quantile(selected_col,0.25, na.rm = T)
    Q1 <- round(Q1, digits = 2)
    Q3 <- quantile(selected_col,0.75, na.rm =T)
    Q3 <- round(Q3, digits = 2)
  } else {
    Average <- NA
    Standard_Deviation <- NA
    Median <- NA
    Q1 <- NA
    Q3 <- NA
  }

  # Finding outliers in columns
  Total_Outliers_3s <- (selected_col > Mean + (3 * Standard_Deviation)) | (selected_col < Mean - (3 * Standard_Deviation))
  Total_Outliers_3s <- sum(Total_Outliers_3s, na.rm = T)
  
  # Finding unique values in each column
  Unique_Values <- length(unique(na.omit(selected_col)))
  
  combined_descrptive <- data.frame(column_name, SUM_NA, Percentage_NA, Mean, Median, Standard_Deviation, Q1,Q3, Total_Outliers_3s, Unique_Values)
  final_column_descriptives <- rbind(final_column_descriptives,combined_descrptive)
  
}

# Finding Duplicate Rows in the Dataframe

c <- secom[!duplicated(as.list(secom))]
duplicate_columns <- ncol(secom) - ncol(secom[!duplicated(as.list(secom))])
print('Number of Duplicate columns')
print(duplicate_columns)

# The SECOM dataset includes 1567 rows (observations) with 590 columns 
# representing 590 features/signals collected from sensors, together with 
# the labels representing pass (-1) / fail (1) yield for in house line testing 
# and associated date time stamp.  

secom.status<-data.frame(table(secom$Status,dnn = c("Status")))


#Plotting histograms
#histogram of nulls values
hist(final_column_descriptives$SUM_NA,
     labels = TRUE, ylim = c(0,20), 
     breaks = 20, 
     main = "Frequency of Null Values in features", 
     xlab = "Number of Missing Values", 
     col = ("limegreen"))

#histogram of missing values percentages
hist(final_column_descriptives$Percentage_NA, 
     labels = TRUE, 
     ylim = c(0,20), 
     xlim = c(0,100), 
     breaks = 20, 
     main = "Percent of Null Values in features", 
     xlab = "Percent of Missing Values", 
     col = ("limegreen"))

#histogram of volatilities
hist((final_column_descriptives$Standard_Deviation),
     ylim = c(0,20), 
     breaks = 100, 
     xlim = c(0,7000), 
     main ="Standard Deviations in the dataset", 
     xlab = "Standard Deviations", 
     col = ("limegreen"))
#LINE to show volatilities 

#histogram of outliers 
hist((final_column_descriptives$Total_Outliers_3s),
     labels = TRUE, 
     ylim = c(0,25), 
     breaks = 50, 
     xlim = c(0,1700), 
     main = "Total number of Outliers based on 3s Rule", 
     xlab = "Number of Outliers",
     col = ("limegreen"))


#count number of features that have 1 unique value
sum(final_column_descriptives$Unique_Values == 1)


# Bar chart of Frequency of Pass and Fail
par(las=2)
secom.barplot.1<-barplot(table(secom$Status),
                         horiz = TRUE,
                         names.arg = c("Pass","Fail"), 
                         col = c("limegreen","azure3"), 
                         xlim = c(0,1600),
                         main = "Frequency of Pass and Fail")

text(secom.barplot.1,
     x = table(secom$Status),
     labels = table(secom$Status), 
     pos = 4)

# Split the dataset with respect to class variables proportions (ratio 14:1)
## generates indexes for randomly splitting the data into training and test sets
secom.train_index<-createDataPartition(secom$Status, times = 1,p = 0.8, list = FALSE) # to put 80% of data to training set

## define the training and test sets by using above index
secom.training<-secom[secom.train_index,]
secom.data.train<-secom.training[,-c(1,2)]
secom.test<-secom[-secom.train_index,]


#Fixed Training and test done new