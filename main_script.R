# === PREPARE SYSTEM
# set language to English
Sys.setenv(LANG = "en")

# Checking if required libraries are installed or not
if (!require("pacman")) install.packages("pacman")
if (!require("smotefamily"))  install.packages("smotefamily")
if (!require("ROSE"))  install.packages("ROSE")
if (!require("performanceEstimation")) install.packages("performanceEstimation")

# load pacman
library(pacman)
#load smotefamily
library(smotefamily)
#load ROSE
library(ROSE)
#load performanceEstimation
library(performanceEstimation)

p_load('tidyverse')
p_load('tidyselect')
p_load("ggplot2")
p_load("dplyr")
p_load("corrplot")
p_load("reshape2")
p_load("data.table")
p_load("caret")
p_load('reshape2')
p_load('DMwR2')


# Importing SECOM dataset - Directly from Online Repository
secom.data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data")
secom.label<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data")
colnames(secom.data)<-paste("Feature", 1:ncol(secom.data), sep = "_")
colnames(secom.label)<-c("Status", "Timestamp")
secom<-cbind(secom.label,secom.data)
sum(is.na(secom))
nrow(secom)

#we create an empty data frame to run our loop to identify
#NA values for each Feature, we will add as well mean, median
#first quartile, third quartile, std.dev., percentage of 
#NA'S per feature, unique values per feature, Outliers by 3s, and Influential Values

# Creating empty datafram to append all descriptives
final_column_descriptives <- data.frame()


for(column_name in colnames(secom.data)) {
  #column_name = 'Feature_39'
  # Selecting Only the current column in a loop
  selected_col <- subset(secom.data, select = column_name)
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
  
  
# Calculating Descriptives (mean, median, stdve, q1, q3)
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
  
  # Finding unique values in each column
  Unique_Values <- length(unique(na.omit(selected_col)))
  
  
  # Normalizingthe selected Column
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  
  selected_col_norm <- range01(selected_col, na.rm = T)
  
  # Finding standard deviation from the Normalized Column
  Standard_Deviation_Norm <- sd(selected_col_norm, na.rm = T)
  
  Standard_Deviation_Norm <- round(Standard_Deviation_Norm, digits = 3)
  
  # Scale the selected column
  selected_col_scaled <- scale(selected_col)
  
  
  # Finding outliers in the scaled column
  Total_Outliers_3s <- (selected_col_scaled > 3) | (selected_col_scaled < -3)
  
  Total_Outliers_3s <- sum(Total_Outliers_3s, na.rm = T)
  
  # Finding Influential Values in the scaled column
  Total_Influential <- (selected_col_scaled > 2 & selected_col_scaled < 3) | (selected_col_scaled < -2 & selected_col_scaled > -3)
  
  Total_Influential <- sum(Total_Influential, na.rm = T)
  
  combined_descrptive <- data.frame(column_name, SUM_NA, Percentage_NA, Mean, Median, Standard_Deviation, Q1,Q3, Standard_Deviation_Norm,Total_Outliers_3s,Total_Influential, Unique_Values)
  final_column_descriptives <- rbind(final_column_descriptives,combined_descrptive)
  
  }

# Finding Duplicate Colummns in the Dataframe
#c <- secom[!duplicated(as.list(secom))]
duplicate_columns <- ncol(secom) - ncol(secom[!duplicated(as.list(secom))])
print('Number of Duplicate columns')
print(duplicate_columns)


# Finding columns with constant values i.e only single unique values
sum(final_column_descriptives$Unique_Values == 1)


# Correlation Matrix

# Create a correlation matrix - Using only pairs that are complete, i.e no missing values
corr_mat <- round(cor(secom.data, use = 'pairwise.complete.obs'),2)


# Create a list of unique column combinations with their correlation from the matrix. Diagonals removed and duplicates combinations removed.
all_comb_correlations <- corr_mat %>%                               
  as.table() %>% as.data.frame() %>%       
  subset(Var1 != Var2) %>% 
  filter(!duplicated(paste0(pmax(as.character(Var1), as.character(Var2)), pmin(as.character(Var1), as.character(Var2))))) %>%
  arrange(desc(Freq))                     


# Percentage of unique combination for different correlation levels.
# No Correlation (0-0.3), Weak  (0.3 - 0.5), Moderate  (0.5 - 0.8), Strong (0.8 - <1), Perfect(1)

no_corr <- all_comb_correlations %>% filter((Freq >= 0 & Freq <= 0.3) | (Freq >= -0.3 & Freq <= 0))
print((nrow(no_corr)/ nrow(all_comb_correlations)) * 100)

weak_corr <- all_comb_correlations %>% filter((Freq >= 0 & Freq <= 0.3) | (Freq >= -0.3 & Freq <= 0))
print((nrow(weak_corr)/ nrow(all_comb_correlations)) * 100)

mod_corr <- all_comb_correlations %>% filter((Freq >= 0.5 & Freq <= 0.8) | (Freq <= -0.5 & Freq >= -0.8))
print((nrow(mod_corr)/ nrow(all_comb_correlations)) * 100)


strong_corr <- all_comb_correlations %>% filter((Freq >= 0.8 & Freq < 1) | (Freq <= -0.8 & Freq > -1))
print((nrow(strong_corr)/ nrow(all_comb_correlations)) * 100)

perfect_correlation <- all_comb_correlations %>% filter((Freq == 1) | (Freq == -1))
print((nrow(perfect_correlation)/ nrow(all_comb_correlations)) * 100)

NA_corr <- all_comb_correlations %>% filter(is.na(Freq))
print((nrow(NA_corr)/ nrow(all_comb_correlations)) * 100)



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

#histogram of outliers 
hist((final_column_descriptives$Total_Outliers_3s),
     labels = TRUE, 
     #ylim = c(0,25), 
     #breaks = 50, 
     #xlim = c(0,1700), 
     main = "Total number of Outliers based on 3s Rule", 
     xlab = "Number of Outliers",
     col = ("limegreen"))

#histogram of Influential Values 
hist((final_column_descriptives$Total_Influential),
     labels = TRUE, 
     #ylim = c(0,25), 
     #breaks = 50, 
     #xlim = c(0,1700), 
     main = "Influential Values", 
     xlab = "Number of Influential Values",
     col = ("limegreen"))


# Histogram of Normalized SD
final_column_descriptives$Standard_Deviation_Norm[is.na(final_column_descriptives$Standard_Deviation_Norm)] <- 0

hist((final_column_descriptives$Standard_Deviation_Norm),
     labels = TRUE, 
     #ylim = c(0,25), 
     #breaks = 50, 
     #xlim = c(0,1700), 
     main = "Volatility in the Features", 
     xlab = "Standard Deviation of Normalized Feature",
     col = ("limegreen"))


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




# The SECOM dataset includes 1567 rows (observations) with 590 columns 
# representing 590 features/signals collected from sensors, together with 
# the labels representing pass (-1) / fail (1) yield for in house line testing 
# and associated date time stamp.  

secom.status<-data.frame(table(secom$Status,dnn = c("Status")))
secom.status

# Split the dataset with respect to class variables proportions (ratio 14:1)
## generates indexes for randomly splitting the data into training and test sets
# Setting seed so that the data is replicable.
set.seed(10)
# Spliting data - Training Data 80% and Test Data 20%
secom.train_index<-createDataPartition(secom$Status, times = 1,p = 0.8, list = FALSE)
## define the training and test sets by using above index
secom.training<-secom[secom.train_index,]
secom.data.train<-secom.training[,-c(1,2)]
secom.test<-secom[-secom.train_index,]

