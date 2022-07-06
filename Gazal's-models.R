# Install Required Packages and load libraries 
install.packages("pacman")
install.packages("smotefamily")
install.packages("ROSE")
install.packages("performanceEstimation")
install.packages("RANN")
install.packages("MASS")
install.packages("randomForest")
install.packages("ranger")

# load pacman
library(pacman)
#load smotefamily
library(smotefamily)
#load ROSE
library(ROSE)
#load performanceEstimation
library(performanceEstimation)
#load Caret
library('caret')


p_load('tidyverse')
p_load('tidyselect')
p_load("ggplot2")
p_load("dplyr")
p_load("corrplot")
p_load("reshape2")
p_load("data.table")
p_load('reshape2')
p_load('DMwR2')

preProcValues <- preProcess(df,
                            method = c("knnImpute"),
                            k = 5,
                            knnSummary = mean)

sum(is.na(df))
secom.imputed <- predict(preProcValues, df, na.action = na.pass)
sum(is.na(secom.imputed))


# knn imputing test data

secom.test <- secom.test %>% select(colnames(secom.imputed), Status)
test_imputed_df <- predict(preProcValues, secom.test)
#double check for null values
sum(is.na(test_imputed_df))


## Choose this default train control parameters.
## Working with both SMOTE and ROSE to select the best results
## When building the models

control_smote <- trainControl(method = "cv",
                              number = 5,
                              classProbs = TRUE,
                              summaryFunction = prSummary,
                              sampling='smote', # SMOTE or ROSE
                              verboseIter = TRUE,
                              savePredictions = "final")

control_rose <- trainControl(method = "cv",
                             number = 5,
                             classProbs = TRUE,
                             summaryFunction = prSummary,
                             sampling='rose', # SMOTE or ROSE
                             verboseIter = TRUE,
                             savePredictions = "final")

##First Step work with RFE Feature Selection 
##and build the Logistic Regression and Linear Discriminant Analysis models
##with both SMOTE or ROSE


source("rfe.R")
# Applying RFE feature selection through calling the function
secom_features_selected_RFE <- rfe_select(secom.imputed, secom.training.label)

df_train_model_RFE <- cbind(secom_features_selected_RFE, secom.training.label)
colnames(df_train_model_RFE)[ncol(df_train_model_RFE)] <- 'Status'


# Logistic Regression Training with SMOTE
mod_LogisticRegressionRFE_Smote <- train(Status ~ .,
                                         data = df_train_model_RFE,
                                         method = "glm",
                                         family="binomial",
                                         trControl = control_smote,
                                         metric = 'AUC')
mod_LogisticRegressionRFE_Smote

# Logistic Regression Training with ROSE
mod_LogisticRegressionRFE_ROSE <- train(Status ~ .,
                                        data = df_train_model_RFE,
                                        method = "glm",
                                        family="binomial",
                                        trControl = control_rose,
                                        metric = 'AUC')
mod_LogisticRegressionRFE_ROSE

# Linear Discriminant Model with SMOTE
mod_LinearDiscRFE_Smote <- train(Status ~ .,
                                 data = df_train_model_RFE,
                                 method = "lda",
                                 family="binomial",
                                 trControl = control_smote,
                                 metric = 'AUC')

mod_LinearDiscRFE_Smote
# Linear Discriminant Model Training with ROSE
mod_LinearDiscRFE_ROSE <- train(Status ~ .,
                                data = df_train_model_RFE,
                                method = "lda",
                                family="binomial",
                                trControl = control_rose,
                                metric = 'AUC')

mod_LinearDiscRFE_ROSE


##Second major step is to take the BORUTA feature selection 
##and build the Logistic Regression and Linear Discriminant Analysis models
##with both SMOTE or ROSE

source("boruta.R")
secom_features_selected_BORUTA <- boruta_select(secom.imputed, secom.training.label)


df_train_model_BORUTA <- cbind(secom_features_selected_BORUTA, secom.training.label)
colnames(df_train_model_BORUTA)[ncol(df_train_model_BORUTA)] <- 'Status'


# Logistic Regression Training with SMOTE
mod_LogisticRegressionBORUTA_Smote <- train(Status ~ .,
                                            data = df_train_model_BORUTA,
                                            method = "glm",
                                            family="binomial",
                                            trControl = control_smote,
                                            metric = 'AUC')
mod_LogisticRegressionBORUTA_Smote

# Logistic Regression Training with ROSE
mod_LogisticRegressionBORUTA_ROSE <- train(Status ~ .,
                                           data = df_train_model_BORUTA,
                                           method = "glm",
                                           family="binomial",
                                           trControl = control_rose,
                                           metric = 'AUC')
mod_LogisticRegressionBORUTA_ROSE

# Linear Discriminant Model with SMOTE
mod_LinearDiscBORUTA_Smote <- train(Status ~ .,
                                    data = df_train_model_BORUTA,
                                    method = "lda",
                                    family="binomial",
                                    trControl = control_smote,
                                    metric = 'AUC')

mod_LinearDiscBORUTA_Smote
# Linear Discriminant Model Training with ROSE
mod_LinearDiscBORUTA_ROSE <- train(Status ~ .,
                                   data = df_train_model_BORUTA,
                                   method = "lda",
                                   family="binomial",
                                   trControl = control_rose,
                                   metric = 'AUC')

mod_LinearDiscBORUTA_ROSE

