#install.packages("RANN")
#install.packages("devtools")
#install.packages("mlbench")

devtools::install_github("jefferis/RANN")
library(Boruta)

library("RANN")
library("devtools")

#For RFE feature selection
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)

#KNN Imputation function

secom.knn.impute <- function(df) {
preProcValues <- preProcess(df,
                            method = c("knnImpute"),
                            k = 20,
                            knnSummary = mean)
impute_df <- predict(preProcValues, df, na.action = na.pass)


procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)

for(i in procNames$col){
  impute_df[i] <- impute_df[i]*preProcValues$std[i]+preProcValues$mean[i] 
}

return(impute_df)

}

#Apply function to df
df_knn_imputed <- secom.knn.impute(df)
length(df_knn_imputed)

df_knn_imputed_numeric<-  df_knn_imputed %>% select(-c("Status"))
head(df_knn_imputed_numeric)
#Boruta feature selection
#Class has to be factor
class(df_knn_imputed$Status)

#Boruta Feature Selection - 16 Features are left after Boruta selecetion

boruta.df <- Boruta(Status ~ .,data = df_knn_imputed, doTrace=2)

plot(boruta.df,las=2)

getSelectedAttributes(boruta.df)
class(boruta.df)


#

