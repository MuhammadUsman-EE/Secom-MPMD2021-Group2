#install.packages("RANN")
#install.packages("devtools")
#install.packages("mlbench")

#devtools::install_github("jefferis/RANN")
library(Boruta)

library("RANN")
library("devtools")

#For RFE feature selection
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(caret)


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
secom.imputed <- secom.knn.impute(secom.df)
length(secom_imputed)

#Boruta feature selection
#Class has to be factor
class(df_knn_imputed$Status)

#Boruta Feature Selection - 16 Features are left after Boruta selecetion

boruta.df <- Boruta(Status ~ .,data = df_knn_imputed, doTrace=2)

#Plot the boruta feature importance an stats of boruta - 16 features left
plot(boruta.df,las=2)
plotImpHistory(boruta.df)
attStats(boruta.df)

getSelectedAttributes(boruta.df)
class(boruta.df)

#RFE feature selection random forest
rfe.ctrl<- rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE)
subsets <-c(2:25, 30, 35, 40, 45, 50, 55, 60, 65:300)
rfe.Profile <- rfe(x=secom.imputed[,-435],y= secom.imputed$Status, rfeControl = rfe.ctrl)

