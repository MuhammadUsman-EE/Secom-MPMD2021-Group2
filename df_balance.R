#Function for SMOTE balancing - call secom_balance.smote with input dataframe and target variable.

secom_balance.smote <- function(inputDF, tVector){
  Status <- c(secom.training.label)
  inputDF <- cbind(inputDF, Status)
  inputDF[, c("Status")] <- sapply(inputDF[,c("Status")], as.numeric)
  smote_balance <- smotefamily::SMOTE(X = inputDF, target = tVector, K=5, dup_size = 0)$data
  return(smote_balance)
}

#Function for ROSE balancing - call secom_balance.rose with input target variable and dataframe.

secom_balance.rose <- function(tVar, inputDF){
  Status <- c(secom.training.label)
  inputDF <- cbind(inputDF, Status)
  rose_balance <- ROSE::ROSE(tVar, inputDF, p=0.5)$data
  return(rose_balance)
}


#Function for SMOTE balancing with performance estimation package

secom_balance.smotePE <- function(tVar, inputDF){
  Status <- c(secom.training.label)
  inputDF <- cbind(inputDF, Status)
  smotePE <- performanceEstimation::smote(tVar, inputDF, perc.over = 10, perc.under = 1, k = 5)
  return(smotePE)
}


# The lines below are for testing the above functions - they can be removed

tempDF <- df[, c("Feature_1", "Feature_2", "Feature_3", "Feature_4", "Status")]
tempDF

smotest2 <- secom_balance.smote(tempDF, tempDF$Status)
## Non-numeric error related to Status column being a factor
rosetest2 <- secom_balance.rose(Status~., tempDF)
smotestPE <- secom_balance.smotePE(Status~., tempDF)
table(tempDF$Status)
table(rosetest$Status)
prop.table(table(rosetest$Status))
table(smotest$Status)
table(smotestPE$Status)
prop.table(table(smotestPE$Status))
