#Function for SMOTE balancing - call secom_balance.smote with input dataframe and target variable.


secom_balance.smote <- function(inputDF, tVector){
  smote_balance <- smotefamily::SMOTE(inputDF, tVector, K=5, dup_size = 0)$data
  return(smote_balance)
}

#Function for ROSE balancing - call secom_balance.rose with input target variable and dataframe.


secom_balance.rose <- function(tVar, inputDF){
  rose_balance <- ROSE::ROSE(tVar, inputDF, p=0.5)$data
  return(rose_balance)
}


# The lines below are for testing the above functions - they can be removed

tempDF <- df[, c(1,2,3,4,5)]

smotest <- secom_balance.smote(tempDF, tempDF$Status)
rosetest <- secom_balance.rose(Status~., tempDF)

table(rosetest$Status)
table(smotest$Status)
