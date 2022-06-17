#Function for ROSE balancing - call secom_balance.rose with input target variable and dataframe.
secom_balance.rose <- function(secom_features_selected, secom.training.label, split){
  rose_df <- cbind(secom_features_selected, secom.training.label)
  colnames(rose_df)[ncol(rose_df)] <- 'Status'
  rose_balance <- ROSE::ROSE(Status ~., rose_df, p=split)$data
  return(rose_balance)
}

#Function for SMOTE balancing with performance estimation package

secom_balance.smote <- function(secom_features_selected, secom.training.label, over, under){
  smote_df <- cbind(secom_features_selected, secom.training.label)
  colnames(smote_df)[ncol(smote_df)] <- 'Status'
  smote_balance <- performanceEstimation::smote(Status ~., smote_df, perc.over = over, perc.under = under)
  return(smote_balance)
}