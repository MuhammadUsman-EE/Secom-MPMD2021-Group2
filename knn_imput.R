secom.knn.impute <- function(secom_data) {
preProcValues <- preProcess(secom_data,
                            method = c("knnImpute"),
                            k = 20,
                            knnSummary = mean)
impute_df <- predict(preProcValues, secom_data, na.action = na.pass)


procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)

for(i in procNames$col){
  impute_df[i] <- impute_df[i]*preProcValues$std[i]+preProcValues$mean[i] 
}

return(impute_df)

}





