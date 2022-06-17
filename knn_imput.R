#KNN Imputation function

knn_impute <- function(df) {
preProcValues <- preProcess(df,
                            method = c("knnImpute"),
                            k = 10,
                            knnSummary = mean)
impute_df <- predict(preProcValues, df, na.action = na.pass)


procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)

for(i in procNames$col){
  impute_df[i] <- impute_df[i]*preProcValues$std[i]+preProcValues$mean[i] 
}

return(impute_df)

}


