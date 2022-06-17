mice_impute <- function(df){

tempData <- mice(df,m=3,maxit=5,meth='rf',seed=500)

completedData <- complete(mice_imputed_Data,1)

return(completedData)

}
