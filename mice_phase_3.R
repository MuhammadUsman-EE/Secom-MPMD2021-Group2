#MICE imputation for Phase 3 - Modelling
#Center - Scale df

preProcValues <- preProcess(df, method = c("center", 'scale'))

Mice_scaled_secom <- predict(preProcValues, df)

dim(Mice_scaled_secom)


# MICE function 
#5 iterations
mice_impute <- function(df){
  
  tempData <- mice(df,m=5,maxit=5,meth='pmm',seed=500)
  
  completedData <- complete(tempData,1)
  
  return(completedData)
  
}


#minmax scale
df.mice <- mice_impute(Mice_scaled_secom)

#Unscaling data
procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)

for(i in procNames$col){
  df.mice[i] <- df.mice[i]*preProcValues$std[i]+preProcValues$mean[i] 
}

save(df.mice,file= "df_mice_imputed.rda")
save(mice_impute,file= "mice_input_funct.rda")
