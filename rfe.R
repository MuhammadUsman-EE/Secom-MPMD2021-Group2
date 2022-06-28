rfe_select <- function(secom.imputed, secom.training.label) {
set.seed(1)
rfFuncs$summary <- twoClassSummary  



control <- rfeControl(functions = rfFuncs, # random forest
                      method = "cv", # repeated cv 
                      number = 5,
                      returnResamp="final",
                      verbose = TRUE)# number of folds


#trainctrl <- trainControl(classProbs= TRUE,
 #                         summaryFunction = twoClassSummary)
set.seed(1)
result_rfe1 <- rfe(x = secom.imputed, 
                   y = secom.training.label, 
                   sizes = c(10:30),
                   metric = 'ROC',
                   rfeControl = control
                   #trControl = trainctrl
                   )

plot(result_rfe1, type=c("g", "o"))

predictors(result_rfe1)

selected_rfe_df <- secom.imputed %>% select(predictors(result_rfe1))
return(selected_rfe_df)
}
