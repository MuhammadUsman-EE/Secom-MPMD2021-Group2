log_model_run <- function(features_selected, y) {
features_selected <- cbind(features_selected, secom.training.label)
colnames(features_selected)[ncol(features_selected)] <- 'Status'



train.control <- trainControl(method="cv", number=5,
                              summaryFunction = twoClassSummary, 
                              savePredictions = T, classProbs = TRUE)                
# train the model on training set
model <- train(Status ~ .,
               data = features_selected,
               method = "glm",
               family="binomial",
               preProc=c("center", "scale"),
               trControl = train.control)

summary(model)
model

return(model)
}

