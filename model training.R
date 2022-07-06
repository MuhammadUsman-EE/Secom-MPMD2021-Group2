# knn imputing training data - use DF created after from replacing outliers.
preProcValues <- preProcess(df,
                            method = c("knnImpute"),
                            k = 5,
                            knnSummary = mean)

sum(is.na(df))
secom.imputed <- predict(preProcValues, df, na.action = na.pass)
sum(is.na(secom.imputed))


# knn imputing test data

secom.test <- secom.test %>% select(colnames(secom.imputed), Status)

install.packages("randomForest")
install.packages("ranger")
#secom.test <- secom.test %>% mutate_all(~ifelse(is.nan(.), NA, .))

test_imputed_df <- predict(preProcValues, secom.test)

sum(is.na(test_imputed_df))



## Feature Selection
source("rfe.R")
source("boruta.R")

# Choose one of the feature selection method

secom_features_selected <- rfe_select(secom.imputed, secom.training.label)
secom_features_selected <- boruta_select(secom.imputed, secom.training.label)

df_train_model <- cbind(secom_features_selected, secom.training.label)
colnames(df_train_model)[ncol(df_train_model)] <- 'Status'

## Choose this default train control parameters for now. Only change SMOTE or ROSE.

control <- trainControl(method = "cv",
                        number = 5,
                        classProbs = TRUE,
                        summaryFunction = prSummary,
                        sampling='smote', # SMOTE or ROSE
                        verboseIter = TRUE,
                        savePredictions = "final")

# SVM training with tuneLength
mod_res <- train(Status ~.,
                 data = df_train_model, 
                 method = "svmRadial", 
                 trControl = control,
                 tuneLength = 10,
                 metric = 'AUC')

mod_res


# SVM training with more specific parameter tuning
mod_res <- train(Status ~.,
                 data = df_train_model, 
                 method = "svmRadial", 
                 trControl = control,
                 tuneGrid = expand.grid(C = seq(0.1, 1, length = 10),
                                        sigma = seq(0.005, 0.05, length = 10)),
                 metric = 'AUC')
mod_res$bestTune
mod_res # signma = 0.01, C = 0.8


# Logistic Regression Training
mod_res <- train(Status ~ .,
                 data = df_train_model,
                 method = "glm",
                 family="binomial",
                 trControl = control,
                 metric = 'AUC')
mod_res


# Random Forest training
mod_res <- train(Status ~ .,
                 data = df_train_model,
                 method = "ranger",
                 tuneLength = 10,
                 trControl = control,
                 metric = 'AUC')
mod_res




library(pROC)

library(MLmetrics)

# Predict test rows probablitiies
test.predictions.prob <- predict(mod_res, newdata=test_imputed_df, type = 'prob')

#test.predictions <- predict(mod_res, newdata=test_imputed_df)

roc.test <- roc(test_imputed_df$Status, test.predictions.prob[,'Yes'])
auc(roc.test)

F1_Score(test_imputed_df$Status, test.predictions, positive = 'Yes')

# Choose threshold and select the one with the lowest cost and get the labels for the test rows

test.predictions <- as.factor(ifelse(test.predictions.prob[,'Yes']> 0.5,'Yes', 'No'))

confusionMatrix(data = test.predictions, reference = test_imputed_df$Status,
                mode = "everything",
                positive="Yes")





# probs <- seq(.1, 0.9, by = 0.02)
# 
# ths <- thresholder(mod_res,
#                    threshold = probs,
#                    final = TRUE,
#                    statistics = "all")
# 
# head(ths)
