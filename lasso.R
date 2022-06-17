lasso_select <- function(secom.imputed, secom.training.label) {
preProcValues <- preProcess(secom.imputed, method = c("center", 'scale'))

lasso_transformed <- predict(preProcValues, secom.imputed)


x <- as.matrix(lasso_transformed) # all X vars
                    
set.seed(100)
cv.lasso <- cv.glmnet(x, y = secom.training.label, family='binomial', alpha=1, parallel=TRUE, type.measure='auc')

plot(cv.lasso)

cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
lass_selected <- df_coef[df_coef[, 1] != 0, ]
lass_selected
}