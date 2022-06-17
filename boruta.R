# Applying Boruta

boruta_select <- function(secom.imputed, secom.training.label) {
boruta_df <- cbind(secom.imputed, secom.training.label)

colnames(boruta_df)[ncol(boruta_df)] <- 'Status'
boruta.train <- Boruta(Status~., data = boruta_df, doTrace = 2)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

s <- getSelectedAttributes(final.boruta, withTentative = F)


plotImpHistory(final.boruta)


plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)


boruta_selected_train <- secom.imputed %>% select(s)
return(boruta_selected_train)
}
