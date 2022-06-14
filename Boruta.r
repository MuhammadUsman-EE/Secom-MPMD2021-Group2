#Boruta

boruta.imputation <- function(df) {
    boruta.df <- Boruta(df$Status ~ .,data = secom.knn.imputed, doTrace=2)
    return(boruta.df)
}