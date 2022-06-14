
#install.packages("RANN")
#install.packages("devtools")
#install.packages("mlbench")

#devtools::install_github("jefferis/RANN")
library(Boruta)

library("RANN")
library("devtools")

#Libraries for feature selection
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(caret)
#install.packages("RANN")
#install.packages("devtools")
#install.packages("mlbench")


#read file and transform to df to run Boruta and RFE

load(file="secom.df.rda")
#name df---df <- as.data.frame(secom.df)

#Apply function to df
secom.knn.imputed <- secom.knn.impute(secom.df)

view(secom.knn.imputed)

#####BORUTA

boruta.imputation <- function(secom.knn.imputed) {
    boruta.df <- Boruta(secom.knn.imputed$Status ~ .,data = secom.knn.imputed, doTrace=2)
    return(boruta.df)
}

#Call boruta function
secom.boruta<-boruta.imputation(secom.knn.imputed)
length(secom.boruta)
#Plot the boruta feature importance an stats of boruta - 16 features left
plot(secom.boruta,las=2)
plotImpHistory(secom.boruta)
attStats(secom.boruta)
getSelectedAttributes(secom.boruta)

#Try 2 with secom.knn.imputation$Status in function

secom.boruta2<-boruta.imputation(secom.knn.imputed)
getSelectedAttributes(secom.boruta2)
plot(secom.boruta2,las=2)


#RFE feature selection random forest
rfe.ctrl<- rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE)
subsets <-c(1:)
rfe.Profile <- rfe(x=secom.knn.imputed[,-435],y= secom.knn.imputed$Status, sizes= subsets, rfeControl = rfe.ctrl,rerank = TRUE)

rfe.Profile
