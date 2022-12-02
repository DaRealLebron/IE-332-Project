#Imports/installs
install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

install.packages("tree")
library(tree)

install.packages("ROCR")
library(ROCR)

#temporarily Using yearly Data

#1: data preprocessing
#a
df_raw<- read.csv("C:/Users/12026/Documents/GitHub/IE-332-Project/detailed_finances_dataset.csv",header=TRUE)
df_raw$Sector<- as.factor(df_raw$Sector)

#b
goodCol<-!sapply(1:length(df_raw), function(i){sum(is.na(df_raw[,i]))>1000})
df<-na.omit(df_raw[,goodCol])

#c
Opr_EtI_ratio<-df$Operating.Expenses/df$Operating.Income
Target<-as.factor(as.numeric(df$X2019.PRICE.VAR....>0))
dropped_df<-cbind(subset(df, select = -c(Operating.Expenses,Operating.Income, X2019.PRICE.VAR....)),Opr_EtI_ratio,Target)

#2 train model function, Should train model to predict next week's stock price change
 trainML<-function(trainingSplit,allData,factorColName)
 {
   #partioning Data
   trainingRows<-createDataPartition(allData$factorColName, p=trainingSplit, list=FALSE)
   train <- allData[c(trainingRows),]
   test <- allData[c(-trainingRows),]
   
   #training Model
   modelT <- tree(Target ~ ., data = train) 
   
   #testing Model
   predT<- predict(modelT, test,type="class") 
   
   return(list(modelT,train,test,predT)) # assuming this return statement is valid!
 }
 
 
# 3 confusion matrix function to show accuracy for produced model
confusionGenerator <- function(predictedDataCol, dataCol)
{
  result<-paste(predictedDataCol,dataCol)
  confusionMatrix<-data.frame(table(result))
  return(confusionMatrix)
}
#actual confusion matrices
confusionMatrixT<-confusionGenerator(predT, test$Target)

#4 using the model to split the money

diversification<- function(predictedDf,availableFunds, X, minPriceChangeThreshold )
{
  predictedDf[order(-predictedDf$predictedStockPriceChange), ]
}




