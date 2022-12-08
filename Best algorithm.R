#Imports/installs
install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

install.packages("tree")
library(tree)

MlPlugin<-specialMerge(tickers,sdate,edate)

#1 train model function, Should train model to predict next week's stock price change
trainML<-function(trainingSplit,allData,factorColName)
{
  #partioning Data
  trainingRows<-createDataPartition(allData$vital, p=trainingSplit, list=FALSE)
  train <- allData[c(trainingRows),]
  test <- allData[c(-trainingRows),]
  
  #training Model
  modelT <- tree(vital ~ ., data = train) 
  
  #testing Model
  predT<- predict(modelT, test,type="class") 
  
  return(list(modelT,train,test,predT)) # assuming this return statement is valid!
}