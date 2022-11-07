#Imports/installs
install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

install.packages("tree")
library(tree)

install.packages("ROCR")
library(ROCR)

#1: data preprocessing
#a
df_raw<- read.csv("C:/Users/12026/Documents/GitHub/IE-332-Project/detailed_finances_dataset.csv",header=TRUE)
df_raw$Sector<- as.factor(df_raw$Sector)

#b
goodCol<-!sapply(1:length(df_raw), function(i){sum(is.na(df_raw[,i]))>1000})
df<-na.omit(df_raw[,goodCol])

#c
Opr_EtI_ratio<-df$Operating.Expenses/df$Operating.Income
Target<-as.numeric(df$X2019.PRICE.VAR....>0)
dropped_df<-cbind(subset(df, select = -c(Operating.Expenses,Operating.Income, X2019.PRICE.VAR....)),Opr_EtI_ratio,Target)

#2 Partitioning Data
#d
trainingRows<-createDataPartition(dropped_df$Sector, p=0.7, list=FALSE)
train <- dropped_df[c(trainingRows),]
test <- dropped_df[c(-trainingRows),]

#3 Train models #### NEED TO FIND CLASSIFICATION FORMULAS: WHICH COLS ARE RELEVENT?
#e
modelB <- naiveBayes(Target ~ ., data = train)

predB <- predict(modelB, test)
#f
modelT <- tree(Target ~ ., data = train)

predT<- round(predict(modelT, newdata = test)) #Not sure about this
#4
#g 
# confusion matrix function
confusionGenerator <- function(predictedDataCol, dataCol)
{
  result<-paste(predictedDataCol,dataCol)
  confusionMatrix<-data.frame(table(result))
  return(confusionMatrix)
}
#actual confusion matrices
confusionMatrixB<-confusionGenerator(predB, test$Target)
confusionMatrixT<-confusionGenerator(predT, test$Target)


