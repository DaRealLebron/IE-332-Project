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
Target<-as.factor(as.numeric(df$X2019.PRICE.VAR....>0))
dropped_df<-cbind(subset(df, select = -c(Operating.Expenses,Operating.Income, X2019.PRICE.VAR....)),Opr_EtI_ratio,Target)

#2 Partitioning Data
#d
trainingRows<-createDataPartition(dropped_df$Sector, p=0.7, list=FALSE)
train <- dropped_df[c(trainingRows),]
test <- dropped_df[c(-trainingRows),]

#3 Train models 
#e
modelB <- naiveBayes(Target ~ ., data = train) 

predB <- predict(modelB, test)
#f
detach("package:e1071") 
modelT <- tree(Target ~ ., data = train) 

predT<- predict(modelT, test,type="class") 
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


#h    ROC plots

#plots dont look correct, but the rest works
spePreB <- prediction(as.numeric(predB), as.numeric(test$Target) ) 
perfB <- performance(spePreB, measure = "tpr", x.measure = "fpr")
plot(perfB, col="blue",main="Bayes vs Tree ROC Plot")

par(new=TRUE)

spePreT <- prediction(as.numeric(predT), as.numeric(test$Target) ) 
perfT <- performance(spePreT, measure = "tpr", x.measure = "fpr")
plot(perfT, col="red")

legend(x="bottomright", legend=c("Naive Bayes", "Decision Tree"), fill = c("blue","red"))


#i resplitting data

#creating new partitions:
trainingRows1<-createDataPartition(dropped_df$Sector, p=0.3, list=FALSE)
nTrain1 <- dropped_df[c(trainingRows1),]
nTest1 <- dropped_df[c(-trainingRows1),]

trainingRows2<-createDataPartition(dropped_df$Sector, p=0.5, list=FALSE)
nTrain2 <- dropped_df[c(trainingRows2),]
nTest2 <- dropped_df[c(-trainingRows2),]

trainingRows3<-createDataPartition(dropped_df$Sector, p=0.8, list=FALSE)
nTrain3 <- dropped_df[c(trainingRows3),]
nTest3 <- dropped_df[c(-trainingRows3),]

#j  training decision trees with each new partition

modelT1 <- tree(Target ~ ., data = nTrain1) #review
modelT2 <- tree(Target ~ ., data = nTrain2) #review
modelT3 <- tree(Target ~ ., data = nTrain3) #review

#k confusion and ROC for each new partition

predT1<- predict(modelT1, nTest1,type="class") 
predT2<- predict(modelT2, nTest2,type="class") 
predT3<- predict(modelT3, nTest3,type="class") 

confusionMatrixT1<-confusionGenerator(predT1, nTest1$Target)
confusionMatrixT2<-confusionGenerator(predT2, nTest2$Target)
confusionMatrixT3<-confusionGenerator(predT3, nTest3$Target)

#percentage correctly classified data function
accuracyPerc<-function(confusionM)
{
  percentage<- sum(confusionM[c(1,4),2])/sum(confusionM[,2])
  return(percentage)
}

#actual percentage values
accPart1<-accuracyPerc(confusionMatrixT1)
accPart2<-accuracyPerc(confusionMatrixT2)
accPart3<-accuracyPerc(confusionMatrixT3)

#plot ROCs
spePreT1 <- prediction(as.numeric(predT1), as.numeric(nTest1$Target) ) 
perfT1 <- performance(spePreT1, measure = "tpr", x.measure = "fpr")
plot(perfT1, col="blue",main="Comparison of Three Tree Models with Different Partitions")

par(new=TRUE)

spePreT2 <- prediction(as.numeric(predT2), as.numeric(nTest2$Target) ) 
perfT2 <- performance(spePreT2, measure = "tpr", x.measure = "fpr")
plot(perfT2, col="red")

par(new=TRUE)

spePreT3 <- prediction(as.numeric(predT3), as.numeric(nTest3$Target) ) 
perfT3 <- performance(spePreT3, measure = "tpr", x.measure = "fpr")
plot(perfT3, col="black")

legend(1,1,x="bottomright", legend=c("Decision Tree p1","Decision Tree p2","Decision Tree p3"), fill = c("blue","red","black"))
