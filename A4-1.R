#Imports/installs
install.packages("caret")
library(caret)

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
#a
trainingRows<-createDataPartition(dropped_df$X, p=0.7, list=FALSE)
Train <- DT[c(trainingRows)]
Test <- DT[c(-trainingRows)]

