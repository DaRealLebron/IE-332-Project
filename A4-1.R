#1: data preprocessing
#a
df_raw<- read.csv("C:/Users/12026/Desktop/detailed_finances_dataset.csv",header=TRUE)
df_raw$Sector<- as.factor(df_raw$Sector)

#b
goodCol<-!sapply(1:length(df_raw), function(i){sum(is.na(df_raw[,i]))>1000})
df<-na.omit(df_raw[,goodCol])

#c
