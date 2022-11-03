#1: data preprocessing
#a
df_raw<- read.csv("C:/Users/12026/Desktop/detailed_finances_dataset.csv",header=TRUE)
df_raw$Sector<- as.factor(df_raw$Sector)

#b
