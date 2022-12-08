#installing relevant libraries
library(XML)
library(tidyverse)
library(rvest)
library(readxl)

StockList<-read.csv("~/GitHub/IE-332-Project/detailed_finances_dataset.csv",header=TRUE)
tickers<- StockList$X[1:5]

#scrape each ticker
for (i in tickers)
{
  url<-paste0("https://finviz.com/quote.ashx?t=",i)
  webpage<-readLines(url)
  html<-htmlTreeParse(webpage, useInternalNodes = TRUE,asText = TRUE)
  tableNodes<-getNodeSet(html,"//table")
  assign(i, readHTMLTable(tableNodes[[8]]))
  df<- get(i)
  df['stock']<-i
  assign(i,df)
}

#aggregate data
stockDataList<-rbind(mget(tickers))
stockData<-do.call(cbind,stockDataList)
stockData<-stockData[,c(ncol(stockData),1:ncol(stockData)-1)]
write.table(stockData,"~/GitHub/IE-332-Project/StockData",sep=",",row.names=FALSE,col.names = FALSE)

rm(df)
length_sdl<-1:ncol(stockDataList)

noms<-c("recVal")
cc<-data.frame(matrix(NA,nrow = ncol(stockDataList),ncol=length(noms)))
colnames(cc)<-noms


for (j in length_sdl)
{
  cc$recVal[i]<-stockDataList[[i]][[11,2]]
}

