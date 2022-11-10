require(quantmod)

#get stock ticker information for selected dates
tickers <- read.delim("C:\\Users\\amitl\\OneDrive\\Documents\\Purdue\\IE332\\A4\\indices.txt", header=FALSE, sep=",")
s_date <- as.Date("2012-01-01")
e_date <- as.Date("2022-10-21")
priceInfo <- getSymbols.yahoo(tickers, from=s_date, to=e_date, auto.assign=TRUE, periodicity="daily", env=globalenv())
for(n in 1:length(tickers)){
  if(grep("^", tickers[n], ignore.case=FALSE, perl=FALSE, fixed=FALSE, useBytes=FALSE)){
    tickers[n] <- sub('.','',tickers[n])
  }else{}
}
#manually inputting certain values
tickers[24] <- ("IMOEX.ME")
tickers[27] <- ("000001.SS")
tickers[28] <- ("399001.SZ")

tickers <- toupper(tickers)

#merge and perform prcomp analysis on data
newInfo <- list()
newInfo = do.call(merge, lapply(tickers, function(x) Ad(get(x))))
principal_components <- prcomp(na.omit(newInfo), tol=0.1,  center=TRUE, scale. = TRUE)

#displays principal components data
summary(principal_components)