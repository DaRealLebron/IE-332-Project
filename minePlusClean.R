library(quantmod)
# Data Range and ticker symbols
sdate <- as.Date("2022-01-03")
edate <- as.Date("2022-09-05")
Symbols <- c("AVY", "WHR", "MTD", "ECL", "BXP", "HSY", "DUK", "CTVA", "ZTS",
             "DAL","MLM","ALGN","NEM","PARA","CEG","EQR","COST","AIZ","SEE","PKG")

minePlusClean<-function(sdate,edate,tickerVector)
{
  ls_price_data <- lapply(tickerVector,function(x) getSymbols(x, from=sdate, to=edate,periodicity = 'weekly',auto.assign=FALSE))
  ls_return_data <- lapply(ls_price_data,function(x) ROC(Ad(x), type="discrete"))
  rdReturnsDF <- as.data.frame(do.call(merge,ls_return_data))
  colnames(rdReturnsDF) <- gsub(".Adjusted","",colnames(rdReturnsDF))
  rdReturnsDF <- rdReturnsDF[-1,]
  return(rdReturnsDF)
}

minePlusClean(sdate,edate,as.vector(unlist(etfTickersBySector[[1]])))
