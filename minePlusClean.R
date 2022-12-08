library(quantmod)
# Data Range and ticker symbols
sdate <- as.Date("2022-01-03")
edate <- as.Date("2022-12-05")
Symbols <- c("AVY", "WHR", "MTD", "ECL", "BXP", "HSY", "DUK", "CTVA", "ZTS",
             "DAL","MLM","ALGN","NEM","PARA","CEG","EQR","COST","AIZ","SEE","PKG")

minePlusClean<-function(sdate,edate,tickerVector)
{
   
  ls_price_data <- try(lapply(tickerVector,function(x) getSymbols(x, from=sdate, to=edate,periodicity = 'weekly',auto.assign=FALSE)))
  if(inherits(ls_price_data, "try-error")) #ensures there is no error, if there is, uses another scrapper
  {
    ls_price_data<-lapply(tickerVector,function(x) getSymbols.tiingo(x,from=sdate,to=edate,periodicity = 'weekly',auto.assign = FALSE, api.key = "db495f78f83c10f4a5b851a508c9bfed36076965"))
  }
  ls_return_data <- lapply(ls_price_data,function(x) ROC(Ad(x), type="discrete"))
  rdReturnsDF <- as.data.frame(do.call(merge,ls_return_data))
  colnames(rdReturnsDF) <- gsub(".Adjusted","",colnames(rdReturnsDF))
  rdReturnsDF <- rdReturnsDF[-1,]
  #csv.write("~etfData.csv") #may not be needed
  return(rdReturnsDF)
}

minePlusClean(sdate,edate,as.vector(unlist(etfTickersBySector[[1]]))) # good test case
###add other techincal indicators then start working on prediction
