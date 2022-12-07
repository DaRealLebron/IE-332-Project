#install.packages('quantmod')
#install.packages('tictoc')
#install.packages("RMySQL")


library(tictoc)
library(quantmod)
library(RMySQL)




tic("total")

tickers <- c("ATVI","ADBE","ADP","ABNB","ALGN","GOOGL","GOOG","AMZN","AMD","AEP",
             "AMGN","ADI","ANSS","AAPL","AMAT","ASML","AZN","TEAM","ADSK","BIDU",
             "BIIB","BKNG","AVGO","CDNS","CHTR","CTAS","CSCO","CTSH","CMCSA","CEG",
             "CPRT","COST","CRWD","CSX","DDOG","DXCM","DOCU","DLTR","EBAY","EA",
             "ENPH","EXC","FAST","FISV","FTNT","GILD","HON","IDXX","ILMN","INTC",
             "INTU","ISRG","JD","KDP","KLAC","KHC","LRCX","LCID","LULU","MAR",
             "MRVL","MTCH","MELI","META","MCHP","MU","MSFT","MRNA","MDLZ","MNST",
             "NTES","NFLX","NVDA","NXPI","ORLY","ODFL","PCAR","PANW","PAYX","PYPL",
             "PEP","PDD","QCOM","REGN","ROST","SGEN","SIRI","SWKS","SPLK","SBUX",
             "SNPS","TMUS","TSLA","TXN","VRSN","VRSK","VRTX","WBA","WDAY","XEL","ZM","ZS")

symbols <- tickers

tic("ALL")
dataset<- xts() # Only run once



#SOURCE: https://quant.stackexchange.com/questions/18758/how-do-i-loop-through-all-the-stocks-with-quantmod-and-ttr

# progress bar to see the % of completion
n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style=3)


# Actual loop: 
for(i in 1:length(symbols)) {
  symbols[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol,from="2022-01-01", src='yahoo', verbose = F))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from="2022-01-01", src='yahoo', verbose = F)
    dataset <- merge(dataset, Ad(get(symbols[i])))
    rm(symbol)
  }
  setTxtProgressBar(pb, i)
}

toc()



mydb = dbConnect(MySQL(), user='g1124543', password='ie332', dbname='g1124543', host='mydb.itap.purdue.edu')
stockDataExport <- as.data.frame(dataset)
dbWriteTable(mydb, "StockData",stockDataExport, append = TRUE)
dbDisconnect(mydb)