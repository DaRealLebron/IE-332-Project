#grab all stock data
# for each stock using the found sector, bind it to the sector data
specialMerge<-function(tickers,sdate,edate)
{
  sectorList<-c("technology","healthcare","energy","real-estate","financials","materials","utilities","industrials","consumer-staples","consumer-discretionaries","telecom")
  
  correspondingSectors<-sapply(tickers,stockIndustryFinder) # gets the sector for each ticker
  
  stockData<-minePlusClean(sdate,edate,tickers) # extract all stock Rate of change data for the given dates
  
  #stockDataAndSector<- cbin#adds the sector col to the stock data
  
  
  stockDataAndSector<-data.frame(tickers,correspondingSectors) #adds the sector col to the stock data
  stockDataAndSector<-stockDataAndSector[order(stockDataAndSector$correspondingSectors),] #reference table during join
  EftsPerSectors<-eftScraper()
  
  mergedStockList<-list()
  for (i in unique(stockDataAndSector$correspondingSectors)) # gives each sector
  {
    #create dummy vars for each iteration
    currentEtfVector<-as.vector(unlist(EftsPerSectors[i]))
    #extract needed data
    currentEtfDB<-minePlusClean(sdate,edate,currentEtfVector)[1:9]
    tempStockList<-list()
    for (j in stockDataAndSector[stockDataAndSector$correspondingSectors==i,1]) # gives each stock in a sector i
    {
      currentStockDb<-stockData[j]
      tempStockList<-c(tempStockList,list(cbind(currentStockDb,currentEtfDB))) #need to offset the weeks to get prediction
    }
    reduceList<-Reduce(function(x, y) merge(x, y, all=TRUE), tempStockList)
    mergedStockList<-c(mergedStockList,list(reduceList))
  }
  mergedStockList<- lapply(1:9, function(i) mergedStockList[[i]][,1:10]) #removing excess rows
  
  answer<-data.table::rbindlist(MlPlugin, use.names = FALSE)
  olnames(answer)<-c("vital",2:10)
  return(answer)
}


