  #scraps etfs for all sectors
  require(XML)
  require(rvest)
  require(xml2)
  
  # scrapes top etfs per sector
  eftScraper<-function()
  {
    # Define certificate file, needed since website is HTTPS
    cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl") #this line already gives the path of the cafile
    cafile <- "/etc/ssl/certs/ca-certificates.crt"
    
    # Read secure page
    ## create the URL to scrape data from
    sectorList<-c("technology","healthcare","energy","real-estate","financials","materials","utilities","industrials","consumer-staples","consumer-discretionaries","telecom")
    # needed for compatibility referencing reasons
    sectorList2<-c("Technology","Healthcare","Energy","Real Estate","Financial Services","Basic Materials","Utilities","Industrials","Consumer Cyclical","Consumer Defensive","Communication Services")
    
    URL <- sapply(sectorList, function(x){paste("https://etfdb.com/etfs/sector/",x,"/#etfs__holdings&sort_name=number_of_holdings&sort_order=desc&page=",1, sep="")})
    
    pages<-lapply(URL,function(x){read_html(x)})
    
    tableData<-lapply(1:length(sectorList),  function(x){head(pages[[x]] %>% #parse and clean data
                                                                html_node(xpath= '//*[@id="etfs"]') %>%    # select the desired table
                                                                html_table(),-1)}[1])
    
    etfTickersBySector<-data.frame(t(sapply(tableData,c))) #convert data for better ease of use
    colnames(etfTickersBySector)<-sectorList2
    return(etfTickersBySector)
  }
 