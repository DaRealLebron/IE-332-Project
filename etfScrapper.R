  require(XML)
  require(rvest)
  require(xml2)
  # Define certificate file, needed since website is HTTPS
  cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl") #this line already gives the path of the cafile
  cafile <- "/etc/ssl/certs/ca-certificates.crt"
  
  
  # Read secure page
  ## create the URL to scrape data from
  sectorList<-c("technology","healthcare","energy","real-estate","financials","materials","utilities","industrials","consumer-staples","consumer-discretionaries","telecom")
  URL <- sapply(sectorList, function(x){paste("https://etfdb.com/etfs/sector/",x,"/#etfs__holdings&sort_name=number_of_holdings&sort_order=desc&page=",1, sep="")})
  
  #combinations<- expand.grid(sectorComb,numPage)
  #page <- lapply(combinations,function(x){read_html(URL[paste0(combinations[1,])])})
  pages<-lapply(URL,function(x){read_html(x)})
  
  tableData<-lapply(1:length(sectorList),  function(x){head(pages[[x]] %>% #parse and clean data
      html_node(xpath= '//*[@id="etfs"]') %>%    # select the desired table
      html_table(),-1)}[1])
  
  #tableData<-colnames(rep(sectorList,times=length(numPage)))

