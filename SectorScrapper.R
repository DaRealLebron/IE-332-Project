#scraps etfs for all sectors
require(XML)
require(rvest)
require(xml2)

#take the stock ticker and returns the sector of that stock
stockIndustryFinder<-function(ticker)
{
  # Define certificate file, needed since website is HTTPS
  cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl") #this line already gives the path of the cafile
  cafile <- "/etc/ssl/certs/ca-certificates.crt"
  
  link <- paste0("https://finance.yahoo.com/quote/", ticker, "/profile?p=", ticker)
  page <- read_html(link)
  sector <- page %>% 
    html_node(xpath = '//*[@id="Col1-0-Profile-Proxy"]/section/div[1]/div/div/p[2]/span[2]') %>%
    html_text()
  
  
  return(sector)
}

