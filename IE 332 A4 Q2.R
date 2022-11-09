aaplData <- read.csv("C:/Users/12026/Documents/GitHub/IE-332-Project/AAPL_earnings_full.csv", stringsAsFactors=FALSE)
library("quantmod")

#part i
#Defining Variables
EPSestimate <- aaplData$epsestimate
EPSactual <- aaplData$epsactual

#Numerator for equation
numerator <- EPSactual - EPSestimate
earningSurprise <- numerator/EPSestimate

earningSurprise[is.nan(earningSurprise)]<-0

#ii
#get return values for AAPL stock for the entire range of dates
aaplPrices <- getSymbols("AAPL", from = "1997-11-25", to = "2022-08-07", auto.assign = FALSE, periodicity = "daily")
returnVals <- ROC(Ad(aaplPrices))
returnVals <- na.omit(returnVals)

#collect the dates of each earning event
cp <- as.Date(aaplData$startdatetime, format = "%m/%d/%Y")
ra <-c()
rb <-c()
Ra <- c()


rb<-sapply(1:length(cp), function(i){sum(returnVals,FROM = returnVals[cp[i]-50], END = returnVals[cp[i]-2])})
#mean return of each stock during estimation range
mibt <- rb/49


#returns for event/post event window
#sum handles the ra value, then subtract m values to get 
#summation is based on t, always subtracting by "same" m value

temoRa<-sapply(1:length(cp), function(j){sum(returnVals,FROM = returnVals[cp[j]-1], END = returnVals[cp[j]+10])})
Ra1<-temoRa-mibt

#Use ROC values for the return
#piazza says to calculate return like in A2, which is ROC
#need to extract return from certain date windows and sum them all together
#use the days from aaplData to find the values from returnVals

