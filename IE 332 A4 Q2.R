aaplData <- read.csv("C:/Users/Derrick/OneDrive - purdue.edu/Documents/AAPL_earnings_full1.csv", stringsAsFactors=FALSE)
library("quantmod")

#part i
#Defining Variables
EPSestimate <- aaplData$epsestimate
EPSactual <- aaplData$epsactual

#Numerator for equation
numerator <- EPSactual - EPSestimate
earningSurprise <- mapply('/',numerator,EPSestimate, SIMPLIFY = FALSE) #Formula 
earningSurprise <- na.omit(earningSurprise)

#get return values for AAPL stock for the entire range of dates
aaplPrices <- getSymbols("AAPL", from = "1997-11-25", to = "2022-08-07", auto.assign = FALSE, periodicity = "daily")
returnVals <- ROC(Ad(aaplPrices))
returnVals <- na.omit(returnVals)

#collect the dates of each earning event
cp <- as.Date(aaplData$startdatetime, format = "%m/%d/%Y")
ra <-c()
rb <-c()
Ra <- c()

#z is an attempt at using lapply, not sure where it is going wrong. Used for loop in place
#return values rb for each event
#z <- lapply(cp, function(i) {sum(returnVals,FROM = returnVals[cp[i]-50], END = returnVals[cp[i]-2])})
for (i in 1:length(cp)){
  u <- sum(returnVals,FROM = returnVals[cp[i]-50], END = returnVals[cp[i]-2])
  rb[i] <- u
}
#mean return of each stock during estimation range
mibt <- rb/49


#returns for event/post event window
#sum handles the ra value, then subtract m values to get 
#summation is based on t, always subtracting by "same" m value
for (j in 1:length(cp)){
  t <- sum(returnVals,FROM = returnVals[cp[j]-1], END = returnVals[cp[j]+10])
  Ra[j] <- t - mibt[j]
}


#Use ROC values for the return
#piazza says to calculate return like in A2, which is ROC
#need to extract return from certain date windows and sum them all together
#use the days from aaplData to find the values from returnVals

