#installing libraries
#install.packages("quantmod")
library("quantmod")
# ticker <- c("AVY", "WHR", "MTD", "ECL", "BXP", "HSY", "DUK", "CTVA", "ZTS", "DAL", "MLM", "ALGN", "NEM", "PARA", "CEG", "EQR", "COST", "AIZ", "SEE","PKG")
# sdate <- as.Date("2022-01-03")
# edate <- as.Date("2022-09-05")
# nVar <- length(ticker)

#Loop through symbols, fetch prices, and store in mySymbol
#mySymbol<-lapply(ticker,function(x) getSymbols(x, from=sdate, to=edate,periodicity = 'weekly',auto.assign=FALSE))

#names(mySymbol) <- ticker
#mySymbol <- na.omit(mySymbol)

#mySymbol: predicted data
#sdate: start date
#edate: end date
#ticker: tickers of all stocks of interst (may not be necessary)
markowitzWeightDistribution<-function(mySymbol,sdate,edate,ticker)
{
  nVar <- length(ticker)
  # Keep only adjusted closing stock prices
  roiList<- lapply(mySymbol,function(x) ROC(Ad(x), type="discrete")) # gets weekly ROI for all tickers of interest
  rd_wmean = sapply(roiList,mean, na.rm = T) #make a list with the average ROI for all tickers over the time period
  names(rd_wmean)<- ticker
  
  #getting data into dataframe and formatting it
  roiDataFrame<-Reduce(merge, roiList)
  rdCov<-cov(roiDataFrame,use ="complete.obs") #compute cov of df ignoring NA
  
  #question 1b
  sp500 <- getSymbols("^GSPC", from=sdate,to=edate,auto.assign=FALSE, periodicity = "weekly")
  sp500_adj<- Ad(sp500)
  sp500_wmean <- mean(na.omit(ROC(sp500_adj))) # gets the adjusted ROC mean of the SP500
  
  
  #question 1e
  
  neighbor_rnorm <- function(x,std=0.01)
  {
    x<-x+abs(rnorm(length(x),std))
    x<- x/sum(x)
    return(x)
  }
  
  #question 1g
  create_initial_meet2 <- function(nVar) {
    # nVar: number of assets in the portfolio
    x <- runif(n = nVar)
    x <- x/sum(x)
    return(x)
  }
  #####2nd function
  neighbor_rnorm_meetbudget <- function(x,std=0.01)
  {
    x<-x+abs(rnorm(length(x),std))
    x2<- x/sum(x)
    return(x2)
  }
  
  #question 1h
  # Calculate the objVal of a candidate solution which does not allow infeasibility.
  
  calculate_objVal_penalty <- function(x, covMat=rdCov, retVec=rd_wmean, bRet=sp500_wmean,p_ret=max(diag(rdCov))/1e-4, p_floor=1e3) 
  {
    # x: a candidate solution
    # covMat: Covariance matrix
    # retVec: expected return vector
    # bRet: benchmark expected return
    x_transpose <- t(x)
    obj_function <-x_transpose%*%covMat%*%x #intial unpenilized obj function
    if(any(x<0)){obj_function<-obj_function+p_floor} #checks for floor constraint violations
    if(t(unlist(retVec))%*%x<bRet){obj_function<-obj_function+p_ret} #checks for return constraint violations
    return(obj_function)
  }
  
  
  #question 1i
  
  mySAfun_simple <- function(nVar,temperature=3000, maxit=1000, cooling=0.95) {
    # nVar: number of stocks
    # temperature: initial temperature
    # maxit: maximum number of iterations to execute for
    # cooling: rate of cooling
    # MISSING LINE 1: generate an initial solution
    c_sol <- create_initial_meet2(nVar)
    # MISSING LINE 2: evaluate initial solution
    c_obj <-calculate_objVal_penalty(c_sol)
    best <- c_sol # track the best solution found so far
    best_obj <- c_obj # track the best objective found so far
    # to keep best objective values through the algorithm
    obj_vals <- best_obj
    cnt <- 0
    # run Simulated Annealing
    
    
    for(i in 1:maxit) {
      # MISSING LINE 3: # generate a neighbor solution
      neigh <-neighbor_rnorm_meetbudget(c_sol)
      # MISSING LINE 4: # calculate the objective value of the new solution
      neigh_obj <-calculate_objVal_penalty(neigh)
      if ( neigh_obj <= best_obj ) {
        # MISSING LINE 5-8: keep neigh if it is the new global best solution
        c_sol <-neigh 
        c_obj <-neigh_obj 
        best <-neigh #if neigh is better than best, make best equal neigh
        best_obj <-neigh_obj
      } else if ( runif(1) <= exp(-(neigh_obj-c_obj)/temperature) ) {
        # MISSING LINE 9-10: otherwise, probabilistically accept
        c_sol <- neigh # if neigh is not ideal, accept it anyways
        c_obj <- neigh_obj
        cnt <- cnt +1
      }
      temperature <- temperature * cooling # update cooling
      obj_vals <- c(obj_vals, best_obj) # maintain list of best found so far
    }
    
    
    return(list(best=best, values=obj_vals))
  }
  
  ####replicate SA algorithm
  trials <- 30
  ls.results = replicate(trials,mySAfun_simple(nVar))
  ###split the result in two lists, one is the solution, the other is
  #the objective value
  ls.split = split(ls.results,1:2)
  ###cbind each trajactory of objective value into a matrix
  results = Reduce(cbind,ls.split[[2]])
  ##obtain the final result of each trial
  trial_best_objs <- tail(results,1)
  ##find the trial with best objective value
  best_trial <- which.min(trial_best_objs)
  ##put other trials into a separate matrix for plotting
  other_objs <- results[,which(1:trials != best_trial)]
  #best objective value
  overall_best <- round(min(trial_best_objs),digits = 8)
  ##worst
  max_obj <- round(max(trial_best_objs),digits = 8)
  mean_obj <- round(mean(trial_best_objs),digits = 8)
  median_obj <- round(median(trial_best_objs),digits = 8)
  std_obj <- round(sd(trial_best_objs),digits = 8)
  ##interquartile range
  iqr_obj <- round(IQR(trial_best_objs),digits = 8)
  cresult<-c(results)
  #outliers <- boxplot(cresult, plot=FALSE)$out
  #cresult<- cresult[-which(cresult %in% outliers)]
  
  ###plotting
  png("simulatedAnnealingGraph.png")
  plot(1:nrow(results), other_objs[,1], col="gray", xlim=c(0,nrow(results)),
       ylim=(range(cresult)), type="b", xlab='Iteration Number', ylab ='Objective Value')
  plot(1:nrow(results), other_objs[,1], col="gray", xlim=c(0,nrow(results)),
       ylim=(range(trial_best_objs)), type="b", xlab='Iteration Number',
       ylab ='Objective Value')
  sapply(2:(trials-1), function(x) lines(other_objs[,x],col="gray",type="b"))
  lines(results[,best_trial], col="red", lwd=2)
  legend("topright",c("Best result","Other results"),fill=c("red","gray"))
  title(paste("median=", round(median_obj*100,4), " min=", round(overall_best*100,4),
              "max=", round(max_obj*100,4)))
  dev.off()
  return(ls.results[1,best_trial]) #returns the weights for the best row and the best trial list
}
