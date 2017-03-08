#  "AA1_pair_trading"=list(lookback=20,threshold=25,series=1:10,posSizes=c(1,1,1,1,1,1,1,10,10,1),
#                        pairSeries =c(2,10 ),hedgeRatio=c(0.758),sdParam=1.5),   #2,10  0.758

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  if (is.null(store)) {
    checkParams(params)
    store <- initStore(newRowList, params$series)
  }
  # else  ### else here is a bug
  store <- updateStore(store, newRowList, params)
  data_store <<- store
  expectedPos <- rep(0,length(newRowList))
  limitOrders1 <- rep(0,length(newRowList))
  limitPrices1 <- rep(0,length(newRowList))
  limitOrders2 <- rep(0,length(newRowList))
  limitPrices2 <- rep(0,length(newRowList))
  
  if (store$iter > params$lookback) {
    
    for (i in 1:( length(params$pairSeries)/2 )   ) {
      # spreadStore[iter,i]
      startIndex <-  store$iter - params$lookback
      
      spread = store$spread[store$iter,i]
      
      bbands <- last(BBands(store$spread[startIndex:store$iter,i],
                            n=params$lookback,sd=params$sdParam))
      
     #  cat(store$iter,bbands["dn"],bbands["up"],spread,"\n")
     
      if(store$waitForLong[store$iter-1,2*i-1] == 1){
        if(spread < bbands["dn"]){
          cat(store$iter, "spread < bbands[dn]","go long\n")
           # go long
           expectedPos[params$pairSeries[2*i-1]] = 1
           expectedPos[params$pairSeries[2*i]] = -params$hedgeRatio[i]
          store$waitForLong[store$iter,2*i-1] = 0
          store$waitForShort[store$iter,2*i-1] = 1
          
        }else{
          cat(store$iter, "waitForLong","go flat\n")
          # go flat
           expectedPos[params$pairSeries[2*i-1]] = currentPos[params$pairSeries[2*i-1]]
           expectedPos[params$pairSeries[2*i]] = currentPos[params$pairSeries[2*i]]
          store$waitForLong[store$iter,2*i-1] = 1
          store$waitForShort[store$iter,2*i-1] = 0
        }
      }
      
      
      if(store$waitForShort[store$iter-1,2*i-1] == 1){
        
        if(spread > bbands["up"]){
          cat(store$iter,"spread > bbands[up]","go short \n")
          # go short
           expectedPos[params$pairSeries[2*i-1]] = -1
           expectedPos[params$pairSeries[2*i]] = params$hedgeRatio[i]
          store$waitForLong[store$iter,2*i-1] = 1
          store$waitForShort[store$iter,2*i-1] = 0
        }else{
          cat(store$iter,"waitForShort","go flat \n")
          # go flat
           expectedPos[params$pairSeries[2*i-1]] = currentPos[params$pairSeries[2*i-1]]
           expectedPos[params$pairSeries[2*i]] = currentPos[params$pairSeries[2*i]]
          store$waitForShort[store$iter,2*i-1] = 1
          store$waitForLong[store$iter,2*i-1] = 0
          
        }
        
      }
      
    }

  }
  
  # exit positions from yesterday
  # expectedPos = expectedPos * params$posSizes
  marketOrders <- expectedPos - currentPos 
  cat(store$iter,expectedPos,"\n")
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}

###############################################################################
# The following function is purely to help to prevent errrors by checking that 
# the requirement parameters are available
###############################################################################

checkParams <- function(params) { # make sure params are correct
  if (!"lookback" %in% names(params))
    stop("Parameter lookback not defined for strategy RSI")
  if (!"threshold" %in% names(params))
    stop("Parameter lookback not defined for strategy RSI")
  if (params$threshold < 0 || params$threshold > 50)
    stop("Parameter lookback is not between 0 and 50")
}

###############################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
###############################################################################

# functions for managing the store

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateSpreadStore <- function(spreadStore, newRowList, params, iter) {
  for (i in 1:(length(params$pairSeries)/2) ){
    spreadStore[iter,i] <- newRowList[[params$pairSeries[2*i-1]]]$Close - params$hedgeRatio[i] * newRowList[[params$pairSeries[2*i]]]$Close
  }
    
      
      #newRowList[[params$pairSeries[2*i-1]]]$Close # -
                           # params$hedgeRatio[i] * newRowList[[params$pairSeries[2*i]]]$Close
    
  return(spreadStore)
}



initStore <- function(newRowList,series) {
  return(list(iter=0,
              cl=matrix(0,nrow=maxRows,ncol=length(series)),
              spread=matrix(0,nrow=maxRows,ncol=length(series)),
              waitForShort = matrix(0,nrow=maxRows,ncol=length(series)),
              waitForLong  = matrix(1,nrow=maxRows,ncol=length(series))
              ))
}

updateStore <- function(store, newRowList, params) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,params$series,store$iter) 
  store$spread <- updateSpreadStore(store$spread,newRowList,params,store$iter) 
  return(store)
}

###############################################################################

# main strategy logic

lgStFt <-	function(clStore,column,iter) {
  # decide if we should go long/short/flat (returning 1/-1/0)
  startIndex <- iter - params$lookback - 1
  rsi <- last(RSI(clStore[startIndex:iter,column],n=params$lookback)) 
  if (rsi > (50 + params$threshold))
    return(-1) # short
  if (rsi < (50 - params$threshold))
    return(1)  # long
  return(0)
}
