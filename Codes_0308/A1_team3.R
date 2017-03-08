source('strategies/A1_store_and_algorithms.R'); 
source('strategies/A1_backtester.R'); #A1_position_generation
source('strategies/A1_position_generation.R');

params =list(iniDay=52,
                Rsi = list(lookback=10,threshold=25,series=1:10),
                Ema = list(series=1:10,sLookback=7, mLookback=10,lLookback=20),
                Cci = list(lookback=22,series=c(6,8),threshold=100),
                Macd = list(series=1:10, fLookback=12,sLookback=26,
                            lookback_sig=9,iniDay=52)
                )

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  marketOrders <- allzero
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero
  expectedPos  <- allzero
  
  # initialize/update store
  if (is.null(store)) {
    store <- initStore(newRowList)
  }else{
    store = updateStore(store, newRowList, currentPos)
  }
  
  
  ########################################### Track performance ####################################
  if (store$iter > params$iniDay ) {
    
    MYpnlList = MyBacktest(store = store) # return(MYDailyPnL)
    
    for(j in 1:10){
      store$MYpnlList[[j]][store$iter-1,"MYDailyPnL"] = MYpnlList$MYDailyPnL[j] 
      store$MYpnlList[[j]][store$iter-1,"MYCumPnL"] = MYpnlList$MYCumPnL[j]
      store$MYpnlList[[j]][store$iter-1,"MYCumMax"] = max(MYpnlList$MYCumPnL[j],
                                                          store$MYpnlList[[j]][store$iter-2,"MYCumMax"])
      MYCumPnL = store$MYpnlList[[j]][store$iter-1,"MYCumPnL"]
      MYCumMax = store$MYpnlList[[j]][store$iter-1,"MYCumMax"]
      
      
      store$MYpnlList[[j]][store$iter-1,"MYmd"] = max(store$MYpnlList[[j]][store$iter-2,"MYmd"],
                                                      MYCumMax-MYCumPnL)
      MYmd = store$MYpnlList[[j]][store$iter-1,"MYmd"]
      
      if(MYCumPnL<=0){
        store$MYpnlList[[j]][store$iter-1,"MYPdRatio"] = MYCumPnL
      }else{
        store$MYpnlList[[j]][store$iter-1,"MYPdRatio"] = round(MYCumPnL/MYmd,2)
      }
      
    }

  }
  
  ###################################################################################################

  # generate expected position [direction] for next day
  if (store$iter > params$iniDay) {
    
    for(ts in 1:length(newRowList)){
      returnedList = AggregateDirections(store,ts)
      
      store$aggregateResults$finalDirections[store$iter, ts] = returnedList$aggDirections  # # aggDirections = -1/0/1
      
      expectedPos[ts] = returnedList$expectedPos
      
      for(strategyIndex in 1:length(store$strategies)){
        store$strategies[[strategyIndex]]$advicedDirections[store$iter,ts] = returnedList$sgnlDirct[strategyIndex]
                                                              #sgnlDirct : vector with length = number of algorithms
  
      }
    }
    
    
    ### exit condition:
    # store$MYpnlList[[j]][store$iter-1,"MYPdRatio"]
    if(store$iter>100){
      StayOrNot = sapply(1:10, function(x) 
        ifelse(store$MYpnlList[[x]][store$iter-1,"MYPdRatio"]<= -30,0,1)
      )
     #  expectedPos = expectedPos * StayOrNot
    }
    
    
  }
  
  # calculate the position size
  meanDiff      = rep(0,10)
  positionSizes = rep(1,10)
  
  openPrices = lapply(1:10, function(x) tail(store$op[[x]],params$iniDay) ) # params$iniDay = 20
  
  if(store$iter > params$iniDay)  positionSizes = PosSizing(openPrices)
  
   expectedPos = expectedPos * positionSizes
  
 
 
  # exit positions from yesterday
  marketOrders <- expectedPos - currentPos 
  
  
  
  # update store
  store$aggregateResults$expectedPos[store$iter,]=expectedPos
  store$aggregateResults$marketOrders[store$iter,]=marketOrders
  data_store2 <<- store
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}


