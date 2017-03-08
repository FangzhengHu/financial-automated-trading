
###############################################################################
# functions for generating the expected postion 
###############################################################################




# 1_ Obtain Aggregate Directions [based on 4 & 5]
AggregateDirections <-	function(store,series) {
  sgnlDirct = SignalDirections(store,series)
  aggSignals = sum( sgnlDirct * SignalWeights() )
  
  # approach 1 for collaborate signals
  if(abs(aggSignals)>0.5){
    
    if(aggSignals>0){
      aggDirections = 1
    }else{
      aggDirections = -1
    }
  }else{
    aggDirections = 0
  }
 
  # approach 2 for collaborate signals
  #aggDirections =aggSignals
  
  expectedPos = aggDirections
  
  returnList = list(aggDirections = aggDirections,
                    sgnlDirct = sgnlDirct,
                    expectedPos = expectedPos
                    
  )
  return(returnList)
  
}





# 2 calculate weight for signals generated from each indicator  [sum of weights = 1]
# [To be developed: based on store (PD)]   
SignalWeights = function(){
               # rsi, macd, ema
  sglWeights = c(1, 0, 0)
  return(sglWeights)
}



# 3_ SignalDirections() [based on algorithm and store ] return a vector with length = number of algorithms
SignalDirections <-	function(store,series) {
  sgnlDirct = rep(0,length(store$strategies))
  
  for(strategyIndex in 1:length(store$strategies)){
    
    sgnlDirct[strategyIndex] = store$strategies[[strategyIndex]]$algorithm(store,series)
    
    
  }
  
  return(sgnlDirct)
}


# 4_ position sizing based on open price
PosSizing = function(op){
  openDiffs <- lapply(op,diff)
  absOpenDiffs    <- lapply(openDiffs,abs)
  avgAbsDiffs <- sapply(absOpenDiffs,mean,na.rm=TRUE)
  largestAvgAbsDiffs <- max(avgAbsDiffs)
  positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
  # print(positionSizes)
  
  return(positionSizes)
}

