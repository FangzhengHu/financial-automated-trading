
###############################################################################
# functions for generating the expected postion 
###############################################################################




## 1_ Trend following strategy [Obtain Aggregate Signal Directions]
TrendFollow <-	function(store,series) {
  
  sgnlDirct = rep(0,length(store$strategies))
  
  for(strategyIndex in 1:length(store$strategies)){
    
    sgnlDirct[strategyIndex] = store$strategies[[strategyIndex]]$algorithm(store,series)
    
  }
  
  SignalWeights = c(1,   # 1_macd
                    1,   # 2_ema
                    1,   # 3_mfi
                    0,   # 4_adx
                    1,    # 5_trix
                    1   # 6_sar
                        # 7_kst
                   )
  aggSignals = sum( sgnlDirct * SignalWeights ) / sum(SignalWeights)  
  
  # approach 1 for collaborate signals
  if(abs(aggSignals) >= 4/4){
    
    if(aggSignals>0){
      aggDirections = 1
    }else{
      aggDirections = -1
    }
  }else{
    aggDirections = 0
  }
  
  # approach 2 for collaborate signals
  #aggDirections = aggSignals
  
  expectedPos = aggDirections
  
  returnList = list(aggDirections = aggDirections,
                    sgnlDirct = sgnlDirct,
                    expectedPos = expectedPos
                    
  )
  return(returnList)
  
}


## 2_pair trading strategy
pairTrading = function(store,params,currentPos,expectedPos){
  
  startIndex <-  store$iter - params$PairTrading$lookback
  spread = store$spread[store$iter]
  
  bbands <- last(BBands(store$spread[startIndex:store$iter],
                        n=params$PairTrading$lookback,
                        sd=params$PairTrading$sdParam))
  #print(bbands)
  if(currentPos[8]>0)   curPos = 1
  if(currentPos[8]<0)   curPos = -1
  if(currentPos[8]==0)  curPos = 0
  
  if(store$pairTrading$waitForLong[store$iter-1] == 1){
    if(spread < bbands["dn"]){
      # cat(store$iter, "spread < bbands[dn]","go long\n")
      # go long
      expectedPos[8] = 1
      store$pairTrading$waitForLong[store$iter] = 0
      store$pairTrading$waitForShort[store$iter] = 1
      
    }else{
      # cat(store$iter, "waitForLong","go flat\n")
      # go flat
      expectedPos[8] = curPos
      
      store$pairTrading$waitForLong[store$iter] = 1
      store$pairTrading$waitForShort[store$iter] = 0
    }
  }
  
  
  
  
  if(store$pairTrading$waitForShort[store$iter-1] == 1){
    
    if(spread > bbands["up"]){
      # cat(store$iter,"spread > bbands[up]","go short \n")
      # go short
      expectedPos[8] = -1
      # expectedPos[9] =  1
      
      store$pairTrading$waitForLong[store$iter] = 1
      store$pairTrading$waitForShort[store$iter] = 0
      
    }else{
      # cat(store$iter,"waitForShort","go flat \n")
      
      # go flat
      expectedPos[8] = curPos
      # expectedPos[9] = - curPos
      
      store$pairTrading$waitForShort[store$iter] = 1
      store$pairTrading$waitForLong[store$iter] = 0
      
    }
    
  }
  returnList = list(expectedPos= expectedPos[8],
                    waitForLong = store$pairTrading$waitForLong[store$iter],
                    waitForShort = store$pairTrading$waitForShort[store$iter])
  
  return(returnList)
}


## 3_Exponential position sizing strategy:
expPos = function(store,params,expectedPos){
  
  ######################## exponential position sizing strategy###############################
  if(store$iter>params$iniDay ){ # 100000
    if(  (store$iter%%3) ==1)  expectedPos[params$expTS] = 1
    
    if(  (store$iter%%3) ==2) expectedPos[params$expTS] = 0
    
    if(  (store$iter%%3) ==0)  expectedPos[params$expTS] = 0
    
    expectedPos[params$expTS] = expectedPos[params$expTS] * 100 * (2^store$iter)
    
    if(store$iter>(params$iniDay+6)){
      ExitOrNot = ifelse(store$MYpnlList[[params$expTS]][store$iter-1,"MYPdRatio"]>3,0,1)
      
      expectedPos[params$expTS] = expectedPos[params$expTS] * ExitOrNot
    }
    
  }
  return(expectedPos[params$expTS])
  
}






## position sizing based on open price
PosSizing = function(op){
  openDiffs <- lapply(op,diff)
  absOpenDiffs    <- lapply(openDiffs,abs)
  avgAbsDiffs <- sapply(absOpenDiffs,mean,na.rm=TRUE)
  largestAvgAbsDiffs <- max(avgAbsDiffs)
  positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
  # print(positionSizes)
  
  return(positionSizes)
}

