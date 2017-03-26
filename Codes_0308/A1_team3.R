params = list(iniDay=64,
              momentumTS = c(1,2,3,4,5,6,7),
              pairTradingTS = c(8,9),
              expTS = 10,
              
              stopLossPct = 0.03,
              weightPower = 0.5,
              maxWeight=2,
              
              # 0
              PairTrading = list(lookback=20,threshold=25,sdParam=1, ts=c(8,9)),
              
              # 1
              Macd = list(series=c(1,2,3,4,5,6,7), fLookback=12,sLookback=26,
                          lookback_sig=9),
              # 2
              Ema = list(series=c(1,2,3,4,5,6,7),sLookback=7, mLookback=10,lLookback=20),
              
              # 3
              Mfi = list(series=c(1,2,3,4,5,6,7),threshold=25,
                         lookback=10,lookback_sig=5),
              # 4
              Adx = list(series=c(1,2,3,4,5,6,7),lookback=15,
                         trendStrength=10,threshold=5),
              # 5
              Trix = list(series=c(1,2,3,4,5,6,7),lookback=16,
                          lookback_sig=9,iniDay=64),
              
              # 6 
              Sar = list(series=c(1,2,3,4,5,6,7))
)


source('strategies/A1_store_and_algorithms.R'); 
source('strategies/A1_backtester.R'); #A1_position_generation
source('strategies/A1_position_generation.R');


# "A1_team3"=list(lookback=10,threshold=25,series=1:10),

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
  
  
  
  
  if (store$iter > params$iniDay) {
    print(store$iter)
    
    ################################ generate expected position [1/0/-1] for next day ###############
    for(ts in 1:length(newRowList)){
      
      ##### 1_Trend Following  #####################################
      # params$momentumTS
      if(ts %in% params$momentumTS){
        
        returnedList = TrendFollow(store,ts)
        
         store$aggregateResults$finalDirections[store$iter, ts] = returnedList$aggDirections  # # aggDirections = -1/0/1
        
         expectedPos[ts] = returnedList$expectedPos

         ############## implement stop loss #########
         if(currentPos[ts]>0  
            && store$op[[ts]][[store$iter]] < store$longStopLoss[store$iter-1,ts] 
            && expectedPos[ts] > 0
         ){
           
           expectedPos[ts] = 0
           cat(store$iter,"long stop loss executed \n")
         }  
         
         if(currentPos[ts]<0  
            && store$op[[ts]][[store$iter]] > store$shortStopLoss[store$iter-1,ts] 
            && expectedPos[ts] < 0
         ){
           
           expectedPos[ts] = 0
           cat(store$iter,"short stop loss executed \n")
           
         }
         
         
         
         ############### update stop loss points for TrendFollow series ###############
         longStopL = round((1-params$stopLossPct)*store$op[[ts]][store$iter],2)
         shortStopL = round((1+params$stopLossPct)*store$op[[ts]][store$iter],2)
         
         if(expectedPos[ts]==0){
           store$longStopLoss[store$iter, ts]  = longStopL
           store$shortStopLoss[store$iter, ts] = shortStopL
         } 
         if(expectedPos[ts]>0) store$longStopLoss[store$iter, ts] = max( longStopL, 
                                                                         store$longStopLoss[store$iter-1, ts])
         
         if(expectedPos[ts]<0) store$shortStopLoss[store$iter, ts] = min( shortStopL, 
                                                                          store$shortStopLoss[store$iter-1, ts])
         
        
      }
      
      ######## 2_Pair Trading  ##############################################
      # 8
      
      if(ts %in% 8){  # 8 
        # print(expectedPos)
        retList = pairTrading(store,params,currentPos,expectedPos)
        
        expectedPos[8] = retList$expectedPos
        expectedPos[9] = - expectedPos[8]
        
        store$pairTrading$waitForLong[store$iter] = retList$waitForLong
        store$pairTrading$waitForShort[store$iter]= retList$waitForShort
        
        
        temp = store$iter 
        # cat(store$iter,"MYCumPnL",store$MYpnlList[[8]][store$iter-1,"MYCumPnL"],"\n")
      }
      
      
      
      ######## 3_Exponential position sizing strategy ##########################
      # params$expTS
      if(ts %in% 11){
        expectedPos[params$expTS] = expPos(store,params,expectedPos)
      }
      
      
      ### moving exit condition based on moving maximum loss tolerance 
      if(ts %in% 1:9){
        
        
        # store$MYpnlList[[j]][store$iter-1,"MYPdRatio"]
        if(store$iter>500){
          movingStop = -200* (1001-store$iter)/500 
          # cat("movingStop",movingStop,"\n")
          
          StayOrNot = ifelse(store$MYpnlList[[ts]][[store$iter-1,"MYCumPnL"]]<= movingStop,0,1)
          
          
          expectedPos[ts] = expectedPos[ts] * StayOrNot
        }
        
      }
      
      
    }
    

    ####### position sizing based on open price ######################################
    meanDiff      = rep(0,10)
    positionSizes = rep(1,10)
    
    openPrices = lapply(1:10, function(x) tail(store$op[[x]],params$iniDay) ) # params$iniDay = 20
    
    if(store$iter > params$iniDay)  positionSizes = PosSizing(openPrices)
    
    expectedPos = expectedPos * positionSizes
    
    
    ###### position sizing based on maximum drawdown###########
    if (store$iter > (params$iniDay+100)) {  
      myMD = sapply(1:9, function(x) store$MYpnlList[[x]][store$iter-1,"MYmd"])
      myMD = as.numeric(myMD)
      
      myMD = sapply(1:9,function(x) max(myMD[x],0.01))
      mdWeight = round(max(myMD)/myMD,2)
      mdWeight = c(mdWeight,1)
      
     
      mdWeight = mdWeight^params$weightPower - 0.2  ### OPTIMIZATION needed  
      mdWeight = sapply(1:10,function(x) min(mdWeight[x],params$maxWeight))  ### OPTIMIZATION needed  
      
      cat("mdWeight:",mdWeight,"\n")
      expectedPos = expectedPos * mdWeight
      

    }
 
    
  }
  

  
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





