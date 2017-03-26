

source('strategies/AA1_1_macd.R');
source('strategies/AA1_2_ema.R');
source('strategies/AA1_3_mfi.R');
source('strategies/AA1_4_adx.R');
source('strategies/AA1_5_trix.R');
source('strategies/AA1_6_sar.R')




## initialize store [at the first trading day]
initStore = function(newRowList) {
  return(list(iter = 1,
              op = initOpStore(newRowList),
              hi = initHiStore(newRowList),
              lo = initLoStore(newRowList),
              cl = initClStore(newRowList),
              vo = initVoStore(newRowList),
              
              spread=matrix(0,nrow=maxRows,ncol=1),
              
              longStopLoss = matrix(0,nrow=maxRows,ncol=length(newRowList)),
              shortStopLoss = matrix(9999,nrow=maxRows,ncol=length(newRowList)),
              
              longStopLoss_P = matrix(0,nrow=maxRows,ncol=length(newRowList)),
              shortStopLoss_P = matrix(9999,nrow=maxRows,ncol=length(newRowList)),
              
              pairTrading = list(
                spread=matrix(0,nrow=maxRows,ncol=1),
                waitForShort = matrix(0,nrow=maxRows,ncol=1),
                waitForLong  = matrix(1,nrow=maxRows,ncol=1)
              ),
              
              strategies = list(
                # UosContrarian
                macd =list(
                  activeOrNot        = 0,
                  algorithm          = Macd

                )
                
                ,
                
                ema = list(
                  activeOrNot        = 0,
                  algorithm          = Ema,
                  entry              = rep(2,10)
                )
                
                ,
                
                mfi = list(
                  activeOrNot        = 0,
                  algorithm          = Mfi
                )
                
                ,
                
                adx = list(
                  activeOrNot        = 0,
                  algorithm          = Adx
                )
                ,
                
                trix = list(
                  activeOrNot        = 0,
                  algorithm          = Trix
                )
                ,
                sar = list(activeOrNot        = 0,
                           algorithm          = Sar
                )

                
                
                
                # cci = list(
                #  activeOrNot        = 0,
                #  algorithm          = Cci,
                #  advicedDirections  = matrix(0,nrow=maxRows,ncol=length(newRowList)),
                #  executedDirections = matrix(0,nrow=maxRows,ncol=length(newRowList)),
                #  entry              = rep(2,10),
                #  waitForLong = rep(0,length(params$Cci$series)),
                #  waitForLong = rep(0,length(params$Cci$series))
                # )
                
                #ema = list(
                #  activeOrNot        = 0,
                #  algorithm          = Ema,
                #  advicedDirections  = matrix(0,nrow=maxRows,ncol=length(newRowList)),
                #  executedDirections = matrix(0,nrow=maxRows,ncol=length(newRowList))
                # )
                
                
              ),
              
              
              aggregateResults = list(
                finalDirections = matrix(0,nrow=maxRows,ncol=length(newRowList)),
                marketOrders = matrix(NA,nrow=maxRows,ncol=length(newRowList)),
                oldPositions = matrix(0,nrow=maxRows,ncol=length(newRowList)),
                expectedPos = matrix(NA,nrow=maxRows,ncol=length(newRowList))
              ),
              MYpnlList = lapply(1:10,function(x) matrix(0,nrow=maxRows,ncol=5,
                                                         dimnames = list(NULL,c("MYDailyPnL","MYCumPnL","MYCumMax","MYmd","MYPdRatio"))))
              
              
              
  ))
}



# functions for managing the store
## sub-functions for initStore()
initOpStore  = function(newRowList) {
  opStore = lapply(newRowList, function(x) x$Open)
  return(opStore)
}
initHiStore  = function(newRowList) {
  hiStore = lapply(newRowList, function(x) x$High)
  return(hiStore)
}
initLoStore  = function(newRowList) {
  loStore = lapply(newRowList, function(x) x$Low)
  return(loStore)
}
initClStore  = function(newRowList) {
  clStore = lapply(newRowList, function(x) x$Close)
  return(clStore)
}
initVoStore  = function(newRowList) {
  voStore = lapply(newRowList, function(x) x$Volume)
  return(voStore)
}

## sub-algorithms of taking position for initStore()



## sub-functions for updateStore()
updateOpStore = function(opStore, newRowList) {
  opStore = mapply(function(x, y) rbind(x, y$Open), opStore, newRowList, SIMPLIFY=FALSE)
  return(opStore)
}
updateHiStore = function(hiStore, newRowList) {
  hiStore = mapply(function(x, y) rbind(x, y$High), hiStore, newRowList, SIMPLIFY=FALSE)
  return(hiStore)
}
updateLoStore = function(loStore, newRowList) {
  loStore = mapply(function(x, y) rbind(x, y$Low), loStore, newRowList, SIMPLIFY=FALSE)
  return(loStore)
}
updateClStore = function(clStore, newRowList) {
  clStore = mapply(function(x, y) rbind(x, y$Close), clStore, newRowList, SIMPLIFY=FALSE)
  return(clStore)
}
updateVoStore = function(voStore, newRowList) {
  voStore = mapply(function(x, y) rbind(x, y$Volume), voStore, newRowList, SIMPLIFY=FALSE)
  return(voStore)
}

updateSpreadStore <- function(spreadStore, newRowList, iter) {
  spreadStore[iter] <- newRowList[[8]]$Close - newRowList[[9]]$Close
  return(spreadStore)
}

## update store [everyday except from the first trading day]
updateStore = function(store, newRowList, currentPos) {
  store$iter = store$iter + 1
  store$op = updateOpStore(store$op, newRowList) 
  store$hi = updateHiStore(store$hi, newRowList)
  store$lo = updateLoStore(store$lo, newRowList)
  store$cl = updateClStore(store$cl, newRowList)
  store$vo = updateVoStore(store$vo, newRowList)
  
  store$aggregateResults$oldPositions[store$iter,] = currentPos
  store$spread = updateSpreadStore(store$spread,newRowList,store$iter)
  return(store)
}

###############################################################################