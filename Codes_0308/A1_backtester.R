
# Load required libraries
library(quantmod)

MYpricesLists = NULL
MYDailyPnL      = rep(0,10)
MYCumPnL  = rep(0,10)
MYpnlList = list()

MyBacktest <- function(store=NULL) {
  
  if(store$iter>=3){
    
    MYpricesLists = lapply(1:10, function(x) cbind(
      tail(store$op[[x]],3),
      tail(store$hi[[x]],3),
      tail(store$lo[[x]],3),
      tail(store$cl[[x]],3),
      tail(store$vo[[x]],3)
    ))
    
    
    
    oldPos = store$aggregateResults$oldPositions[store$iter-2, ]
    marketOrders = store$aggregateResults$marketOrders[store$iter-2, ]
    
    # cat("iter=: ",store$iter," include data from day",store$iter-2," to day ",store$iter,"\n")
    
    MYDailyPnL <- mapply(MYfindPnL,
                         MYpricesLists = MYpricesLists, 
                         oldPos=oldPos,
                         marketOrder=marketOrders,
                         iter=rep(store$iter,10) ,
                         SIMPLIFY = TRUE
    )
    
    
    MYCumPnL = sapply(1:10, function(x) store$MYpnlList[[x]][store$iter-2,"MYCumPnL"] + MYDailyPnL[x])
    
    
    #cat("\n","\n")
  }
  return(MYpnlList = list(MYDailyPnL = MYDailyPnL,
                          MYCumPnL = MYCumPnL))
  
}



# [1] call 2, call 3
MYfindPnL <- function(MYpricesLists = NULL, oldPos=NULL, marketOrder=NULL, 
                      limitOrder1=rep(0,10), 
                      limitPrice1=rep(0,10), 
                      limitOrder2=rep(0,10), 
                      limitPrice2=rep(0,10), 
                      iter= NULL,
                      sMult=0.2) {
  
  # Fetch prices
  prices <- MYgetPrices(MYpricesLists)
  
  pnl    <- oldPos * (prices$nextOp - prices$curOp) # pnl from oldPos
  
  if (marketOrder != 0) {
    # run from day 2, where oldPos would always be 0, until penultimate day
    slippage <- slip(prices$prevCl, prices$curOp, sMult)
    
    pnl <- pnl + marketOrder * (prices$nextOp - prices$curOp) - abs(marketOrder) * slippage
  }
  
  return(pnl)
}


# [2] 
MYgetPrices <- function(MYpricesLists) {
  
  prevCl = as.numeric(MYpricesLists[1,"Close"]) #prevCl
  curOp = as.numeric(MYpricesLists[2,"Open"]) #curOp
  curHi = as.numeric(MYpricesLists[2,"High"]) #curHi
  curLo = as.numeric(MYpricesLists[2,"Low"]) #curLo
  nextOp = as.numeric(MYpricesLists[3,"Open"]) #nextOp
  
  prices     <- list(prevCl = prevCl, 
                     curOp = curOp, 
                     curHi = curHi, 
                     curLo = curLo, 
                     nextOp = nextOp)
}


# [3]
slip  <-  function(prevClose, curOpen, sMult) { 
  overnightGap  <- abs(prevClose-curOpen)
  return(sMult * overnightGap) 
}
