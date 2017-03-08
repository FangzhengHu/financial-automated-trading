Ema =  function(store,series){
  
  sgnlDirct = 0  
  
  if (series %in% params$Ema$series){
    
    # sgnlDirct = 1
    close =  store$cl[[series]]
    
    currentPos = store$aggregateResults$oldPositions[store$iter,series]
    
    
    Ema_1 = EMA(close[(nrow(close)-params$Ema$sLookback+1):nrow(close)], n=params$Ema$sLookback)
    Ema_2 = EMA(close[(nrow(close)-params$Ema$mLookback+1):nrow(close)], n=params$Ema$mLookback)
    Ema_3 = EMA(close[(nrow(close)-params$Ema$lLookback+1):nrow(close)], n=params$Ema$lLookback)
    
    if(is.nan(Ema_1) || is.nan(Ema_2) || is.nan(Ema_3)){
      Ema_1 = 0
      Ema_2 = 0
      Ema_3 = 0
    }
    if(last(close)>last(Ema_1) && last(Ema_1)>last(Ema_2) && last(Ema_2)>last(Ema_3)) {
      # go long if short line> middle line> long line
      sgnlDirct = 1
    }
    else if(last(close)<last(Ema_1) && last(Ema_1)<last(Ema_2) && last(Ema_2)<last(Ema_3)) {
      # go short if short line< middle line< long line
      sgnlDirct = -1
    }
    else {
      sgnlDirct = currentPos
    }
    
    
  }
  
  
  return(sgnlDirct)
  
  
}