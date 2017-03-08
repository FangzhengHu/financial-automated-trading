# Relative Strength Index (RSI) measures the speed and change of price movements
RsiContrarian <-	function(store,series) {
  sgnlDirct = 0  
  
  if (series %in% params$Rsi$series){
    # decide if we should go long/short/flat (returning 1/-1/0)
    startIndex <- store$iter - params$Rsi$lookback - 1
    rsi <- last(RSI(store$cl[[series]][startIndex:store$iter],n=params$Rsi$lookback)) 
    
    if(is.nan(rsi)) rsi = 0
    # no action
    if (rsi > (50 + params$Rsi$threshold)) sgnlDirct = -1 # short
    if (rsi < (50 - params$Rsi$threshold)) sgnlDirct =  1 # long
    
  }
  
  
  return(sgnlDirct)
  
}