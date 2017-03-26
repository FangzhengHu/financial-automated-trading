
Macd <-	function(store,series) {
  
  sgnlDirct = 0  
  
  if (series %in% params$Macd$series){
    dataIndex = (store$iter-2*params$Macd$sLookback+1):store$iter
    
    close =  store$cl[[series]]
    currentPos = store$aggregateResults$oldPositions[store$iter,series]
    
    Macd = last(MACD(close, nFast=params$Macd$fLookback, nSlow=params$Macd$sLookback, nSig=params$Macd$lookback_sig))
    if(is.nan(Macd$macd)) Macd$macd = 0
    if(is.nan(Macd$signal)) Macd$signal = 0
    
    
    if(Macd$macd>Macd$signal && Macd$macd>0)
      sgnlDirct = 1 
    else if(Macd$macd<Macd$signal && Macd$macd<0)
      sgnlDirct = -1 
    else
      sgnlDirct = currentPos
    
  }
  
  return(sgnlDirct)
  
}