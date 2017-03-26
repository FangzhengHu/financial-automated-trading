Trix =  function(store,series){
  
  sgnlDirct = 0  
  
  if (series %in% params$Trix$series){
    
    dataIndex = (store$iter-params$iniDay+1):store$iter
    currentPos = store$aggregateResults$oldPositions[store$iter,series]
    close = store$cl[[series]][dataIndex]
    
    Trix = last(TRIX(close, n=params$Trix$lookback, nSig=params$Trix$lookback_sig))
    
    if(is.nan(Trix$TRIX))        Trix$TRIX = 0
    if(is.nan(Trix$signal))      Trix$signal = 0
    
    # Buy/sell signals are generated when the TRIX crosses above/below the signal line and
    # is also above/below zero.      
    if(Trix$TRIX>0 && Trix$TRIX>Trix$signal)
      sgnlDirct = 1 
    else if(Trix$TRIX<0 && Trix$TRIX<Trix$signal)
      sgnlDirct = -1 
    else
      sgnlDirct = currentPos

    
    
  }
  
  
  return(sgnlDirct)
  
  
}