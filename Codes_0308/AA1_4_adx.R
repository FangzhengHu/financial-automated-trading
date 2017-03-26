Adx =  function(store,series){
  
  sgnlDirct = 0  
  
  if (series %in% params$Adx$series){
    
    dataIndex = (store$iter-params$iniDay+1):store$iter
    currentPos = store$aggregateResults$oldPositions[store$iter,series]
    hlc = cbind(store$hi[[series]][dataIndex], store$lo[[series]][dataIndex], store$cl[[series]][dataIndex])
    
    Adx = last(ADX(hlc, n=params$Adx$lookback)) #Average Directional Index
    
    if(is.nan(Adx$ADX))        Adx$ADX = 0
    if(is.nan(Adx$DIp))        Adx$DIp = 0
    if(is.nan(Adx$DIn))        Adx$DIn = 0
    
    if(Adx$ADX > params$Adx$trendStrength) { #Judge if ADX meet the requirments of signals
      if(Adx$DIp - Adx$DIn > params$Adx$threshold) { # Long condition
        sgnlDirct = 1 
      }
      else if(Adx$DIn - Adx$DIp > params$Adx$threshold) { # Short condition
        sgnlDirct = -1 
      }
      else {
        sgnlDirct = currentPos #Keep the position
      }
    }
    else {
      sgnlDirct = 0 #else position return to 0
    }
    
    

    
  }
  
  
  return(sgnlDirct)
  
  
}