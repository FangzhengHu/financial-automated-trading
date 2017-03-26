Mfi =  function(store,series){
  
  sgnlDirct = 0  
  
  if (series %in% params$Mfi$series){
    
    dataIndex = (store$iter-params$iniDay+1):store$iter
    currentPos = store$aggregateResults$oldPositions[store$iter,series]
    hlc = cbind(store$hi[[series]][dataIndex], store$lo[[series]][dataIndex], store$cl[[series]][dataIndex])
    vol = store$vo[[series]][dataIndex]
    
    
    Mfi = MFI(hlc,vol,n=params$Mfi$lookback)
    Mfi_signal = SMA(Mfi,n=params$Mfi$lookback_sig)
    
    
    
    if(is.nan(last(Mfi)))        last(Mfi) = 0
    if(is.nan(last(Mfi_signal))) last(Mfi_signal) = 0
    
    
    if (last(Mfi)>last(Mfi_signal) && last(Mfi)>50+params$Mfi$threshold)
      sgnlDirct = 1 
    else if (last(Mfi)<last(Mfi_signal) && last(Mfi)<50-params$Mfi$threshold)
      sgnlDirct = -1 
    else
      sgnlDirct = currentPos
 
  }
  
  
  return(sgnlDirct)
  
  
}