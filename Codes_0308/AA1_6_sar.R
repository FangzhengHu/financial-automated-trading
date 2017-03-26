Sar =  function(store,series){
  
  sgnlDirct = 0  
  
  if (series %in% params$Sar$series){
    
    dataIndex = (store$iter-params$iniDay+1):store$iter
    currentPos = store$aggregateResults$oldPositions[store$iter,series]
    hl = cbind(store$hi[[series]][dataIndex], store$lo[[series]][dataIndex])
    Cl = store$cl[[series]][dataIndex]
    
    Sar = last(SAR(hl,accel = c(0.02, 0.2))) #Average Directional Index
    
    if(is.nan(Sar))        Sar$SAR = 0
    
    
    if(Sar > Cl)  #Judge if ADX meet the requirments of signals
       # Long condition
        sgnlDirct = -1 
      
      else if(Sar < Cl)  # Short condition
        sgnlDirct = 1 
      
      
    else 
      sgnlDirct = 0 #else position return to 0
    
  }
  
  
  return(sgnlDirct)
  
  
}