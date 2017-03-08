params = list(series=c(1,2,3,4,5,6,7,8,9,10),
              threshold=15,
              iniDay=45)

getOrders = function(store, newRowList, currentPos, params) {
  allzero = rep(0, length(newRowList))
  
  if (is.null(store))
    store = initStore(newRowList)
  else
    store = updateStore(store, newRowList, currentPos)
  
  pos = allzero
  marketOrders = allzero
  limitOrders1 = allzero
  limitPrices1 = allzero
  limitOrders2 = allzero
  limitPrices2 = allzero
  
  if(store$iter >= params$iniDay) {
    for (i in params$series) {
      store$meanDiff[i] = mean(abs(diff(store$op[[i]][(store$iter-params$iniDay+1):store$iter])[-1]))
    }
    for (i in params$series) {
      store$posSize[i] = round(max(store$meanDiff)/store$meanDiff[i])
    }
  }
  if (store$iter >= params$iniDay) {
    for (i in params$series) {
      dataIndex = (store$iter - params$iniDay + 1):store$iter
      
      hlc = cbind(store$hi[[i]][dataIndex], store$lo[[i]][dataIndex], store$cl[[i]][dataIndex])
      
      smi = last(SMI(hlc, n=9, nFast=2, nSlow=25, nSig=9))
      
      if(smi$SMI>smi$signal && smi$SMI>params$threshold) {
        pos[i] = 1 * store$posSize[i]
      } 
      else if(smi$signal>smi$SMI && smi$SMI<(-params$threshold)) {
        pos[i] = -1 * store$posSize[i]
      }
      else
        pos[i] = currentPos[i]
    }
    marketOrders = -currentPos + pos
  }
  return(list(store = store,
              marketOrders = marketOrders,
              limitOrders1 = limitOrders1,
              limitPrices1 = limitPrices1,
              limitOrders2 = limitOrders2,
              limitPrices2 = limitPrices2))
}

initOpStore  = function(newRowList) {
  opStore = lapply(newRowList, function(x) x$Open)
  return(opStore)
}
updateOpStore = function(opStore, newRowList) {
  opStore = mapply(function(x, y) rbind(x, y$Open), opStore, newRowList, SIMPLIFY=FALSE)
  return(opStore)
}

initHiStore  = function(newRowList) {
  hiStore = lapply(newRowList, function(x) x$High)
  return(hiStore)
}
updateHiStore = function(hiStore, newRowList) {
  hiStore = mapply(function(x, y) rbind(x, y$High), hiStore, newRowList, SIMPLIFY=FALSE)
  return(hiStore)
}

initLoStore  = function(newRowList) {
  loStore = lapply(newRowList, function(x) x$Low)
  return(loStore)
}
updateLoStore = function(loStore, newRowList) {
  loStore = mapply(function(x, y) rbind(x, y$Low), loStore, newRowList, SIMPLIFY=FALSE)
  return(loStore)
}

initClStore  = function(newRowList) {
  clStore = lapply(newRowList, function(x) x$Close)
  return(clStore)
}
updateClStore = function(clStore, newRowList) {
  clStore = mapply(function(x, y) rbind(x, y$Close), clStore, newRowList, SIMPLIFY=FALSE)
  return(clStore)
}

initVoStore  = function(newRowList) {
  voStore = lapply(newRowList, function(x) x$Volume)
  return(voStore)
}
updateVoStore = function(voStore, newRowList) {
  voStore = mapply(function(x, y) rbind(x, y$Volume), voStore, newRowList, SIMPLIFY=FALSE)
  return(voStore)
}

initStore = function(newRowList) {
  return(list(iter = 1,
              op = initOpStore(newRowList),
              hi = initHiStore(newRowList),
              lo = initLoStore(newRowList),
              cl = initClStore(newRowList),
              vo = initVoStore(newRowList),
              meanDiff = rep(0,10),
              posSize=rep(1,10)))
}
updateStore = function(store, newRowList, currentPos) {
  store$iter = store$iter + 1
  store$op = updateOpStore(store$op, newRowList) 
  store$hi = updateHiStore(store$hi, newRowList)
  store$lo = updateLoStore(store$lo, newRowList)
  store$cl = updateClStore(store$cl, newRowList)
  store$vo = updateVoStore(store$vo, newRowList)
  return(store)
}