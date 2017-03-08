#params for this strategy
params = list(series=c(1,2,3,4,5,6,7,8,9,10),
              sLookback=9,
              mLookback=18,
              lLookback=22, 
              iniDay=22)    

##################################              RMA             ###################################
# The exponential moving average (EMA) is a type of moving average.
# The 9, 18, 22 EMAs are used to the short/middle/long-term average by optimizing parameters.

getOrders = function(store, newRowList, currentPos, params) {
  # initialize vectors
  allzero = rep(0, length(newRowList))

  # creat a list to store all the information of orders
  if (is.null(store))
    store = initStore(newRowList)
  else
    store = updateStore(store, newRowList, currentPos)

  # initialize the position and all orders
  pos = allzero
  marketOrders = allzero
  limitOrders1 = allzero
  limitPrices1 = allzero
  limitOrders2 = allzero
  limitPrices2 = allzero

  # calculate the position size  
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
      close =  store$cl[[i]]
      
      if(currentPos[i] == 0) {
        # determine the long/middle/short line of EMA
        Ema_1 = EMA(close[(store$iter-params$sLookback+1):store$iter], n=params$sLookback)
        Ema_2 = EMA(close[(store$iter-params$mLookback+1):store$iter], n=params$mLookback)
        Ema_3 = EMA(close[(store$iter-params$lLookback+1):store$iter], n=params$lLookback)
        
        #determine the long/short position
        #if short line> middle line> long line, go long
        #if short line< middle line< long line, go short
        if(last(close)>last(Ema_1) && last(Ema_1)>last(Ema_2) && last(Ema_2)>last(Ema_3)) {
          pos[i] = 1 * store$posSize[i]
        }
        else if(last(close)<last(Ema_1) && last(Ema_1)<last(Ema_2) && last(Ema_2)<last(Ema_3)) {
          pos[i] = -1 * store$posSize[i]
        }
        else {
          pos[i] = currentPos[i]
        }
        
        if(pos[i] != currentPos[i])
          store$entry[i] = store$iter
      }
      else {
        if(currentPos[i] < 0) {
          ret = store$op[[i]][[store$entry[i]]]/store$op[[i]][[store$iter-1]] - 1
        }
        else {
          ret = store$op[[i]][[store$iter-1]]/store$op[[i]][[store$entry[i]]] - 1
        }
        if(ret > -0.01)
          pos[i] = currentPos[i]
        else
          pos[i] = 0
      }
      marketOrders[i] = -currentPos[i] + pos[i]
    }
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
              entry = rep(1,10),
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