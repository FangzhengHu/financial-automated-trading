library(zoo) 
library(tseries)

checkPair <- function(filName,fileName1){
  
  gld <- read.csv(fileName, stringsAsFactors=F)
  gdx <- read.csv(fileName1, stringsAsFactors=F)
  class(gld)
  
  gld_dates <- as.Date(gld[1:500,1])
  gdx_dates <- as.Date(gdx[1:500,1])
  class(gdx_dates)
  
  gld <- zoo(gld[1:500,5])
  gdx <- zoo(gdx[1:500,5])

  
  t.zoo <- merge(gld, gdx, all=FALSE)
  t <- as.data.frame(t.zoo)
  
  cat("Date range is", format(start(t.zoo)), "to", format(end(t.zoo)), "\n")
  
  
  #sprd
  
  m <- lm(gld ~ gdx + 0, data=t)
  beta <- coef(m)[1]
  cat("Assumed hedge ratio is", beta, "\n")
  sprd <- t$gld - beta*t$gdx
  
  
  
  #p-value
  ht <- adf.test(sprd, alternative="stationary", k=0)
  cat("ADF p-value is", ht$p.value, "\n")
  
  
  if (ht$p.value < 0.05) {
    cat("The spread is likely mean-reverting.\n")
  } else {
    cat("The spread is not mean-reverting.\n")
  }
}

