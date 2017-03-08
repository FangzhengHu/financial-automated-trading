#install.packages('tseries')
setwd("./DATA/PART2")
library(zoo)
library(tseries)
filenames = list.files(full.names=TRUE)
Series = lapply(filenames, function(i){read.csv(i, stringsAsFactors=FALSE)})

cointegration = matrix(NA, nrow=45, ncol=6)
colnames(cointegration) = c('Series_x', 'Series_y', 'Beta', 'Spread_variance', 'Spread_mean', 'p_value')

i=1
for(a in 1:9) {
  for(b in (a+1):10) {
    Series_x_dates = as.Date(Series[[a]][1:1000,1])
    Series_y_dates = as.Date(Series[[b]][1:1000,1])
    Series_x = zoo(Series[[a]][1:1000,5], Series_x_dates)
    Series_y = zoo(Series[[b]][1:1000,5], Series_y_dates)
    Pair_x_y.zoo = merge(Series_x, Series_y, all=FALSE)
    Pair_x_y = as.data.frame(Pair_x_y.zoo)
    #cat("Date range is", format(start(Pair_x_y.zoo)), "to", format(end(Pair_x_y.zoo)), "\n")

    linear_model = lm(Series_x ~ Series_y + 0, data=Pair_x_y)
    beta = coef(linear_model)[1]
    #cat("Assumed hedge ratio is:", beta, "\n")

    spread = Pair_x_y$Series_x - beta*Pair_x_y$Series_y
    ht = adf.test(spread, alternative="stationary", k=0)
    #cat("ADF p-value is:", ht$p.value, "\n")
    if (ht$p.value < 0.05) {
      #cat("The spread is likely mean-reverting.\n")
    }
    else {
      #cat("The spread is not mean-reverting.\n")
    }
    cointegration[i,] = round(c(a, b, beta, var(spread), mean(spread), ht$p.value), digit=5)
    i = i + 1
  }
}

print(cointegration)