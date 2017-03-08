source('framework/data.R')
dataList = getData(directory="PART2")

volatility = matrix(NA, nrow=10, ncol=2)
colnames(volatility) = c('Series', 'Volatility(%)')


for(i in 1:10) {
  volatility[i, ] = c(i,sd(ROC(dataList[[i]]$Close[1:1000], type='discrete')[-1]))
}

volatility = round(volatility[order(volatility[,2], decreasing = TRUE), ], digit=3)
print(volatility)