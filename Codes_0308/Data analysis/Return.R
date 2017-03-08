source('framework/data.R')
library(TTR)
dataList = getData(directory="PART2")

pdf("pdf/Returns_TTR.pdf")
par(mfrow=c(2,2))
for(i in 1:10) {
  log_returns = ROC(dataList[[i]]$Close[1:1000], type='continuous')
  simple_returns = ROC(dataList[[i]]$Close[1:1000], type='discrete')
  plot(log_returns, main = paste("Log returns of Series", i))
  hist(log_returns, breaks=50, col='brown', main = paste('Histogram of log returns of Series', i))
  plot(simple_returns, main = paste("Simple returns of Series", i))
  hist(simple_returns, breaks=50, col='brown', main = paste('Histogram of simple returns of Series', i))
}
dev.off()