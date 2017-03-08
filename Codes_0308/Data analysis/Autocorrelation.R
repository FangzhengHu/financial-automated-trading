source('framework/data.R')
dataList = getData(directory="PART2")

Series_return = list()
Result_matrix = matrix(NA, nrow=10, ncol=3)
colnames(Result_matrix) = c('Series', 'Mean', 'Volatility')

pdf("pdf/Autocorrelation_abs_returns.pdf")
for(i in 1:10) {
  Series_return[[i]] = diff(log(dataList[[i]]$Close[1:1000]))
  acf(Series_return[[i]][-1], main = paste("Autocorrelation of absolute returns of Series", i))
  Result_matrix[i,] = round(c(i, mean(Series_return[[i]][-1]), sd(Series_return[[i]][-1])), digit=4)
}
dev.off()

print(Result_matrix)