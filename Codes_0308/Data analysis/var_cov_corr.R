source('framework/data.R')
dataList <- getData(directory="PART2")

#Initialise the variance, covariance and correlation matrix.
variance = matrix(NA, nrow=10, ncol=2)
colnames(variance) = c('Series', 'Variance')

covariance = matrix(NA, nrow=45, ncol=3)
colnames(covariance) = c('Series_x', 'Series_y', 'Covariance')

correlation = matrix(NA, nrow=45, ncol=3)
colnames(correlation) = c('Series_x', 'Series_y', 'Correlation')

for(a in 1:10) {
  variance[a,] = c(a, var(dataList[[a]]$Open[1:1000]))
}

i = 1
for(b in 1:9) {
  for(c in (b+1):10) {
      covariance[i,] = c(b, c, cov(dataList[[b]]$Open[1:1000], dataList[[c]]$Open[1:1000]))
      correlation[i,] = c(b, c, cor(dataList[[b]]$Open[1:1000], dataList[[c]]$Open[1:1000]))
      i = i+1
  }
}

variance = round(variance[order(variance[,2], decreasing = TRUE),], digit=2)
covariance = covariance[order(covariance[,3], decreasing = TRUE),]
correlation = correlation[order(correlation[,3], decreasing = TRUE),]
print(variance)
print(covariance)
print(correlation)