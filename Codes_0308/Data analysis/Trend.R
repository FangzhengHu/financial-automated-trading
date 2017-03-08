source('framework/data.R')
dataList = getData(directory="PART2")

pdf("pdf/Trend.pdf")
for(i in 1:10) {
  chartSeries(dataList[[i]][1:1000], theme='white', name=paste('Series', i))
}
dev.off()