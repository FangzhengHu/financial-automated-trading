source('framework/data.R')
dataList = getData(directory="PART1")
Movement_matrix = matrix(NA, nrow=10, ncol=3)
colnames(Movement_matrix) = c("Series", "Upward probability", "Downward probability")

for(i in 1:10) {
  Series_x = dataList[[i]][1:1000,]
  length = nrow(Series_x)
  Upward_days = as.numeric((Series_x$Close - Series_x$Open) > 0)
  Upward_probability = sum(Upward_days)/length
  Downward_probability = (length - sum(Upward_days))/length
  Movement_matrix[i,] = c(i, Upward_probability, Downward_probability)
}

print(Movement_matrix)