source('framework/data.R')
dataList = getData(directory="PART2")

Conditional_probabilities = matrix(NA, nrow=10, ncol=4)
colnames(Conditional_probabilities) = c("P(U|U)", "P(D|U)", "P(U|D)", "P(D|D)")

for (i in 1:10) {
  Series_x = dataList[[i]][1:1000,]
  length = nrow(Series_x)
  Up_days = as.numeric((Series_x$Close - Series_x$Open) > 0)
  Up_days_lag = as.data.frame(cbind(Up_days[1:(length-1)], Up_days[2:length]))
  Up_up_days = length(which(Up_days_lag$V1 + Up_days_lag$V2 == 2))
  Up_down_days = length(which(Up_days_lag$V1 == 0 & Up_days_lag$V2 == 1))
  Down_up_days = length(which(Up_days_lag$V1 == 1 & Up_days_lag$V2 == 0))
  Down_down_days = length(which(Up_days_lag$V1 + Up_days_lag$V2 == 0))
  Conditional_probabilities[i,] = c(Up_up_days/sum(Up_days),
                                    Up_down_days/sum(Up_days),
                                    Down_up_days/(length - sum(Up_days)),
                                    Down_down_days/(length - sum(Up_days)))
}

print(round(Conditional_probabilities, digit=3))