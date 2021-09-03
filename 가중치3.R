setwd("C:/Users/user/Desktop/new")
거리점수총합 <- read.csv("거리점수총합.csv")
minmax <- read.csv("scaling_add_11.19.csv")
격자 <- cbind(거리점수총합[,c(2)],minmax[,-c(1)])

위경도 <- 거리점수총합[,c(2,3,4)]

columns <- c("버스1","버스2","버스3","아파트1","아파트2","아파트3","교차로","유치원"
             ,"마트","지하철","노인장애인","보행자사고","초등3","초등2","초등1")
weights <- c(2,3,4,2,2.5,3,3,3,3,3,2,5,3,2.5,2)
minmax <- minmax[,-c(1)]
minmax$총합 <- minmax[,columns[1]] * weights[1]
for (i in 2:length(columns)) {
  minmax$총합 <- minmax$총합 + 격자[,columns[i]] *weights[i]
}
minmax_1 <- cbind(위경도,minmax)
write.csv(minmax_1, "격자_총합11.19.10.csv", row.names=T)
