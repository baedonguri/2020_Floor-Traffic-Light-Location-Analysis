rm(list=ls())

setwd ("D:/그룹 공유/공공데이터인턴/빅데이터분야/바닥 신호등")

격자 <- read.csv("격자 point.csv")

요소 <- read.csv("인천광역시_서구_노인장애인보호구역_20200713_1594617323275_1309.csv" )

library("geosphere")
library("dplyr")

격자$거리_요소 <-0

for (i in 1:nrow(격자)) {
  for (j in 1:nrow(요소)) {
    if (격자$거리_요소[i] == 0) {
      loc1 <- c(격자$경도[i], 격자$위도[i])
      loc2 <- c(요소$경도[j], 요소$위도[j])
      dist <- distHaversine(loc1, loc2)
      if (dist <= 100) {
        격자$거리_요소[i] <-3
      } else if (dist <= 200) {
        격자$거리_요소[i] <-2
      } else if (dist <= 300) {
        격자$거리_요소[i] <-1
      }
    } else {
      loc1 <- c(격자$경도[i], 격자$위도[i])
      loc2 <- c(요소$경도[j], 요소$위도[j])
      dist <- distHaversine(loc1, loc2)
      거리 <- 0
      if (dist <= 100) {
        거리 <-3
      } else if (dist <= 200) {
        거리 <-2
      } else if (dist <= 300) {
        거리 <-1
      }
      격자$거리_요소[i] <- max(격자$거리_요소[i], 거리)
    }
  }
}


요소그림 <- 격자%>%filter(거리_요소 >=1)


write.csv(격자, "노인장애인보호구역_거리.csv", row.names=TRUE)
write.csv(요소그림 , "노인장애인보호구역_거리그림.csv", row.names=TRUE)

