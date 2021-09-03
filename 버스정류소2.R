rm(list=ls())

setwd ("D:/그룹 공유/공공데이터인턴/빅데이터분야/바닥 신호등")

격자 <- read.csv("격자 point.csv")

버스<- read.csv("3)정류소이용현황_좌표추가_최종.csv")

library(plyr)
밀집_순서 <- arrange(버스, desc(승하차.건수))
str(밀집_순서)

버스이용_3단계 <- 버스[버스$승하차.건수 >= 509, ]
버스이용_2단계 <- 버스[버스$승하차.건수 <509 & 버스$승하차.건수 >= 173, ]
버스이용_1단계 <- 버스[버스$승하차.건수 <173 , ]


library("geosphere")
library("dplyr")
격자$버스거리_3단계 <- 0
격자$버스거리_2단계 <- 0
격자$버스거리_1단계 <- 0

for (i in 1:nrow(격자)) {
  for (j in 1:nrow(버스이용_3단계)) {
    loc1 <- c(격자$경도[i], 격자$위도[i])
    loc2 <- c(버스$GPS_LONG [j], 버스$GPS_LATI[j])
    dist <- distHaversine(loc1, loc2)
    if (dist <= 100) {
      격자$버스거리_3단계[i] <-2
      break
    }
  }
}

for (i in 1:nrow(격자)) {
  for (j in 1:nrow(버스이용_2단계)) {
    loc1 <- c(격자$경도[i], 격자$위도[i])
    loc2 <- c(버스$GPS_LONG [j], 버스$GPS_LATI[j])
    dist <- distHaversine(loc1, loc2)
    if (dist <= 100) {
      격자$버스거리_2단계[i] <-2
      break
    }
  }
}


for (i in 1:nrow(격자)) {
  for (j in 1:nrow(버스이용_1단계)) {
    loc1 <- c(격자$경도[i], 격자$위도[i])
    loc2 <- c(버스$GPS_LONG [j], 버스$GPS_LATI[j])
    dist <- distHaversine(loc1, loc2)
    if (dist <= 100) {
      격자$버스거리_1단계[i] <-2
      break
    }
  }
}

거리_버스정류소그림1 <- 격자%>%filter(버스거리_1단계 >= 1)
거리_버스정류소그림2 <- 격자%>%filter(버스거리_2단계 >= 1)
거리_버스정류소그림3 <- 격자%>%filter(버스거리_3단계 >= 1)


write.csv(격자, "거리_버스정류소.csv", row.names=T)
write.csv(거리_버스정류소그림1, "거리_버스정류소그림1.csv", row.names=T)
write.csv(거리_버스정류소그림2, "거리_버스정류소그림2.csv", row.names=T)
write.csv(거리_버스정류소그림3, "거리_버스정류소그림3.csv", row.names=T)
