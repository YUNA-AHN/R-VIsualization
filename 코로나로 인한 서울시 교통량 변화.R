setwd('C:/Users/sgvin/Desktop/개인2/대학/2022/22-2/비즈니스정보시각화프로그래밍/팀플/data/')

# 버스 데이터 불러오기
data <- read.csv("버스/버스데이터_최종.csv")
head(data)
str(data)

# 총 승차인원 생성 
data$total <- data$X00시승차총승객수+data$X1시승차총승객수+data$X2시승차총승객수+
  data$X3시승차총승객수+data$X4시승차총승객수+data$X5시승차총승객수+data$X6시승차총승객수+
  data$X7시승차총승객수+data$X8시승차총승객수+data$X9시승차총승객수+data$X10시승차총승객수+
  data$X11시승차총승객수+data$X12시승차총승객수+data$X13시승차총승객수+data$X14시승차총승객수+
  data$X15시승차총승객수+data$X16시승차총승객수+data$X17시승차총승객수+data$X18시승차총승객수+
  data$X19시승차총승객수+data$X20시승차총승객수+data$X21시승차총승객수+data$X22시승차총승객수+
  data$X23시승차총승객수

library(dplyr)
month_data = data %>% group_by(사용년월) %>% summarise(month_total = sum(total),
                                                   month_am7 = sum(X7시승차총승객수),
                                                   month_am8 = sum(X8시승차총승객수),
                                                   month_am9 = sum(X9시승차총승객수),
                                                   month_pm2 = sum(X14시승차총승객수),
                                                   month_pm3 = sum(X15시승차총승객수),
                                                   month_pm4 = sum(X16시승차총승객수))
month_data$사용년월 <- paste(substr(month_data$사용년월,1,4),substr(month_data$사용년월,5,6),sep='-')


# 코로나 데이터 불러오기 및 데이터 확인
covid <- read.csv('수정_서울시 코로나19 확진자 발생동향.csv', fileEncoding = "euc-kr")
head(covid)
str(covid)

# 코로나 데이터 기준일 숫자형으로 변경
covid$서울시.기준일 <- gsub('\\D','', covid$서울시.기준일)
covid$서울시.기준일 <- as.numeric(substr(covid$서울시.기준일,1,8))
covid[994:1016,]$서울시.기준일 = covid[994:1016,]$서울시.기준일+2000000000
covid$기준월  <- substr(covid$서울시.기준일,1,6)


month_covid = covid %>% group_by(기준월) %>% summarise(month_p = sum(서울시.추가.확진),
                                                    month_stack = sum(서울시.확진자))
month_covid$기준월 <- paste(substr(month_covid$기준월,1,4),substr(month_covid$기준월,5,6),sep='-')

## 그래프 --------------------
# 월별 코로나 확진자 라인그래프 ---
library(ggplot2)
p <- ggplot(data=month_covid, aes(x= factor(기준월) ,y = month_p, group=1))
p + geom_area(alpha=0.1, fill = 'purple 3',colour='purple 3',size=0.7)  + 
  geom_text(aes(x='2022-01',y=2000000,label = "위드코로나 시행"), color="black",nudge_x=-2, size=4)+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1)
        ,legend.position = "none")+
  geom_vline(xintercept = '2022-01',linetype = 2 ,alpha=0.5,size=0.7)+
  labs(x='월',y='확진자수')+ 
  ggtitle("서울시 코로나 확진자 수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


# 2020-2021
p <- ggplot(data=month_covid[1:23,], aes(x= factor(기준월) ,y = month_p, group=1))
p + geom_area(alpha=0.1,size=0.7,fill = 'midnight blue' ,colour='midnight blue') + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1)
        ,legend.position = "none")+
  labs(x='월',y='확진자수')+ 
  ggtitle("2020-2021 서울시 확진자 수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


# 2022
p <- ggplot(data=month_covid[24:29,], aes(x= factor(기준월) ,y = month_p, group=1))
p + geom_area(alpha=0.1,size=0.7,fill = 'midnight blue' ,colour='midnight blue') + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1)
        ,legend.position = "none")+
  labs(x='월',y='확진자수')+ 
  ggtitle("2022 서울시 확진자 수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


# 누적확진자그래프 ----------
head(month_covid)
month_covid[1:23,]
p <- ggplot(data=month_covid[1:29,], aes(x= factor(기준월) ,y = month_stack, group=1))
p + geom_area(colour='red', alpha=0.3, fill='pink') + geom_point(size=1, shape=22, fill="pink", color="red")+
  geom_text(aes(x='2022-01',y=100000000,label = "위드코로나 시행"), color="black",nudge_x=-2, size=4)+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+ 
  geom_vline(xintercept = '2022-01',linetype = 2 ,alpha=0.5,size=0.7)+
  labs(x='월',y='누적 확진자 수')+ 
  ggtitle("서울시 누적 확진자 그래프") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


## 구별 확진자 수 ----------------------
library(sf)
map <- st_read("sig.shp")
map$SIG_KOR_NM <- iconv(map$SIG_KOR_NM, localeToCharset(), from = "CP949", to = "UTF-8")
seoul_map <- map[map$SIG_CD <= 11740,]

seoul_covid <- read.csv('C:/Users/sgvin/Desktop/개인2/대학/2022/22-2/비즈니스정보시각화프로그래밍/팀플/data/자치구별확진자.csv')
seoul_covid$year  <- substr(seoul_covid$date,1,4)
library(dplyr)
library(tidyverse)
library(viridis)
library(RColorBrewer)
total_covid = seoul_covid %>% group_by(year,variable) %>% summarise(month_p = sum(value),.groups='keep')
M <- merge(seoul_map, total_covid, by.x = "SIG_KOR_NM", by.y='variable')

## 로그화
p <- ggplot() + 
  geom_sf(data = M, aes(fill = log(month_p))) +
  scale_fill_gradient(limits= c(min(log(M$month_p)),max(log(M$month_p))), low="light steel blue", high="blue 4")+
  theme(legend.text.align = 1,
        legend.title.align = 0.5,
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  geom_sf_text(data = M, aes(label = SIG_KOR_NM),size=3,family="sans",color='white')
p+facet_wrap(~year)+ 
  ggtitle("서울시 자치구별 확진자 그래프") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


## 버스 -----------------
# 월별 승차인원 라인그래프
p <- ggplot(data=month_data[6:42,], aes(x= factor(사용년월) ,y = month_total, group=1))
p + geom_line(colour='indian red 1', size=0.7) + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  geom_vline(xintercept = c('2020-02', '2020-08','2020-11','2021-06','2022-01'),linetype = 2 ,alpha=0.5,size=0.7)+
  labs(x='월',y='승차인원')+ 
  ggtitle("서울시 버스 승차인원") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))

# 전월 대비 증감 그래프 -------
library(reshape)
(month_data$month_total[2]-month_data$month_total[1])/month_data$month_total[1]*100
m8 <- c(0)
m3 <- c(0)
for (i in 1:nrow(month_data)){
  m8 <- c(m8,(month_data$month_am8[i+1]-month_data$month_am8[i])/month_data$month_am8[i]*100)
  m3 <- c(m3,(month_data$month_pm3[i+1]-month_data$month_pm3[i])/month_data$month_pm3[i]*100)
}

month_data$am8 <- m8[1:46]
month_data$pm3 <- m3[1:46]

si <- data.frame(month_data[8:43,])
new.2 <- melt(data=si, id.vars=c('사용년월'),
            measure.vars=c('am8', 'pm3'))

p <- ggplot(data=new.2, aes(x= factor(사용년월) ,y = value, group=variable,fill=variable))
p + geom_bar(stat = 'identity', position = "dodge") + 
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1))+
  labs(x='월',y='증감')+ 
  ggtitle("시간대별 전월 대비 승차인원 증감") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))


##  따릉이 --------------
bicyle <- read.csv('따릉이.csv')
bicyle$대여일시 <- substr(bicyle$대여일시, 1,7)
bicyle$대여건수 <- as.numeric(gsub('\\D','', bicyle$대여건수))
bicyle
ggplot(bicyle, aes(x = 대여일시, y = 대여건수, fill = 대여일시),element_text(size=7,angle=45,vjist=0.7))+ 
  geom_boxplot() + 
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "white")+
  theme_classic()+
  theme(axis.text.x = element_text(size =10, angle = 45, vjust  = 1, hjust=1),
        legend.position = "none")+
  labs(x='월',y='이용자수')+ 
  ggtitle("서울시 따릉이 대여횟수") + theme(plot.title=element_text(face="bold", size=15, hjust=0.5))

