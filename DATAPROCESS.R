#install the packages
install.packages("readr", "dplyr")
install.packages('caret', 'ROCR')

#install the library
library("readr")
library("dplyr")
library("ggplot2")
library(tidyverse)
library(caret)
library(ROCR)
library(pROC)

#Setting the directory
setwd("C:/Users/Taegun/Desktop/NICE DNB 데이터 챌린지/챌린지 데이터")

#txt파일 csv로 변환후 저장하기
finance = read.table("재무데이터.txt", header=TRUE, fileEncoding = "euc-kr", sep="\t", fill=TRUE)
head(finance)
ncol(finance)
write.csv(finance,"finance.csv", fileEncoding = "cp949")

#변환한 csv 파일 다시 불러오기
finance = read.csv("finance.csv", header=TRUE, fileEncoding = "euc-kr")

#재무데이터의 경우 n년치 데이터가 들어가 있기 때문에 n년치 평균을 사용하여 계산할 예정임
##26번째 col인 매출액 순이익률까지만 사용, 그 이후는 결측치가 너무 많음 
head(finance)
finance_1 <- finance[,1:26]
head(finance_1)

code_count = count(finance_1, 사업자번호)
head(code_count)

nrow(code_count)

finance_2 = aggregate(finance_1, by=list(finance_1$사업자번호),FUN=sum)
head(finance_2)
nrow(finance_2)


names(finance_2)[1] <- c("사업자번호")
head(finance_2)

finance_2 <- finance_2[,-c(2,3)]
head(finance_2)

averaged = finance_2[,c(2:25)]

head(averaged)

averaged_div = averaged/code_count$n
head(averaged_div)

finance_head = finance_2[,1]
head(finance_head)

finance_3 <- cbind(finance_head, averaged_div)
head(finance_3)

names(finance_3)[1] <- c("사업자번호")
head(finance_3)

ncol(finance_2)
#finance_3 <- 25개 col을 가진 4개년 평균 데이터로 변환 완료

#export
write.csv(finance_3,"finance_3.csv", fileEncoding = "cp949")

#재무데이터를 폐업/액티브기업으로 나누기
active = read.csv("Active.csv", header=TRUE)
closed = read.csv("closed.csv",header=TRUE)

active$intersect = ifelse(active$BIZ_NO %in% finance_3$사업자번호, 'yes','no')
active_1 <- active[!(active$intersect =='no'),]

finance_3$intersect = ifelse(finance_3$사업자번호 %in% active_1$BIZ_NO, 'yes', 'no')
head(finance_3)

#finance$intersect, yes = active /  no=closed
##폐업을 예측하기 위해서, active를 0, closed 를 1로 설정함

finance_3 <- finance_3 %>%
  mutate(newcol=ifelse(finance_3$intersect=='no', 1,0))

finance_3 <- subset(finance_3,select=-c(intersect))

head(finance_3)

#newcol이 폐업여부, 0은 액티브, 1은 폐업
write.csv(finance_3,"finance_4.csv", fileEncoding = "cp949")





#xgboost, logistic 분석을 위한 영업일수, 근로자수, 산업코드 앞자리만 추출하기
##날짜 (일수) 산출하기
#active 기업
active_date <- active[,c('BIZ_NO', 'ESTB_DATE')]
head(active_date)

converted_date <- as.Date(as.character(active_date$ESTB_DATE),format = "%Y%m%d")
head(converted_date)
active_date <- cbind(active_date, converted_date) 
head(active_date)

d1=as.Date("2022-11-01")
active_date$converted_date <- d1 - active_date$converted_date 
head(active_date)
nrow(active_date)
#198651개 


#closed 기업
##closed 에는 establish date가 있고, 휴폐업이력에서 STAT_OCR_DATE가 폐업 발생일자임
head(closed)
closed_history = read.csv("휴폐업이력.csv",header=TRUE)
head(closed_history)


closed_date <- closed[,c('BIZ_NO', 'ESTB_DATE')]

head(closed_date)
closed_date <- merge(closed_date, closed_history, by='BIZ_NO')


head(closed_date)
closed_date <- closed_date[,-c(3,4,5)]

head(closed_date)
est_date <- as.Date(as.character(closed_date$ESTB_DATE),format = "%Y%m%d")

closed_date <- cbind(closed_date, est_date)
head(closed_date)

closed_date <- closed_date[,c(-4)]
head(closed_date)

closed_date1 <- as.Date(as.character(closed_date$STAT_OCR_DATE),format="%Y%m%d")
closed_date <- cbind(closed_date, closed_date1)
head(closed_date)

closed_date <- closed_date[,c(-2,-3)]
head(closed_date)
operation_date <- closed_date1 - est_date 
head(operation_date)

closed_date1 <- cbind(closed_date,operation_date)
head(closed_date1)

closed_date <- closed_date1
head(closed_date)

nrow(closed_date)
#6832개, 다시 확인하기

##근로자수 산출하기(컬럼은 EMP_CNT를 사용한다.)
head(active)
active_emp <- active[,c("BIZ_NO", "EMP_CNT")]
head(active_emp)
nrow(active_emp)
#198651

head(closed)
closed_emp <- closed[,c("BIZ_NO", "EMP_CNT")]
head(closed_emp)
nrow(closed_emp)
#6739


#일수, 근로자수 합치기
##액티브 기업 날짜는 active_date, 액티브 기업 인원수는 active_emp
###폐업 기업 날짜는 closed_date, 폐업기업 인원수는 closed_emp
####inner join 문법 closed_date <- merge(closed_date, closed_history, by='BIZ_NO')

active_merged <- merge(active_date, active_emp, by='BIZ_NO')
head(active_merged)

closed_merged <- merge(closed_date, closed_emp, by = "BIZ_NO")
head(closed_merged)

colnames(closed_merged)[2] <- 'ESTB_DATE'
head(closed_merged)

colnames(active_merged)[3] <- 'operation_date'
head(active_merged)

closed_merged <- closed_merged[,(-3)]

closed_merged[,"new_col"] = 1
head(closed_merged)

active_merged[,'new_col'] = 0
head(active_merged)

closed_merged1 <- closed_merged[,(-2)]
active_merged1 <- active_merged[,(-2)]


merged <-rbind(closed_merged1, active_merged1)
nrow(merged)
na.omit(merged)
nrow(merged)

#export
write.csv(merged,"비재무.csv", fileEncoding = "cp949")

#결측치를 제외한 관측치 수 205483

closed_omit <- na.omit(closed_merged)
nrow(closed_omit)

active_omit <- na.omit(active_merged)
nrow(active_omit)



head(closed_merged)
nrow(closed_merged)
nrow(active_merged)

nrow(merged)


#비 재무적 기준 (Operation_date, EMP_CNT)이 재무적 기준에 영향을 미치는가?
##독립변수를 비 재무적 기준, 종속변수를 재무적 기준들 중 하나로 하는 다중회귀분석 진행하기
### 재무적 기준중 폐업에 가장 큰 영향을 미친 기준들 선정
####기업 순이익률

finance_3_1 <- finance_3[,c('사업자번호', '기업순이익률...')]
head(finance_3_1)

colnames(finance_3_1)[1] <- 'BIZ_NO'
head(merged)

finance_3_1 <- merge(finance_3_1, merged, by = "BIZ_NO")
head(finance_3_1)

reg1 <- lm(formula= 기업순이익률... ~ operation_date + EMP_CNT, data=finance_3_1)
summary(reg1)
#RSQUARE 0.123, 사용할 수 없음

head(finance_3_1)

finance_3_1 <- finance_3_1[,c(-1,-5)]
head(finance_3_1)

finance_3_1$operation_date <- as.numeric(finance_3_1$operation_date)
finance_3_1$기업순이익률... <- as.numeric(finance_3_1$기업순이익률...)
finance_3_1$EMP_CNT <- as.numeric(finance_3_1$EMP_CNT)

cor(finance_3_1)

finance_3_2 <- na.omit(finance_3_1)

cor(finance_3_2)
#상관관계 분석에서도 유의미한 결과 없었음

####자산 총계
finance_3_1 <- finance_3[,c('사업자번호', '자산총계')]
colnames(finance_3_1)[1] <- 'BIZ_NO'
finance_3_1 <- merge(finance_3_1, merged, by = "BIZ_NO")
reg1 <- lm(formula= 자산총계 ~ operation_date + EMP_CNT, data=finance_3_1)
summary(reg1)

#Rsquare 0.004

finance_3_1$operation_date <- as.numeric(finance_3_1$operation_date)
finance_3_1$자산총계 <- as.numeric(finance_3_1$자산총계)
finance_3_1$EMP_CNT <- as.numeric(finance_3_1$EMP_CNT)

na.omit(finance_3_1)
cor(finance_3_1)



### 재무데이터에 산업코드 추가하기
head(active)
active_3 <- active[,c('BIZ_NO', 'IND_CD1')]
head(active_3)
finance_4 <- merge(finance_4, active_3, by="BIZ_NO")
head(finance_4)


finance_4 <- finance_3
colnames(finance_4)[1] <- 'BIZ_NO'
head(finance_4)

write.csv(finance_4,"finance_5.csv", fileEncoding = "cp949")


#기업 근속년수 5년이상과 그 이하로 나누기
##active_merged1와 closed_merged1사용
head(closed_merged1)
head(active_merged1)

closed_merged1$operation_date <- as.numeric(closed_merged1$operation_date)
closed_merged2 <- closed_merged1[closed_merged1$operation_date>1800,]
head(closed_merged2)
nrow(closed_merged2)

#폐업한 기업들중 5년 이상 영업한 기업들 5194개 (closed_merged_long)
##5년 이상 영업했을 경우 long column이 1, 그 이하일 경우 0

closed_merged_long <- closed_merged2
closed_merged_long[,"long"] = 1
head(closed_merged_long)


#폐업한 기업들중 5년 이하 영업한 기업들 
closed_merged_short <- closed_merged1[closed_merged1$operation_date<1800,]
closed_merged_short <- subset(closed_merged1, operation_date<=1800)
head(closed_merged_short)
nrow(closed_merged_short)

##폐업한 기업들중 5년 이하 영업한 기업들 1638개, long col에 0으로 표시

closed_merged_short[,"long"] = 0
head(closed_merged_short)

head(finance_3_1)
finance_3_1$long = ifelse(finance_3_1$operation_date > 1800, 1, 0)
head(finance_3_1)
finance_3_1<- finance_3_1[,c(1,2,3,4,6,5)]


write.csv(finance_3_1,"finance_5years.csv", fileEncoding = "cp949")


##온실가스

gas = read.csv("Gas.csv", header=TRUE, fileEncoding = "euc-kr")
print(gas)

colnames(gas)[1] <- 'IND_CD1'

##
gas_2 <- gas
print(gas_2)
gas_2 = aggregate(gas, by=list(gas$IND_CD1),FUN=sum)

##컬럼별로 데이터 타입 파악
unlist(lapply(sapply(gas_2,FUN="class"), FUN="[",1))

#as.numeric 함수를 넣었을때 에러가 나오는 경우는 원본 csv 파일에서 character형에서 comma가 존재하기 때문이다
##gsub를 사용하여 comma 제거하기

gas_2$Gas <- gsub("," , "", gas_2$Gas)
gas_2$Energy <- gsub("," , "", gas_2$Energy)

gas_2$Gas <- as.numeric(gas_2$Gas)
gas_2$number <- as.numeric(gas_2$number)
gas_2$Energy <- as.numeric(gas_2$Energy)

head(gas_2)

#IND_CD1에 따라 더하기
##전체 데이터셋에 aggregate를 적용하면 IND_CD1열이 문자열이기 때문에 연산이 되지 않음.
### 그럴때는 연산 대상만 cbind(A,B) 처럼 A,B col을 지정해야함
####gas_3은 온실가스와 에너지 사용량 종합한 파일
gas_3 = aggregate(cbind(gas_2$Gas,  gas_2$Energy, gas_2$number), by=list(gas_2$IND_CD1),FUN=sum)
head(gas_3)
colnames(gas_3)[1] <-'IND_CD1'
colnames(gas_3)[2] <-'Gas'
colnames(gas_3)[3] <-'Energy'
colnames(gas_3)[4] <-'number'

ind<- gas_3[,c(1)]
head(ind)
gas_3 <- gas_3[,-c(1)]

gas_avg = gas_3/gas_3$number
head(gas_avg)
gas_4 <- cbind(gas_avg, ind)
head(gas_4)
gas_4<- gas_4[,-c(3)]


rawdata = read_csv("재무_산업코드.csv")
finance_5 <- as.data.frame(rawdata)
head(finance_5)
##원본데이터셋 = finance_5

finance_5_1 <- finance_5
colnames(finance_5_1)[28] <- 'IND_CD1'
head(finance_5_1)


#사업자 번호 삭제

finance_5_1 <- finance_5_1[,-c(1)]

head(finance_5_1)
#사용할 산업코드: A,C,D,E,F,G
##사용할 재무 항목:기업순이익률, 자산총계, 판매비와 관리비, 매출액, 법인세비용, 비유동부채

##A
finance_5_A <- subset(finance_5_1,subset= IND_CD1 == "A")
finance_5_A[is.na(finance_5_A)] <- 0
aggregate(finance_5_A$매출액, by=list(finance_5_A$폐업여부), mean)
##A_매출액 평균 21874855


##C
finance_5_C <- subset(finance_5_1,subset= IND_CD1 == "C")
finance_5_C[is.na(finance_5_C)] <- 0
aggregate(finance_5_C$매출액, by=list(finance_5_C$폐업여부), mean)

##C_매출액 평균 48924784


##D
finance_5_D <- subset(finance_5_1,subset= IND_CD1 == "D")
finance_5_D[is.na(finance_5_D)] <- 0
aggregate(finance_5_D$매출액, by=list(finance_5_D$폐업여부), mean)

##D_매출액 평균 20318886


##E
finance_5_E <- subset(finance_5_1,subset= IND_CD1 == "E")
finance_5_E[is.na(finance_5_E)] <- 0
aggregate(finance_5_E$매출액, by=list(finance_5_E$폐업여부), mean)

##E_매출액 평균 28619843


##F
finance_5_F <- subset(finance_5_1,subset= IND_CD1 == "F")
finance_5_F[is.na(finance_5_F)] <- 0
aggregate(finance_5_F$매출액, by=list(finance_5_F$폐업여부), mean)

##F_매출액 평균 37033450

##G
finance_5_G <- subset(finance_5_1,subset= IND_CD1 == "G")
finance_5_G[is.na(finance_5_G)] <- 0
aggregate(finance_5_G$매출액, by=list(finance_5_G$폐업여부), mean)

##E_매출액 평균 57420637


##H
finance_5_H <- subset(finance_5_1,subset= IND_CD1 == "H")
finance_5_H[is.na(finance_5_H)] <- 0
aggregate(finance_5_H$매출액, by=list(finance_5_H$폐업여부), mean)

##H_매출액 평균 31337504


##I
finance_5_I <- subset(finance_5_1,subset= IND_CD1 == "I")
finance_5_I[is.na(finance_5_I)] <- 0
aggregate(finance_5_I$매출액, by=list(finance_5_I$폐업여부), mean)

##I_매출액 평균 42773896

##J
finance_5_J <- subset(finance_5_1,subset= IND_CD1 == "J")
finance_5_J[is.na(finance_5_J)] <- 0
aggregate(finance_5_J$매출액, by=list(finance_5_J$폐업여부), mean)

##J_매출액 평균 28404298

##K
finance_5_K <- subset(finance_5_1,subset= IND_CD1 == "K")
finance_5_K[is.na(finance_5_K)] <- 0
aggregate(finance_5_K$매출액, by=list(finance_5_K$폐업여부), mean)

##J_매출액 평균 25278518

##L
finance_5_L <- subset(finance_5_1,subset= IND_CD1 == "L")
finance_5_L[is.na(finance_5_L)] <- 0
aggregate(finance_5_L$매출액, by=list(finance_5_L$폐업여부), mean)

##L_매출액 평균 38132676


##O
finance_5_O <- subset(finance_5_1,subset= IND_CD1 == "O")
finance_5_O[is.na(finance_5_O)] <- 0
aggregate(finance_5_O$매출액, by=list(finance_5_O$폐업여부), mean)

##O_매출액 평균 78772994

##P
finance_5_P <- subset(finance_5_1,subset= IND_CD1 == "P")
finance_5_P[is.na(finance_5_P)] <- 0
aggregate(finance_5_P$매출액, by=list(finance_5_P$폐업여부), mean)

##P_매출액 평균 35195586


##Q
finance_5_Q <- subset(finance_5_1,subset= IND_CD1 == "Q")
finance_5_Q[is.na(finance_5_Q)] <- 0
aggregate(finance_5_Q$매출액, by=list(finance_5_Q$폐업여부), mean)

##Q_매출액 평균 44940795


##R
finance_5_R <- subset(finance_5_1,subset= IND_CD1 == "R")
finance_5_R[is.na(finance_5_R)] <- 0
aggregate(finance_5_R$매출액, by=list(finance_5_R$폐업여부), mean)

##P_매출액 평균 16853876


print(gas_4)
asset <- c(21874855, 48924784, 20318886, 28619843, 37033450, 57420637, 31337504,42773896, 28404298, 25278518, 38132676, 78772994, 35195586, 44940795, 16853876)

gas_4_1 <- cbind(gas_4, asset)
print(gas_4_1)

##회귀분석 
reg1 <- lm(formula= asset ~ Gas + Energy, data=gas_4_1)
summary(reg1)

##온실가스 배출량의 산업별평균으로는 의미 있는 분석 결과를 도출 하지 못함


##온실가스 데이터와 이름이 겹치는지 확인하고 데이터 확보하기
co2 = read.csv("co2names.csv", header=TRUE)
head(co2)

co2<- co2[,c(1,2,3)]

head(active)
active_names <- active[,c('BIZ_NO','CMP_NM1')]
head(active_names)

co2$intersect = ifelse(co2$BIZ_NAME %in% active_names$CMP_NM1, 'yes','no')
print(co2)
co2<- co2[,-c(4)]
 
##액티브와 어느정도 겹치는것을 확인함

head(closed)
closed_names <- closed[,c('BIZ_NO','CMP_NM')]

co2$intersect = ifelse(co2$BIZ_NAME %in% closed_names$CMP_NM, 'yes','no')
print(co2)

##폐업데이터도 어느정도 겹침 


##2020년도 데이터
co2020_closed <- co2[!(co2$intersect =='no'),]
head(co2020_closed)

colnames(co2020_closed)[1] <- 'CMP_NM'

co2020_closed <- inner_join(co2020_closed, closed_names, by='CMP_NM')
print(co2020_closed)

co2020_closed<- co2020_closed[-11,]


co2020_active <- co2[!(co2$intersect =='no'),]

colnames(co2020_active)[1] <- 'CMP_NM'
head(co2020_active)

colnames(active_names)[2] <- 'CMP_NM'
co2020_active <- inner_join(co2020_active, active_names, by='CMP_NM')

co2020_closed <- co2020_closed %>%
  mutate(new_col=ifelse(co2020_closed$intersect=='yes', 1,0))
head(co2020_closed)

co2020_active <- co2020_active %>%
  mutate(new_col=ifelse(co2020_active$intersect=='yes', 0,1))
head(co2020_active)
print(co2020_active)

write.csv(co2020_active,"co2020_active.csv", fileEncoding = "cp949")


co2020_active = read.csv("co2020_active.csv", header=TRUE)

nrow(co2020_active)
nrow(co2020_closed)
print(co2020_closed)

##2019년데이터
co2019 = read.csv("co2019.csv", header=TRUE)
head(co2019)
co2019<- co2019[,c(1,2,3)]

co2019$intersect = ifelse(co2019$CMP_NM %in% closed_names$CMP_NM, 'yes','no')
co2019_closed <- co2019[!(co2019$intersect =='no'),]
print(co2019_closed)
head(co2019_closed)

co2019_closed <- inner_join(co2019_closed, closed_names, by='CMP_NM')
co2019_closed <- co2019_closed %>%
  mutate(new_col=ifelse(co2019_closed$intersect=='yes', 1,0))

head(co2020_closed)

co2020_closed <- co2020_closed[,c(1,2,3,5,6)]

head(co2020_active)

co2020<- rbind(co2020_closed, co2020_active)
print(co2020)



co2020 <- co2020[,-c(1)]
co2020 <- co2020[,c(3,1,2,4)]
head(co2020)

finance_5_2020 <- finance_5
colnames(finance_5_2020)[1] <- 'BIZ_NO'

co2020_1 <- inner_join(co2020, finance_5_2020, by='BIZ_NO')
head(co2020_1)

co2020_2 <- subset(co2020_1, select =-c(산업코드...27, 산업코드...28))
head(co2020_2)

co2020_2 <- subset(co2020_2, select =-c(new_col))

unlist(lapply(sapply(co2020_2,FUN="class"), FUN="[",1))

co2020_2$Co2 <- as.numeric(co2020_2$Co2)

write.csv(co2020_2,"gas_2020.csv", fileEncoding = "cp949")

co2020_2 = read.csv("gas_2020.csv", header=TRUE, fileEncoding = "euc-kr")
head(co2020_2)

reg1 <- lm(formula= 기업순이익률 ~Co2 + Energy, data=co2020_2)
summary(reg1)

reg1 <- lm(formula= 자산총계 ~Co2 + Energy, data=co2020_2)
summary(reg1)

reg1 <- lm(formula= 판매비와관리비 ~Co2 + Energy, data=co2020_2)
summary(reg1)

reg1 <- lm(formula= 매출액 ~Co2 + Energy, data=co2020_2)
summary(reg1)

reg1 <- lm(formula= 법인세비용 ~Co2 + Energy, data=co2020_2)
summary(reg1)

reg1 <- lm(formula= 비유동부채 ~Co2 + Energy, data=co2020_2)
summary(reg1)

head(co2020_2)

log1 <- glm(formula = 폐업여부~ Co2 + Energy + 자산총계,family = binomial, data = co2020_2)
summary(log1)

gas_5 = read.csv("Gas.csv", header=TRUE, fileEncoding = "euc-kr")
head(gas_5)
gas_3 = aggregate(cbind(gas_2$Gas, gas_2$Energy, gas_2$number), by=list(gas_2$IND_CD1),FUN=sum)


