# 주사위 (1,2,3,4,5,6) 4번 던져서 나오는 숫자의 합 x에 대한 확률
library(prob)
rolldie(5)
S <- rolldie(4)
dim(S)
str(S)
x <- apply(S, 1, sum)
x
x.freq <- table(x)
length(x.freq)
x.freq <- x.freq / length(x)
x.freq
plot(x.freq, type="h")

# temp <- matrix(c(1,2,3,4), nrow=2)
# temp
# apply(temp,1,sum)
# 이산확률변수 -> 이산확률분포

# 50개 제품중 8개가 불량이 있는 상자로부터
# 10개의 제품을 랜덤 샘플링시 발견되는 불량 개수 x의 확률분포는?

npop <- 50
nsamp1 <- 10
ndef <- 8
d <- choose(npop,nsamp1)
freq <- choose(ndef, 0:nsamp1) * choose(npop-ndef, nsamp1-(0:nsamp1))
freq
fx <- freq /d
fx
plot(0:10,fx,type="h")


# 여덟명이 각각의 모자를 들고 모였는데, 갑자기 정전이 되서 아무 모자나 들고 집으로 감
# 자기 자신의 모자를 들고 간 신사의 수를 x라고 할때, 확률변수 x의 확률은?
options(stringsAsFactors = F)
hats <- LETTERS[1:8]
S <- urnsamples(hats, size=8, ordered = T)
str(S)
dim(S)
rowN <- nrow(S)
ncol(S)
table(S)
checkFunc <- function(x){sum(x==hats)}
X <- apply(S,1,checkFunc)
X.freq2 <- table(X)
X.prob2 <- round(X.freq2/rowN,6)
sum(X.prob2)
plot(X.prob2, type="h")


#주사위 3개를 던짐. 짝수의 개수
S <- rolldie(3)
rowN <- nrow(S)
S
rowN
checkFunc <- function(x) sum(1-x%%2)
Y <- apply(S,1,checkFunc)
table(Y)

# 주사위를 두번 던지는 시행.
# 눈의 최대치 X, 눈의 최소치 Y
# Z=XY 의 기대값은?

S1 <- rolldie(2)
str(S1)
X1 <- apply(S1, 1, max)
table(X1)
X2 <- apply(S1, 1, min)
table(X2)
temp <- table(X1,X2)/nrow(S1)
temp
class(temp)
XY <- (1:6 %o% 1:6)
XY
(as.vector(XY)) %*% as.vector(XY)

S3 <- tosscoin(4)
S3
nrow(S3)
#앞면 뒷면 갯수를 세는 함수
countH <- function(x) sum(x=='H')
countT <- function(x) sum(x=='T')
# 확률 변수 변수 생성
X3 <- apply(S3, 1, countH)
Y3 <- apply(S3, 1, countT)
V3 <- Y3 -X3
W3 <- abs(V3)
par(mfrow=c(2,3))
plot(X3,Y3)
plot(X3,V3)
plot(X3,W3)
plot(Y3,W3)
plot(V3,W3)



# 평균, 중앙값(mean, median)
# 주사위 4개 던졌을때
# list <- 합, 평균, 최대치, 최소치

S5 <- rolldie(4)
N5 <- nrow(S5)
X5_sum <- apply(S5, 1, sum)
X5_mean <- apply(S5, 1, mean)


# 불량률이 0.03인 공정에서 20개의 표본을 추출하여 검사하고 발견한
# 불량 개수를 X 라 할때, X의 확률분포 함수, 2개의 불량률이 발견될 확률.
dbinom(0:2, 20, 0.03)



# 정규분포와 관련 분포
# 1. 기대값 중심으로 대칭이며, 중심위치는 엎어놓은 종 모양의 분포
# dnorm(), pnorm(), qnorm(), rnorm()
# 표준정규분표 기대값 = 0,  표준편차 =1
# x축 -7 ~ 7
x <- (-140:140)/20
dnorm(x, 0, 1)
dnorm(x, 0, 2)
dnorm(x, 2, 1)
dnorm(x, 2, 2)
data <- matrix(c(dnorm(x, 0, 1), dnorm(x, 0, 2), dnorm(x, 2, 1), dnorm(x, 2, 2)),
               ncol=4,
               byrow = F)
data
par(mfrow=c(2,2))
plot(x, data[,1], type="l")
plot(x, data[,2], type="l")
plot(x, data[,3], type="l")
plot(x, data[,4], type="l")
segments(0,0,0,max(data[,1]), lty=2,col=4)

# 확률변수 x가 N(2,2^2)을 따를때, 구간 P(-1<x<4)를 구하시오.
x3 <- (-140:140)/70
x3
mu3 <- 2; sig3 <- 2; a3<- -1; b3<- 4
fx3 <- matrix(c(dnorm(x3,0,1),
                dnorm(x3,mu3,sig3)),
              ncol=2, byrow=F)
fx3
px3 <- pnorm(4, mu3, sig3); px3
px4 <- pnorm(-1, mu3, sig3); px4
px3 - px4

plot(x3, fx3[,2], type='l')
x5 <- c(a3, seq(a3, b3, 0.01),b3)



# 1. 성공확률이 0.2인 n회의 시행에서 나온 성공횟수를 Xn이라 할때, n=10,20,50 
# 각각에 대해 Xn의 확률 분포함수를 그래프로 출력하라. (dbinom)
x10 <- 0:10; x20 <- 0:20; x50 <- 0:50
par(mfrow=c(1,3))
plot(x10, dbinom(x10,10,0.2), type="h")
plot(x20, dbinom(x20,20,0.2), type="h")
plot(x50, dbinom(x50,50,0.2), type="h")


# 2. 성공확률이 20%이고 200개의 단위로 구성된 모집단에서 비복원추출한 n개의
# 표본에서 나온 성공회수를 Xn이라고 할때, n=10,20,50 각각에 대해 Xn의
# 확률분포함수를 그래프로 출력하시오.(dhyper)

x10 <- 0:10; x20 <- 0:20; x50 <- 0:50
plot(x10, dhyper(x10, 40, 160, 10), type="h")
plot(x20, dhyper(x20, 40, 160, 20), type="h")
plot(x50, dhyper(x50, 40, 160, 50), type="h")

# 3.단위 제품 당 평균 결점수가 3개인 n개의 단위 제품에서 나온 결점수를 Xn이라고
# 할때, n=2,5,10 각각에 대해 Xn의 확률 분포 함수를 그래프로 출력하라.(dpois)
x2 <- 0:12; x5 <- 0:30; x10 <- 0:60
par(mfrow=c(1,3))
plot(x2, dpois(x2, 6), type="h")
plot(x5, dpois(x5, 15), type="h")






#중심 극한정리(central limit theorem)
library(ggplot2)
install.packages("devtools")
install.packages("kassambara/easyGgplot2")
library(devtools)
library(easyGgplot2)
library(data.table)
data <- data.table(read.table("ME_2765tmax.txt", header=F))
str(data)
class(data)
colnames(data) <- c("StateID","YearDay","Year","Month","MonthDay","MaxTemp")
colnames(data)
.
