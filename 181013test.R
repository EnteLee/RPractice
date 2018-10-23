# Probability(확률)
# 1. 어떤 실험을 통해서 나오는 결과를 알지 못하는 경우
# 2. 결과를 알지 못하지만, 결과로 나타날 수 있는 가능성이 있는 경우
# 3. 동일한 실험을 몇번이고, 반복할 수 있음
# 표본공간, 사건
# 사건 - 합사건, 곱사건, 여사건, 배반사건, 독립사건

# Ex) 동전던지기 (동전을 2번 던져 나오는 면을 확인)
# 확률실험, 표본공간, 사건
# 표본공간 : HH, HT, TH, TT
# 사건 : 첫 번째 동전이 앞면이 나올 확률 (HH, HT)

# EX) 인터넷 사용시간 (하루 중 인터넷 사용 시간을 관찰)
# 표본공간 : 0 <- t <- 24
# 사건 : 사용시간이 1시간 이하인 사건 (0 <- t<1)

# N번 반복
# A 사건 발생 : n
# A 사건 발생 확률 : P(A) = n/N

library(prob)
tosscoin(2)
rolldie(1)
urnsamples(1:3, size=2) #비복원추출
urnsamples(1:3, size=2, replace = T) #복원추출

# 문자 R 3개, 문자 B 2개 구성.
urnsamples(c(rep("R",3),rep("B",2)),size =2)

# 동전을 두번 던졌을때 나올 확률
tosscoin(2, makespace = T)
tosscoin(3, makespace = T)

# 동전을 두번 던졌을 때 앞면이 나오는 횟수
tosscoin(2)
# HH 2 
# TH 1
# HT 1
# TT 0 (H 몇번 나왔는가)

x <- c(0,1,2)
px <- c(1/4, 2/4, 1,4)
Ex <- sum(x*px)
Ex


# 동전을 25번 던졌을 때 19번이 나올 확률은?
options(scipen = 999)
probability_value <- .5
wins <- 19
flips <- 25
dbinom(wins, flips, probability_value)
dbinom(1:18, flips, probability_value)
pbinom(wins-1, flips, probability_value)
1-pbinom(wins-1, flips, probability_value)

# 0:25
library(ggplot2)
wins <- c(0:25)
totalFlips <- 25
probability_value <- .5

x1 <- 1-pbinom(wins-1, flips, probability_value)
x2 <- pbinom(wins-1, flips, probability_value)
data <- data.frame(wins = x1, prob_value=x2)
data

# ggplot
library(ggplot2)
ggplot(data, aes(x=x1, y=))


# 확률변수 x가 시행의 횟수가 6이고, 확률이 1/3 인 이항분포를 따를때
n <- 6
p <- 1/3
x <- 0:n
# dbinom(x, size, prob)
# x : 이항분포의 성공 횟수의 벡터
# size : 시행의 횟수
# prob : 성공의 확률
dbinom(x, size=6, prob=p)
dbinom(4, size=n, prob=p)
px <- dbinom(x, size=6, prob=p)
plot(x, px, type = "s",
     xlab = "성공횟수(x)",
     ylab ="확률(P(X=x))",
     main="B(6,1/3)")

# 이항분포 누적함수
# pbinom(x, size, prob)
# x: 이항분포의 성공횟수
# size : 시행의 횟수
# prob : 성공의 확률

pbinom(2, size=n, prob=p)
pbinom(4, size=n, prob=p)


# 동전 1000번 던져 490번 앞면이 나올 확률은?
options(scipen = 999)
probability_value <- .5
wins <- 490
flips <- 1000
dbinom(wins, flips, probability_value)

dbinom(490, size= 1000, prob=0.5)

# 흡연률이 25%로 알려진 1020명이 있는 대학에서 무작위로 50명을 뽑았을때 흡연자일 확률은?
dbinom(50, size=1020, prob =0.25)

library(prob)
library(corrplot)
s <- rolldie(4)
s
N <- nrow(s)
N
item <- c("합", "범위", "최대치", "최소치")
Mt <- paste("주사위 4개의 눈", item, "확률분포")
X <- vector("list", 4)
X[[1]] <- apply(s,1,sum)
X[[3]] <- apply(s,1,max)
X[[4]] <- apply(s,1,min)
X[[2]] <- X[[3]]-X[[4]]
corrplot(X, Mt, item)



# 성공확률이 각각 0.2, 0.5, 0.8인 무한모집단에 10개씩 표본을 취하였을때
# 나타나는 성공 횟수의 확률분포표를 그리시오.(plot)
n <- 10
p <- c(0.2,0.5,0.8)
x <- 1:n
