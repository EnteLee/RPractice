install.packages("UsingR")
library(UsingR)
#galton 부모키/자식키
str(galton)
head(galton,2)
par(mfrow=c(1,2))
hist(galton$child)
hist(galton$parent)
cor(galton$child, galton$parent)
options(scipen = 999)
cor.test(galton$child, galton$parent)
xtabs(~child+parent, data=galton)

lm_data <- lm(child ~ parent, data=galton)
summary(lm_data)
plot(child~parent, data=galton)
abline(lm_data, col="red")
library(ggplot2)
g <- ggplot(data=galton, aes(x=parent, y=child))
g1 <- g + geom_smooth(method="lm") 
g2


#1. 안타와 홈런 변수를 활용한 회귀분석을 하시오.
# 안타(H), 홈런(HR)
data1 <- read.csv("example_kbo2015.csv")
data1
hist(data1$H)
hist(data1$HR)
str(data1)
cor(data1$H, data1$HR)
cor.test(data1$H, data1$HR)
lm_data1 <- lm(H ~ HR, data=data1)
summary(lm_data1)
plot(H~HR, data=data1)
abline(lm_data1, col="red")



# 선형회귀
# 종속변수 or 목표변수와 연속변수 or 독립변수와의 관계를 찾는 방법.
# 변수는 연속적이어야 한다. (즉, 숫자값)
# Residual -> 종속변수의 관측된 값과 회귀에서 예측된 종속변수 값의 차이

#단순회귀 (Simpler Linear)
#다항(Polynomial)
#다중(Multiple linear)
# 다수준, 다변량...로지스틱....
# y= ax+b
# weight = a * height + b

#다항회귀
height <- c(58,59,60,61,62,63)
weight <- c(115,117,120,123,126,129)
data2 <- data.frame(height, weight)
data3 <- lm(weight ~ height + (height^2), data=data2)
# weight = a + bx + x^2
plot(weight ~ height, data=data2)



# 다중회귀 (Multiple linear regression)
data5 <- mtcars[,c("mpg","disp","hp","wt")]
data5
model <- lm(mpg~disp+hp+wt, data=data5)
model
# coef 회귀계수
# (intercept)  weight
#  -10          3
coef(model)
coef(model)[1]
coef(model)[2]
# y = 37 -0.000937*disp - 0.031157*hp ...
# disp, hp, wt
# coef() : 회귀계수
# fitted() : 적합값
fitted(model)[1:4]
# residuals() : 오차(error)
residuals(model)[1:4]
# confint() : 회귀례수의 신뢰구간
confint(model)







# 피어슨 상관계수
# 선형적 상관관계 있는지 확인시 사용. [-1 ~ 1]
cov(1:10, 2:11)
cov(1:5, 2:6)
cov(1:5, c(4,4,4,4,4))

# 스피어만 상관계수
# 두 데이터의 실제 값의 순위를 사용해 상관계수를 계산하는 방식
s <- c(2,3,4,3,2,1,5)
rank(sort(s))
x <- matrix(c(1:10, (1:10)^2, ncol=2))
cor(x, method="spearman")
cor(x, method="pearson")



# multiple Regression
install.packages("car")
install.packages("visreg")
install.packages("rgl")
install.packages("scatterplot3d")
library(corrplot)
library(car)
library(visreg)
library(rgl)
library(knitr)
library(scatterplot3d)

?Prestige
head(Prestige, 5)
str(Prestige)
summary(Prestige)
colnames(Prestige)
data <- Prestige[, c(1:4)]
data
plot(data)
plot(data, pch=16)
# Income = B0 + B1 * Education
#         +B2 * Prestige
#         +B3 * Women

# educ <- scale(data$education, center = T, scale =F)
data <- Prestige[, -6]
cor(data)
corrplot(cor(Prestige[,-6]), method="number")
Prestige[,5]
p <- ggplot(data=Prestige, aes(x=prestige, y=income, col=type))
p1 <- p + geom_point()
p1

# education*prestige
# women*prestige
# consus&prestige


# 홈런과 다른 변수간의 상관계수를 살펴보시오.
# 회귀모델을 구하고 p-value값을 확인하시오.
# 잔차(residuals) 관련 그래프를 출력하시오.

df <- read.csv("example_kbo2015.csv", stringsAsFactors = F, na = "-")
df
str(df)


# 1. diamonds 데이터로 캐럿에 따른 가격을 예측하시오
#  - 회귀 모델을 구하고, 검증하시오.
#  - 10캐럿, 20캐럿

diamonds

lm_data <- lm(carat~price, data=diamonds)
lm_data
# y= 0.3672 + 0.0001x
summary(lm_data)
plot(lm_data)


#A
d <- lm(diamonds$price ~ diamonds$carat, data = diamonds)
summary(d)
d2 <- c(10,20)
predict(d, data.frame(carat=c(10,20)))
colnames(diamonds)
