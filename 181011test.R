library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
library(magrittr)

#남여 몸무게에 대한 boxplot을 출력하시오.
women_weight <- 
  c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <-
  c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)

boxplot(women_weight,
        men_weight, names = c("여자", 
                              "남자"))

#A1.
data <- data.frame(
  group=rep(c('Women','Man'), each=9),
  weight=c(women_weight, men_weight)
)
data
ggboxplot(data, x="group", y="weight",
          color="group", palette = c("#00AFCC", "#E7B0CC"),
          ylab = "Weight", xlab = "Groups")

#var.test를 적용하고, 확인하시오.
var.test(women_weight,
         men_weight)

#unpaired two-samples t- test를 적용하고, 확인하시오.
t.test(women_weight,
       men_weight,
       paired=TRUE, alternative = "less")


#상관분석
install.packages("corrplot")
install.packages("RColorBrewer")
library(corrplot)
library(RColorBrewer)

data3 <- cor(mtcars)
corrplot(data3, type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"))

# -1<...<0    0<...<1
# rquery.cormat()
dim(mtcars)
source("rquery_cormat.r")
data4 <- mtcars[,c(1,3,4,5,6,7)]
require("corrplot")
rquery.cormat(data4)


# ANOVA 테스트 가설
# Null hypothesis
# -> 다른 그룹의 평균과 같음.
# Alternative hypothesis
# -> 하나 이상의 표본이 다른 평균과 동일하지 않음.
library(ggpubr)
library(magrittr)
library(dplyr)
PlantGrowth
str(PlantGrowth)
names(PlantGrowth)
levels(PlantGrowth$group)
data4 <- PlantGrowth


data4 %>% group_by(group) %>% summarise(count = n(),
                                        mean = mean(weight, na.rm=T),
                                        sd = sd(weight, na.rm =T))
library(ggpubr)
ggboxplot(data4, x="group", y="weight", color="group",
          palette = c("#00AFBB","#E7BBAF","#FC4E07"))
ggline(data4, x="group", y="weight", add = c("mean_se","jitter"))
boxplot(weight ~ group, data=data4,col=c("#00AFBB","#E7BBAF","#FC4E07"))

data5 <- aov(weight ~ group, data=data4)
summary.aov(data5)

# 정수기 as 기사는 몇명이 적절할까
# 1열 : 총 정수기 대여 대수(월)
# 2열 : 10년 이상 노후 정수기 대여 수
# 3열 : AS 시간 (당월)

data <- read.csv("example_data01.csv")
data
str(data)
par(mfrow = c(1,2))
plot(data$purifier, data$as_time)
plot(data$old_purifier, data$as_time)
cor(data$purifier, data$as_time) # 0이면 연관x, 1or-1이면 연관있음
cor(data$old_purifier, data$as_time)

# speed: 차 속도, dist : 제동거리
lm_result <- lm(formula= dist~speed, data=cars)
summary(lm_result)
# 회기분석
# 어떤 현상을 발생시키는 원인들(독립변순)이 결과(종속변수)에
# 영향을 미치는지를 간략화된 "회귀방정식"을 통해서 분석/예측하는 방법

lm_result
coef(lm_result)
par(mfrow=c(2,2))
plot(lm_result)
plot(cars$speed, cars$dist)
abline(lm_result)

# y=2x+!
x <- c(151,174,160,186,150,179,153)
y <- c(63, 81, 56, 91, 47, 76, 62)
plot(x,y)
cor(x,y)

lm_data <- lm(y~x)
lm_data
summary(lm_data)
abline(lm_data)
h <- data.frame(c(155,172,156,181,151,175,159))
predict(lm_data,h)

p<- ggplot(faithful, aes(waiting,eruptions))
p1 <- p + geom_point() 
p1
cor(faithful$waiting, faithful$eruptions)
lm_data <- lm(faithful$waiting~faithful$eruptions, data=faithful)
lm_data
plot(lm_data)
abline(lm_data)

p<- ggplot(faithful, aes(waiting,eruptions))
p1 <- p + geom_point() + geom_smooth(method="lm") 
