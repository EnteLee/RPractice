library(dplyr)
library(magrittr)
library(readxl)

df <- read_excel("아파트(매매)_실거래가_201801.xlsx", skip = 16) #초기화가 한번 필요함
df <- data.frame()
str(df)
for(i in 1:9){
  file <- paste0("C:/MyRWorkshop/181004/아파트(매매)_실거래가_20180",i,".xlsx")
  print(file)
  data1 <- read_excel(file, skip = 16)
  rbind(df, data1) #paste0 공백 없애주기
}
df
dim(df)




library(ggplot2)
str(cars)
cars
plot(
  cars$speed,
  cars$dist,
  main="Speed/Dist",
  xlab = "mph",
  ylab = "ft",
  pch=1,
  col="red"
)
summary(cars$dist)
data <- c(1,2,3,4,5,3,4,2,2,1)
mean(data)
median(data)

class(Nile)
Nile
plot(Nile)

data <- read.csv("cafedata.csv", header = T, na.strings = "na", stringsAsFactors = F)
data
str(data)
library(ggplot2)
View(data)
g <- ggplot(data, aes(data$Coffees))
g2 <- g + geom_bar(fill="gray")
g3 <- g2 + xlim(0,50) + ylab("판매량")
g3
data <- na.omit(data)
#최소값/최대값
data$Coffees
sort(data$Coffees)[1]
sort(data$Coffees, decreasing = T)[1]
min(data$Coffees)
max(data$Coffees)
stem(data$Coffees)
table(cut(data$Coffees,
          breaks = seq(0,50,by=10), right=F ))
stem(data2)
table(cut(data$Coffees))



height <- c(164,166,168,170,172,174,176)
height.m <- mean(height)
height.v <- var(height)
height.s <- sd(height)
height.m
height.v
height.s 

x<-seq(158,182,by=0.01)
x
qs <- quantile(rc)
qs
par(mar=c(2,2,2,2))
bp <- boxplot(rc)




install.packages("varhandle")
library(varhandle)

df1 <- read.csv("example_studentlist.csv")
df1
summary_df <- summary(df1$height)
var_df <- var(df1$height)
sd_df <- sd(df1$height)
df2 <- df1 %>% mutate(summary_df = summary(df1$height),
                      var_df = var(df1$height),
                      sd_df = sd(df1$height))
df2

#ques4
data2 <- read.csv("example_salary.csv", na.strings = "-")
colnames(data2)
colnames(data2) <-
  c('age', 'salaryMonth', 'salaryYearSpecial',
    'timeWork', 'numberOfCompany', 'experiences', 'gender')
data2
temp1 <- median(data2$salaryMonth, na.rm=TRUE)
temp2 <- median(data2$salaryYearSpecial, na.rm=TRUE)
temp3 <- median(data2$timework, na.rm=TRUE)
temp4 <- median(data2$numberOfCompany, na.rm=TRUE)



