install.packages("foreign")
install.packages("ggplot2")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
library(magrittr)

data <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T) 
str(data)
colnames(data)

data2 <- data
dim(data2)

# #h10_g3 : sex(성별)
# birth = h10_g4(태어난 연도)
# marriage = h10_g10(혼인 상태)
# religion = h10_g11(종료)
# income = p1002_8aq1(월급)
# code_job = h10_eco9(직종코드)
# code_region = h10_reg7(지역코드)

data2 <- rename(data2, sex = h10_g3, birth = h10_g4, marriage = h10_g10,
                religion = h10_g11, income = p1002_8aq1, code_job = h10_eco9,
                code_region = h10_reg7)

colnames(data2)
#성별에 따른 월급의 차이
data2$sex
class(data2$sex)
head(data2$sex)
table(data2$sex)
summary(data2$sex)
#ifelse(조건, 조건만족시 변경 9를 na로, 불만족시 그대로)
data2$sex <- ifelse(data2$sex == 9, NA, data2$sex) 
data2$sex <- ifelse(data2$sex == 1, "male", "female")

#NA값 있는지 체크
is.na(data2$sex)
table(is.na(data2$sex))

qplot(data2$sex)



data2$income
summary(data2$income)
class(data2$income)
table(data2$income)
# + 는 옵션추가의 의미
qplot(data2$income) + xlim(0,1000)

data2$income <- ifelse(data2$income %in% c(0,9999), 0, data2$income)
head(data2$income,1000)
is.na(data2$income)
table(is.na(data2$income))


#성별 월급 평균표
sex_income<- data2 %>% filter(income != 'NA') %>% group_by(sex) %>% summarise(income_mean = mean(income))
ggplot(data=sex_income, aes(x=sex, y=income_mean))+geom_col()

#나이 월급 관계 그래프
data2$age <- 2018 - data2$birth +1
data2$birth
summary(data2$age)
qplot(data2$age)
age_income <- data2 %>%  filter(income != 'NA') %>% group_by(age) %>% summarise(income_mean = mean(income))
age_income
ggplot(data=age_income, aes(x=age, y=income_mean))+geom_line()

data2 <- data2 %>%  mutate(age1 = ifelse(age<30, 'young', ifelse(age <=50, 'middle', 'old')))
data2$age1
table(data2$age1)
qplot(data2$age1)

age_income1 <- data2 %>%  filter(income != 'NA') %>% group_by(age1) %>% summarise(age_mean = mean(income))
ggplot(data=age_income1, aes(x=age1, y=age_mean))+geom_col()

age_income2 <- data2 %>%  filter(income != 'NA') %>% group_by(age1, sex) %>% summarise(age_mean2 = mean(income))
ggplot(data=age_income2, aes(x=age1, y=age_mean2, fill = sex))+geom_col(position="dodge")


#code_job, job -> data.frame

#남성 직업 빈도 상위 10개 직업
data2$sex <- ifelse(data2$sex == 1, "male", "female")
job_male <- data2 %>%  filter(!is.na(job)&sex == "male") %>% group_by(job) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% head(10)
g <- ggplot(data = job_male, aes(x=reorder(job,n), y=n))

head(data2$religion,3)
range(data2$religion) # 1(yes), 2(no)
range(data2$marriage) # 1(marriage), 3(divorse)

#종교 유무에 따른 이혼율

data2$religion <- ifelse(data2$religion == 1, "Yes", "No")
data2$marriage <- ifelse(data2$marriage == 1, "Married", ifelse(data2$marriage == 3, "Divorsed", "N"))
data2$marriage
rel_marry <- data2 %>%  filter(marriage != 'N') %>% group_by(religion)
rel_marry

table(is.na(data2$marriage))
rel_marry <- data2 %>%  filter(marriage != 'N') %>%
  group_by(religion, marriage) %>%  summarise(n=n()) %>% mutate(tot = sum(n)) %>% 
  mutate(pct = n/tot * 100)



install.packages("ggiraphExtra")
install.packages("maps")
install.packages("mapproj")
library(ggiraphExtra)
library(dplyr)
library(tibble)
library(ggplot2)

USArrests
str(USArrests)
crime <- rownames_to_column(USArrests, var = "state")
head(crime,3)
crime$state <- tolower(crime$state)
str(crime)

states_map <- map_data("state")
states_map
ggChoropleth(data=crime, aes(fill=Murder, map_id=state),
             map = states_map)




install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")

str(changeCode(korpop1))



#여성 직업 빈도 상위 10개 직업을 출력하시오.
#연령대별 이혼율표를 만드시오.
#연령대 및 종교 유무별 이혼률 표 만들기.
#4. 지역코드(code_region[1 ~ 7]) 목록을 만들고
# code_region		region
# 1				서울
# 2				수도권(인천/경기)
# 3				부산/경남/울산
# 4				대구/경북
# 5				대전/충남
# 6				강원/충북
# 7 				광주/전남/제주도 
# 위의 지역명을 추가하고, 지역별 연령대 비율표를 만드시오.


data <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T) 
data2 <- rename(data2, sex = h10_g3, birth = h10_g4, marriage = h10_g10,
                religion = h10_g11, income = p1002_8aq1, code_job = h10_eco9,
                code_region = h10_reg7)
data2 <- data
data2$sex <- ifelse(data2$sex == 1, "male", "female")

data2$code_job
data2$job

data4 <- data3 %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job)
data5 <- data3 %>% 
  filter(!is.na(code_job) 
         & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(job_mean = mean(income)) 

job_female <- data2 %>%  filter(!is.na(job)&sex == "female") %>% group_by(job) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% head(10)
g <- ggplot(data = job_male, aes(x=reorder(job,n), y=n))