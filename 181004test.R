id-root, pwd=1234
host=127.0.0.1, database=employees

install.packages("RMySQL") 
library(RMySQL)

library(RMySQL)
con <- dbConnect(
  MySQL(), 
  user="user 1", 
  password="1234", 
  dbname="employees", 
  host="localhost"
)
dbListTables(con)
df <- dbGetQuery(con,"select * from employees")
df2 <- dbGetQuery(con,"select * from dept_emp")
df3 <- dbGetQuery(con,"select * from departments")
df4 <- dbGetQuery(con,"select * from salaries")
colnames(df)
colnames(df2)
colnames(df3)
colnames(df3)
colnames(df4)
head(df)
head(df2)
str(df)
str(df2)
View(df)
View(df2)
library(magrittr)
library(dplyr)
colnames(df)
df %>% select(emp_no, gender) %>% 
  filter(gender == 'M')
df %>% group_by(gender) %>% 
  summarise(gender_count = n())

ljdf <- left_join(df, df2, by="emp_no")
head(ljdf)
rjdf <- right_join(df, df2, by="emp_no")
head(rjdf)
ijdf <- inner_join(df, df2, by="emp_no")
head(ijdf)
# semi_join 
# anti_join

# purr

theList <- list(
  A= matrix(1:9, 3),
  B= 1:5,
  C= matrix(1:4, 2),
  D= 2)
library(purrr) 
map(theList, sum) 

myFunc <- function(a){return(a+1)} 
map(theList, myFunc)
theList %>% map(myFunc) 
map(theList, function(a){a+1})


library(dplyr)
library(RMySQL) 
library(dplyr)
library(magrittr)
con <- dbConnect(
  MySQL(), 
  user="user 1", 
  password="1234", 
  dbname="employees", 
  host="localhost"
)
dbListTables(con)
df <- dbGetQuery(con,"select * from employees")
df2 <- dbGetQuery(con,"select * from dept_emp")
df3 <- dbGetQuery(con,"select * from departments")
df4 <- dbGetQuery(con,"select * from salaries")
head(df)
head(df2)
head(df3)
head(df4)
ljdf <- left_join(df, df2, by="emp_no")
head(ljdf)
filter(ljdf, dept_no == "d005")
ljdf2 <- left_join(df, df4, by = "emp_no")
head(ljdf2)
ljdf2 %>% arrange(desc(salary))
tail(ljdf2, n=5)

#1.# dplyr
# 1. 부서중 development부서의 인원을 출력하시오.
# 2. employees 부서별 인원을 출력하시오.
# 3. employees중에서 월급이 가능 높은 상위 5명을 출력하시오.
# 4. 부서별로 인원을 출력하고 가장 인원이 많은 부서를 출력하시오.

#1,2
df3 %>%  left_join(df2, by="dept_no") %>% left_join(df, by="emp_no") %>%
  group_by(dept_name) %>% summarise(count=n()) %>% arrange(desc(count)) %>%
  filter(dept_name == 'Development')

#3
colnames(df4)
df %>% left_join(df4, by="emp_no") %>% select(emp_no, salary)
df %>% left_join(df4, by="emp_no") %>% group_by(emp_no) %>% summarise(salary_mean = mean(salary)) %>% arrange(desc(salary_mean)) %>% head(5)



library(stringr)
data <- c("i", "am", "a", "data scientist")
str_length(data)
length(data)
nchar(data)
data2 <- "I am a data scientist"
str_sub(data2, 1, 4)
str_sub(data2, 6, 8)
substr(data2, 6, 8)

paste(c("i am a","data scientist"))
paste0(c("I am a","data scientist",""))

data3 <- "i am a data scientist"
str_split(data3, " ", 4)

data4 <- "서울특별시 강남구 1111 2222"
str_split(data4, " ", 4)

data5 <- "010-1234-5678"
str_split(data5, "-", 3)
str_split_fixed(data5, "-", 3)
data5 <- str_split_fixed(data5, "-", 3)
data5[1,2]

data6 <- c("서울특별시 강남구 1111 222")
data6 <- str_split_fixed(data6," ", 4)
data6[1, 3]



data7 <- c('apple','banana','straberry')
str_detect(data7, 'n')
data8 <- c('apple','b1nana','straberry')
str_detect(data8, '^[a-z]{2}')
str_extract(data8,'^[a-z]{2}')
grep("\\d", data8, value=TRUE)

phones <- c("010 2879 0201","032-1234-4213","031 341 3312", "02 3012 3211", "010 1111 2223")
phonePattern <- "([0-9]{3})[-.]([0-9]{4})[-.]([0-9]{4})"
str_extract(phones, phonePattern)

str_replace(data7, "[ae]","-")
str_replace_all(data7, "[ae]", "-")



for(i in 1:9){
  dfi <- read.csv("아파트(매매)_실거래가_20180"i".xlsx")}

# df1 <- read.csv("아파트(매매)_실거래가_201801.xlsx")
# df1 <- df1[-c(1:16),]
# df1  다틀림



