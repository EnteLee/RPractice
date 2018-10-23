library(dplyr)
library(magrittr)

data <- read.csv("example_data.csv")
data
str(data)
class(data)
select(data, Index)
d <- data %>% select(Index, State:Y2008)
head(d,3)

data %>% select(-Index, -State)
data %>% select(-c(Index, State))
select(data, starts_with("Y"))
select(data, ends_with("5"))
select(data, contains("1"))

#starts_with(), ends_with()
#contains(),matches()
#one_of(),num_range(),everthing()
select(data, State, everyrhing())
#rename

colnames(data)
filter(data, Index == "A")
filter(data, Index %in% c("A","C"))
filter(data, !Index %in% c("A","C"))
filter(data, Index %in% c("A","C") & Y2002)
select(data, State)
filter(data, grepl("Ar",State))

summarise(data, mean=mean(Y2015),Y2015_median = median(Y2015),
          mean2=mean(Y2014))
summarise_at(data, vars(Y2005,Y2006), funs(mean, median))
myTop <- function(x, N=5){x %>%  arrange(desc(x)) %>% head(N)}
data %>% group_by(State) %>%  do(myTop(.,N=3))
str(data)
group_by(data, State)




library(dplyr)
library(magrittr)
data <- read.csv("example_data.csv")
data
length(colnames(data))
dim(data)
d1 <- select(data, Index, State)
head(d1,2)
d2 <- select(data, starts_with("I"))
head(d2,2)
dim(d2)
filter(d1, Index=="A")
filter(d1, Index %in% c('A','C'))
filter(d1, grepl("Ar",state))

myFunc <- function(x){x %>% arrange(desc(Index)) %>% head}
data %>% select(Index,State) %>% do(myFunc(.))
data %>%  filter(Index %in% c('A','C','I')) %>%  group_by(Index) %>% do(head(.,2))

library(ggplot2)
str(diamonds)
unique(diamonds$color)

df1 <- data.frame(
  Id = c(1,2,3,4,5),
  w = c('a','b','c','d','e'))
df2 <- data.frame(
  Id <- c(1,7,3,6,8),
  y <- c('t','z','y','l','s'))
colnames(df2) <- c('Id','y')
df1
df2
inner_join(df1, df2, by="Id")
left_join(df1, df2, by="Id")
full_join(df1, df2, by="Id")
semi_join(df1, df2, by="Id")
anti_join(df1, df2, by="Id")



library(varhandle)
distinct(data, Index)
select(data, Index)
distinct(data, Index, State)
distinct(data, Index, State,.keep_all = T)
unique(data$Index)
class(unique(data$Index))





library(magrittr)
t <- 1
f <- function(x, add=1){x+add}
f(t,add=2)
t %>% f()
t %>% f(.)
s <- 3
t %>% f(s,add=.)
t %>% f(1, add=.)
f(f(f(x,1),2),3)
x %>% f(1) %>% f(2) %>% f(3)



#log,users,items
library(dplyr)
log <- data.frame(
  user_id = sample(c(1,2,3),10, TRUE),
  item_id = sample(c(1,2,3),10,TRUE),
  correct = sample(c(0,1),10,TRUE)) %>% as.tbl
users <- data.frame(
  user_id = c(1,2,3,4),
  age = c(21,24,30,21)) %>% as.tbl
items <- data.frame(
  item_id = 1:3,
  item = c("1+1","2+2","3/3")) %>% as.tbl
class(log)
distinct(log, user_id)
left_join(users, log, by="user_id")
inner_join(users, log, by="user_id")
full_join(users, log, by="user_id")
semi_join(users, log, by="user_id")
anti_join(users, log, by="user_id")


library(stringr)
theFiles <- dir("data.", pattern="\\.csv")
for(a in theFiles){
  nameToUse <- str_sub(string - a, start = 12, end =18)
  temp <- read.table(file = file.path("data", a),
                     header = TRUE, sep=",", stringAsFactors = FALSE)
  assign(x= nameToUse, value = temp)
}


#cross tab
library(reshape2)
df <- read.csv("example_data5.csv")
df
melt(df,id.vars=c("id","time"))

# dcast
df2 <- read.csv("example_data3.csv")
df2
dcast(df2, Year ~ SemiYear, value.var="Income")
dcast(df2, SemiYear, value.var="Income")
dacast(df2, Year~Product, fun.aggregate = sum, value.var = "Income")


df3 <- read.csv("example_data4.csv")
x <- colnames(df3[,-1])
#id.vars
#measure.vars
#variable.name
#value.name
melt(df3, id.vars = "ID",
     measure.vars = x,
     variable.name = "Species",
     nalue.name = "Sepal.Length",
     na.rm = TRUE)
