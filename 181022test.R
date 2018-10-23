# RMSE(Root Mean Square Error)
# MAE(MEan Absoult Error)

install.packages("caret")
install.packages("psych")
rm(list=ls())
library(caret)
library(psych)
head(sat.act,1)
df <- sat.act
#521
df_ctrl <- trainControl(method="cv",number=5)
# ACT ~ gender + age +SATV+SATQ
names(df)
df_model <- train(ACT~ gender + age+ SATV + SATQ,
                  data = df,
                  trControl = df_ctrl,
                  method = "lm",
                  na.action = na.pass)
df_model
df_model$finalModel
df_model$resample
sd(df_model$resample$Rsquared)

lm_data <- lm(ACT~ gender + age+ SATV + SATQ,
              data = df)
options(scipen= 999)
summary(lm_data)
set.seed(0123)
half_size <- floor(0.50 * nrow(df))
random_sample <-
  sample(seq_len(nrow(df)),size = half_size)
random_sample

first_half_data <- df[random_sample,]
lm_data2 <- lm(ACT~ gender + age+ SATV + SATQ, data = first_half_data)
summary(lm_data2)

library(tidyverse)
library(caret)




# recommendation System
# User-Based Collaborative Filtering(UBCF)
# Item-Based Collaborative Filtering(IBCF)



#install.packages("googlesheets")
#install.packages("DT")
#install.packages("pander")
install.packages("recommenderlab")
rm(list=ls())
knitr::opts_chunk$set(message = FALSE, echo = TRUE)
suppressWarnings(suppressMessages(library(googlesheets)))
library(RCurl)
library(tidyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(DT)
library(pander)
library(Matrix)
suppressWarnings(suppressMessages(library(recommenderlab)))
data(MovieLense, package = "recommenderlab")
movielense <- MovieLense
moviemeta <- MovieLenseMeta
pander(head(moviemeta), caption = "Movie Meta Data")
View(movielense)
View(moviemeta)
class(movielense)
nrow(movielense)
ncol(movielense)
nrow(moviemeta)
ncol(moviemeta)
pander(head(moviemeta), caption = "Movie Meta Data")

df <- movielense
df_m <- df[rowCounts(movielense) > 20, colCounts(movielense)>50]
df_min <- min(rowCounts(df_m))
nrow(df_m)
ncol(df_m)

set.seed(101)
train.data <- 
  s <- sample(x=c(TRUE, FALSE), size=nrow(df_m),         replace = T,
              prob=c(0.8, 0.2))
str(s)
train.data2 <- df_m[s,]
test.data2 <- df_m[!s,]
train.data2
test.data2
# IBCF
myModel <- Recommender(
  data=train.data2, method="IBCF",
  parameter=list(k=25, method="Cosine")
)
num <- 10
myResult <- predict(object=myModel,
                    newdata=test.data2, n=num)
myResult

myResult2 <- data.frame(user=sort(rep(1:length(myResult@items))),
                        rating=unlist(myResult@ratings),
                        index=unlist(myResult@items))
myResult2

myResult2$title <- 
  myResult@itemLabels[myResult2$index]
myResult2$year <-
  moviemeta$year[myResult2$index]

myResult3 <- myResult2 %>% group_by(user) %>% 
  top_n(5, rating)
datatable(myResult3[myResult3$user %in% (1:10),])





rt$open() 
rt$navigate("http://ksp.credu.com/ksp/servlet/controller.gate.common.GateConstServlet?p_grcode=000002&p_ssochk=N&p_gubun=&p_ifsubj=&p_ifyear=&p_ifsubjseq=&p_ifdistcode=") 

html <- rt$getPageSource()[[1]] 
html <- read_html(html) 
btnLogin <- rt$findElement(using = "xpath", value = "//*[@id='glovalWrap']/div/div[1]/ul/li[1]")
btnLogin$clickElement();

# btnLogin <- rt$findElement(using = "xpath", value = "//*[@id="glovalWrap"]/div/div[1]/ul/li[1]/a/img")
btnLogin$clickElement();

id <- rt$findElement(using="xpath", value='//*[@id="id"]') 
pw <- rt$findElement(using="xpath", value='//*[@id="pw"]') 
id$sendKeysToElement(list("pose3690")) 
pw$sendKeysToElement(list("k!")) 

btnLogin <-
  rt$findElement(using = "xpath", value = '//*[@id="contents"]/div[2]/div/div/form/fieldset/div[2]/button')
btnLogin$clickElement()

btnIT <-
  rt$findElement(using = "xpath", value = '//*[@id="itmenu"]/a')
btnIT$clickElement()


html <- rt$getPageSource()[[1]] 
html <- read_html(html) 
nodes <- html_nodes(html, ".title")
length(nodes)
raw_data <- html_text(nodes)
raw_data <- gsub("\n|\t", "", raw_data)
raw_data
library(dplyr)
data <- class(data.frame(raw_data))

btnUrl <-
  rt$findElement(using = "xpath", value = '//*[@id="course_result"]/div[1]/div[2]/table/tbody/tr[1]/td[1]/a')
btnUrl$clickElement()
html2 <- rt$getPageSource()[[1]] 
html2 <- read_html(html2) 
nodes2 <- html_nodes(html2, ".review_count")
nodes2
rt$goBack()

btnUrl <-
  rt$findElement(using = "xpath", value = '//*[@id="course_result"]/div[1]/div[2]/table/tbody/tr[2]/td[1]/a')
btnUrl$clickElement()
html2 <- rt$getPageSource()[[1]] 
html2 <- read_html(html2) 
nodes2 <- html_nodes(html2, ".review_count")
nodes2

for(i=1.)
  btnUrl <-
  rt$findElement(using = "xpath", value = '//*[@id="course_result"]/div[1]/div[2]/table/tbody/tr[3]/td[1]/a')
btnUrl$clickElement()
html2 <- rt$getPageSource()[[1]] 
html2 <- read_html(html2) 
nodes2 <- html_nodes(html2, ".review_count")
nodes2

sys.sleep() # for delay time