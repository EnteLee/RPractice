# Linear Regression
# -1 < 0 < 1

x <- c(150,171,138,186,128,136,179,152,131,140)
y <- c(63,81,56,91,47,57,76,72,62,42)

lm_data <- lm(y~x)
lm_data
summary(lm_data)
x1 <- data.frame(x=170)
x1
predict(lm_data,x)
plot(y,x,col="blue")
abline(lm(x~y))
abline(lm_data,cex=1.3,pch=16)


library(ggplot2)
df <- mtcars[, c("mpg","disp","hp","wt")]
df


# rvest => text minig => wordcloud

# 텍스트 마이닝
# 문자로 된 데이터에서 가치있는 정보를 
# 얻어내는 방법

# 분석순서
# 1. 형태소 분석
# 2. 명사, 동사 형용사 등의 의미를 지닌 
#   품사 단어 추출
# 3. 빈도표 만들기
# 4. 시각화

install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")
library(rJava)
library(memoise)
library(KoNLP)
useNIADic()

library(rvest)
library(RSelenium)
library(igraph)
library(qdap)
library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)
library(scam)
library(NLP)
library(openNLP)
library(ggmap)
library(rworldmap)
library(rworldxtra)

text.clean = function(x)                    
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # HTML 태그를 제거하기위한 정규 표현식
  x  =  iconv(x, "latin1", "ASCII", sub="") # ASCII 문자 유지
  x  =  gsub("[^[:alnum:]]", " ", x)        # 영문숫자만 유지 
  x  =  tolower(x)                          # 소문자로 변환
  x  =  removeNumbers(x)                    # 숫자 제거
  x  =  stripWhitespace(x)                  # 공백제거
  x  =  gsub("^\\s+|\\s+$", "", x)          # 선행 및 후행 공백 제거
  return(x)
}

library(tm)
url <- "https://www.imdb.com/title/tt0910970/reviews?ref_=tt_ov_rt"
page1 <- read_html(url)
filter_page1 <- html_nodes(page1, ".content")
length(filter_page1)

stopword2 <- stopwords("english")
data <- html_text(filter_page1)
data <- gsub("\n"," ",data)
data <- gsub("'"," ", data)

text.clean(data)
data
data2 <- text.clean(data)
data2 <- removeWords(data2, stopword2)
data2 <- stripWhitespace(data2)
data2
length(data2)

library(stringr)
data3 <- str_split(data2, " ")
data3
table(data3)

-------
  
  
  url <- 
  "https://www.imdb.com/title/tt0910970/reviews?ref_=tt_ov_rt"
url
page1 <- read_html(url)
filter_page1 <- html_node(page1, ".content")
#length(filter_page1)
library(tm)
stopword2 <- stopwords("english")
data <- html_text(filter_page1)
data <- gsub("\n"," ", data)
data2 <- text.clean(data)
data2 <- removeWords(data2, stopword2)
#data <- gsub("'", " ", data)
data2 <- stripWhitespace(data2)
data2
length(data2)
# 
library(stringr)
#data2 <- gsub("s", " ", data2)
#data2 <- gsub("t", " ", data2)
data3 <- str_split(data2, " ")
data3
# 상위 가장 빈도수 높은 단어 10 추출하시오.

data4 <- table(data3)
head(data4, 5)
d <- data.frame(data4)
names(d)
head(d)
library(dplyr)
d2 <- d %>% select(data3, Freq) %>% 
  arrange(desc(Freq)) %>% 
  head(10)
d2
colnames(d2) <- c("d_name", "d_count")
library(ggplot2)

ggplot(data=d2, 
       aes(x = d_name, 
           y = d_count)) + geom_bar(stat = "identity")

# question01 - 
# html_nodes => 25건으로 된경우 동일한 처리를 
# 하시오

d2 <- d %>% select(data3, Freq) %>% 
  arrange(desc(Freq)) %>% 
  head(25)
d2
colnames(d2) <- c("d_name", "d_count")
library(ggplot2)

ggplot(data=d2, 
       aes(x = d_name, 
           y = d_count)) + geom_bar(stat = "identity")



str <- "stests"
str2 <- "stet a"
gsub("\\s[[:alpha:]]$","",str2)

install.packages("tokenizers")
library(rvest) # R webscraping package
library(RSelenium) # R login package
library(igraph)
library(qdap)
library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud) # 워드클라우드
library(ggplot2) # 그래프
library(scam)
library(NLP) # word parsing package
library(openNLP) 
library(ggmap)
library(rworldmap)
library(rworldxtra)
#for (i in 1:25){
url1 = "https://www.imdb.com/title/tt0910970/reviews?ref_=tt_ov_rt"
page1 = read_html(url1)
reviews1 = html_text(html_nodes(page1,'.content'))
class(reviews1)                               
#}
reviews = gsub("\n",' ',reviews)
writeLines(reviews,'test.txt')

temp.text = readLines(file.choose()) 
class(temp.text)
head(temp.text, 4)

data = data.frame(id = 1:length(temp.text), 
                  text = temp.text, stringsAsFactors = F)
dim(data)
colnames(data)
data$id
data$text

stpw1 = readLines(file.choose()) 
stpw2 = tm::stopwords('english')   
comn  = unique(c(stpw2, stpw1))    
stopwords = unique(gsub("'"," ",comn))

x  = text.clean(data$text)            
x  =  removeWords(x,stopwords)        
x  =  stripWhitespace(x)              
#x  =  stemDocument(x)


library(rJava)
library(memoise)
library(KoNLP)
useNIADic()
data <- readLines("hiphop.txt")
data
library(stringr)
data <- str_replace_all(data, "\\W"," ")
data
extractNoun("반갑습니다. 즐거운")
KoNLP::extractNoun("방갑습니다. 즐거운 하루 되세요")
nouns <- extractNoun(data)
nouns
class(nouns)
data2 <- unlist(nouns)
data3 <-table(data2)
data4 <- data.frame(data3, stringsAsFactors = F)
colnames(data4)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
set.seed(1234)
pal <- brewer.pal(8, "Dark2")
wordcloud(data4$data2, data4$Freq, min.freq=4, max.words = 200, random.order =F, colors = pal)
