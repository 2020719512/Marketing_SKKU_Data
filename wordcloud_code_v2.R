## crawling
library(rvest)
library(RCurl)
library(tm)

#x <- getURL("https://raw.githubusercontent.com/2020719512/Marketing_SKKU_Data/master/ecovacs-deebot2709.csv")
data <- read.csv("https://raw.githubusercontent.com/2020719512/Marketing_SKKU_Data/master/ecovacs-deebot2709.csv")
#data <- read.csv(text = x, encoding = "UTF-8")
#head(data)
data<-data[,2]
#head(data,10)
#data2 <- iconv(data, 'UTF-8', 'ASCII')
docs <- Corpus(VectorSource(data))
summary(docs)
docs <- tm_map(docs, removePunctuation)  
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, c("vacuum", "deebot", "ve", "'s", "don't", "doesn't", "didnt", "'m", "ive", "'s", "will", "much", "'ve", "'m"))
docs <- tm_map(docs, stripWhitespace)

library(tm)
#install.packages("SnowballC")
library(SnowballC)
dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:20,1:20])
dtm <- DocumentTermMatrix(docs)   
inspect(dtm)
mdtm <- as.matrix(dtm) 

#write.csv(unlist(dtm), "N_DTM_1.csv")

class(dtm)

tdm <- TermDocumentMatrix(docs)   

#write.csv(as.matrix(dtm),"aa_1.csv")
#write.csv(mdtm,"aa2_1.csv")

inspect(tdm)
inspect(dtm)

freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
#write.csv(m, file="DocumentTermMatrix_1.csv")   
dtms <- removeSparseTerms(dtm, 0.95)   
dtms

head(table(freq), 20)

tail(table(freq), 20)   

freq <- colSums(as.matrix(dtms))   

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   

wf <- data.frame(word=names(freq), freq=freq)   

library(wordcloud)
dtms <- removeSparseTerms(dtm, 0.9) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm))# Find word frequencies
str(freq)
class(freq)
freq_1 <- freq %>% as.data.frame() %>% arrange(desc(.)) %>% filter(rownames()%in%c("deebot", "'s", "will", "much", "'ve", "'m", " "))
freq %>% as.data.frame() %>% arrange(desc(.)) %>% head(200)
rownames(freq_1)
color <- brewer.pal(8, "Accent")
brewer.pal.info

wordcloud(names(freq), freq, max.words=150, rot.per=0.4, colors=color, random.order=F)
