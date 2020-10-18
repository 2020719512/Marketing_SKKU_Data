## crawling
library(rvest)
library(RCurl)
library(tm)
library(SnowballC)

data <- read.csv("https://raw.githubusercontent.com/2020719512/Marketing_SKKU_Data/master/ecovacs-deebot2709.csv")

stpwords <- read.csv("https://raw.githubusercontent.com/2020719512/Marketing_SKKU_Data/master/stop_words.txt")
stpwords <- as.vector(unlist(stpwords)) #convert data frame to vector

data<-data[,2]
docs <- Corpus(VectorSource(data))
summary(docs)
docs <- tm_map(docs, removePunctuation)  
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stpwords)
docs <- tm_map(docs, removeWords, c("its","ive","robot","roomba", "vacuuming","vacuum", "deebot", "ecovacs", "ve", "'s", "don't", "doesn't", "'m", "ive", "'s", "'ve", "'m"))
docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, removeWords, stopwords("english"))

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


------------------------------------------------------------
#word frequency chart
library(ggplot2)

df <- data.frame(stringsAsFactors=FALSE,
                 word = head(wf$word, 15),
                 freq = head(wf$freq, 15)
)


ggplot(df, aes(x = reorder(word, freq), y = freq)) +
  geom_col() +
  labs(title="Word Frequency Chart ",
       x = NULL,
       y = "Frequency") +
  coord_flip()

wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq > 400), aes(x = reorder(word, -freq), y = freq)) +   labs(title="Word Frequency Chart ",
                                                                                      x = NULL,
                                                                                      y = "Frequency")
# You can modify 300 into your own number for best output
p <- p + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=45, hjust=1))   
p

-------------------------------------------------------------------------

library(wordcloud)
dtms <- removeSparseTerms(dtm, 0.9) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm))# Find word frequencies
str(freq)
class(freq)
freq_1 <- freq %>% as.data.frame() %>% arrange(desc(.)) %>% filter(rownames()%in%c("deebot", "'s", "will", "much", "'ve", "'m", " "))
freq %>% as.data.frame() %>% arrange(desc(.)) %>% head(200)
rownames(freq_1)
color <- brewer.pal(8, "Accent")
color <- c("#7FC97F", "#BEAED4" ,"#FDC086" ,"#87D4A1" ,"#386CB0", "#F0027F", "#BF5B17","#666666")

brewer.pal.info

wordcloud(names(freq), freq, max.words=150, rot.per=0.4, colors=color, random.order=F)


