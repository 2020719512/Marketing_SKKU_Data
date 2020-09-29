## crawling
library(rvest)
library(RCurl)
'setwd("C:/Users/ammy7/OneDrive/πŸ≈¡ »≠∏È/texts")

url <- "https://www.amazon.com/ECOVACS-N79S-Connectivity-Controls-Self-Charging/product-reviews/B077HW9XM7/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber="
N_pages <- 500

A <- NULL
for(j in 1: N_pages){
  alexa <- read_html(paste0(url, j))
  B <- cbind(alexa %>% 
               html_nodes(".review-text") %>% 
               html_text(), alexa %>%     
               html_nodes("#cm_cr-review_list .review-date") %>% 
               html_text()     )
  A <- rbind(A,B)
  print(j)
}

print(j)
head(A,15)
tail(A,5)
write.csv(data.frame(A),"EcovacsDEEBOT.csv")
A[3, 1]

## Word Cloud

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")

install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

setwd("C:/Users/ammy7/OneDrive/πŸ≈¡ »≠∏È/texts")
cname <- file.path("C:/Users/ammy7/OneDrive/πŸ≈¡ »≠∏È", "texts")   
cname   
dir(cname)'

library(tm)
x <- getURL("https://raw.githubusercontent.com/2020719512/Marketing_SKKU_Data/master/ecovacs-deebot2709.csv")
data <- read.csv(text = x)
#data <- read.csv("EcovacsDEEBOT.csv")
head(data)
data<-data[,2]
head(data,10)
docs <- Corpus(VectorSource(data))
summary(docs)

docs <- tm_map(docs, removePunctuation)  
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, c("vacuum", "deebot", "'s", "don't", "doesn't", "didnt", "'m", "ive", "'s", "will", "much", "'ve", "'m", " ", "months"))
docs <- tm_map(docs, stripWhitespace)

library(tm)
install.packages("SnowballC")
library(SnowballC)
dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:20,1:20])
dtm <- DocumentTermMatrix(docs)   
inspect(dtm)
mdtm <- as.matrix(dtm) 

write.csv(unlist(dtm), "N_DTM_1.csv")

class(dtm)

tdm <- TermDocumentMatrix(docs)   

write.csv(as.matrix(dtm),"aa_1.csv")

write.csv(mdtm,"aa2_1.csv")

inspect(tdm)
inspect(dtm)

freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix_1.csv")   
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

