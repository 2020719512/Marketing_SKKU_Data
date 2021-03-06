## crawling
library(rvest)
library(RCurl)

library(tm)


data <- read.csv("https://raw.githubusercontent.com/2020719512/Marketing_SKKU_Data/master/irobot.csv")

#load more stopwords to remove them
stpwords <- read.csv("https://raw.githubusercontent.com/2020719512/Marketing_SKKU_Data/master/stop_words.txt")
stpwords <- as.vector(unlist(stpwords)) #convert data frame to vector


#head(data)
data<-data[,2]
#head(data,10)
#data2 <- iconv(data, 'UTF-8', 'ASCII')
docs <- Corpus(VectorSource(data))
summary(docs)
docs <- tm_map(docs, removePunctuation)  
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
#docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, c("robot","roomba", "vacuuming","vacuum", "deebot", "ve", "��s", "don't", "doesn't", "'m", "ive", "'s", "'ve", "'m"))
docs <- tm_map(docs, removeWords, stpwords)

#remove this words for better clusters
docs <- tm_map(docs, removeWords, c("use", "day", "clean", "hair"))

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


library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.915) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   

k <- 4
set.seed(123)
kfit <- kmeans(d,k) 
plot.new()
op = par(mfrow = c(1, 1))
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex = 1,
         main = paste("K-Means Clustering with ", k, "Cluster"))


#------------------------
dtmss <- removeSparseTerms(dtm, 0.99) # This makes a matrix that is only 15% empty space.
dtmss

library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
fit
 
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=15)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=15, border="red") # draw dendogram with red borders around the 8 clusters   
# # Try many other k's for the interpretation of your data.


plot.new()
hcd = as.dendrogram(fit)
op = par(mfrow = c(2, 1))
plot(cut(hcd, h = 100)$upper, main = "Upper tree of cut at h=100"))
plot(cut(hcd, h = 100)$lower[[2]], main = "Second branch of lower tree with cut at h=100")


#---------------------------
## 3.5  Term Correlations (Co-occurence)
  # See the description above for more guidance with correlations.
  # If words always appear together, then correlation=1.0.    
findAssocs(dtms, "sensors", corlimit=0.01) # specifying a correlation limit of 0.01
findAssocs(dtms, "stopped", corlimit=0.1) # specifying a correlation limit of 0.01
findAssocs(dtms, "clean", corlimit=0.01) # specifying a correlation limit of 0.01
