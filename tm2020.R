# tm2020

rm(list=ls(all=TRUE)) #clear all

# Please read carefully
# Please run the code by each section separated by #.
# Follow the explanation from the source: https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html

# must have items for your group presentation includes:
#   word freq table
#   word freq plot
#   word cloud for each star rating
#   hierarchal clustering
#   k-means clustering

# The most important thing is that how to interpret your data not just show the data and
#results of your data analysis.



##########################################################################################
# 1. Installing relevant packages
##########################################################################################

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")

 install.packages(Needed, dependencies = TRUE)

 install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")


 
##########################################################################################
# 2.                             Loading Texts                                         #
##########################################################################################      
#
#     Start by saving your text files in a folder titled:    "texts"
#     This will be the "corpus" (body) of texts you a2re mining.   
#  
#     Next, choose the type of computer you have...

######
# **On a Mac**, save the folder to your *desktop* and use the following code chunk:
######

#cname <- file.path("~", "Desktop", "texts")   
#cname   
#dir(cname)   # Use this to check to see that your texts have loaded.   

######    
# *On a PC*, save the folder to your *C: drive* and use the following code chunk:  
######

cname <- file.path("C:/Users/poldeborsu/OneDrive/Documents/Universite/Master 2 Korea/Marketing data analytics","GroupAssignment") # This part should be revised by your own location of your texts folder.
# Please make sure you exchange \ with /. Make sure you are using /.
cname   
dir(cname)   

###########################
# Change the working directory with your own file location
setwd(cname)



##########################################################################################
# 3.                               Start Your Analyses                                     #
##########################################################################################
# **Load the R package for text mining and then load your texts into R.**
library(tm)


###################################
# 3.0 Generating a Corpus, one document data set
#
library(RCurl)
x <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
data <- read.csv(text = x)
#data <- read.csv("RobotRoomba.csv") #change the name of the file
data<-data[,2]
head(data,10)
docs <- Corpus(VectorSource(data))
#summary(docs)


# 3.1    Preprocessing   ==parsing                                  #
##       
docs <- tm_map(docs, removePunctuation)   
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, c("echo","show")) # you can add more irrelevant words hear!
docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, PlainTextDocument)
# *This is the end of the preprocessing stage.*   

### Stage the Data      
dtm <- DocumentTermMatrix(docs)   
#inspect(dtm[1:20,1:20])
dtm <- DocumentTermMatrix(docs)   
inspect(dtm)
mdtm <- as.matrix(dtm) 

write.csv(unlist(dtm), "N_DTM.csv")

class(dtm)

tdm <- TermDocumentMatrix(docs)   

write.csv(as.matrix(dtm),"aa.csv")

write.csv(mdtm,"aa2.csv")

inspect(tdm)
inspect(dtm)


# 3.2 Explore your data
##
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix.csv")   
### FOCUS - on just the interesting stuff...   
#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.95) # This makes a matrix that is 10% empty space, maximum.   
dtms


### 3.3 Word Frequency   
head(table(freq), 20)   
# The above output is two rows of numbers. The top number is the frequency with which 
# words appear and the bottom number reflects how many words appear that frequently. 
#
tail(table(freq), 20)   
# Considering only the 20 greatest frequencies
#
# **View a table of the terms after removing sparse terms, as above.
freq <- colSums(as.matrix(dtms))   
freq   
# The above matrix was created using a data transformation we made earlier. 
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 20)   #This will show you the top 20 most frequently mentioned words
tail(freq, 10)   #This will show you the bottom 10 least frequently mentioned words

# **An alternate view of term frequency:**   
# This will identify all terms that appear frequently (in this case, 50 or more times).   
findFreqTerms(dtm, lowfreq=30)   # Change "30" to whatever is most appropriate for your data.

# Another format to try
wf <- data.frame(word=names(freq), freq=freq)   
head(wf,30)  
head(wf,10)

### 3.4 Plot Word Frequencies
# **Plot words that appear at least 100 times.**   
library(ggplot2)   
wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq > 100), aes(x = reorder(word, -freq), y = freq)) 
# You can modify 300 into your own number for best output
p <- p + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=45, hjust=1))   
p
#  
## Relationships Between Terms
# IF YOU WISH TO MAXIMIZE YOUR OUTPUT, YOU NEED TO HAVE SEVERAL EXCEL FILES.
# ONE WAY TO HAVE FIVE FILES IS THAT FIRST SORT THE REVIEWS WITH THE STAR RATING.


### 3.5  Term Correlations (Co-occurence)
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.    
findAssocs(dtms, "quality", corlimit=0.1) # specifying a correlation limit of 0.01
findAssocs(dtms, "sound", corlimit=0.1) # specifying a correlation limit of 0.01
findAssocs(dtms, "charging", corlimit=0.01) # specifying a correlation limit of 0.01
##########



### 4. Word Clouds!   
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.9) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
# You can change the color from http://www.datavis.ca/sasmac/brewerpal.html
# In this case, 8 means the number of colors using among the color set.
# wordcloud(names(freq), freq, min.freq=60)    
# wordcloud(names(freq), freq, max.words=50)    
#   wordcloud(names(freq), freq, min.freq=10, rot.per=0.7, colors=dark2)    
wordcloud(names(freq), freq, max.words=100, rot.per=0.6, colors=dark2)    
# Also adjust the `max.words=`, `rot.per=`, `colors=` to any value you feel is necessary.



### 5. Clustering by Term Similarity


### 5.1 Hierarchical Clustering   
dtmss <- removeSparseTerms(dtm, 0.99) # This makes a matrix that is only 15% empty space.
dtmss

library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
fit

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red") # draw dendogram with red borders around the 8 clusters   
# Try many other k's for the interpretation of your data.


# 5.1.2. Alternative option
# plot dendrogram with some cuts; Also see https://rpubs.com/gaston/dendrograms
plot.new()
hcd = as.dendrogram(fit)
op = par(mfrow = c(2, 1))
plot(cut(hcd, h = 100)$upper, main = "Upper tree of cut at h=100")
plot(cut(hcd, h = 100)$lower[[2]], main = "Second branch of lower tree with cut at h=100")


### 5.2 K-means clustering   #iteration, stopping rule
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.94) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d,2) 
plot.new()
op = par(mfrow = c(1, 1))
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   



# Source: https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html
