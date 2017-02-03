# Loading Required Libraries

library(tm)
library(lattice)
library(ggplot2)
library(caret)

# Q1. You have to point towards the folder containing the input data
# Answer to Q1 is as follows

setwd("D:/downlds/-==STUDY==-/Edureka Data Science/Projects/NFL To Do")

data <- read.csv("NFL_SocialMedia_sample_data1.csv", header = TRUE)

head(data)


# Q2. After the input data has been fed, read machine log and separate out 'log' and 'time' columns. 
# Create text corpus
str(data)
Log_Data <- data$content
head(Log_Data)

str(data$tstamp)
head(data$tstamp)

time_data <- data

Dates <- format(as.POSIXct(strptime(time_data$tstamp,"%Y-%m-%d", tz="EST5EDT")) ,format = "%Y/%m/%d")
head(Dates)

Time <- format(as.POSIXct(strptime(time_data$tstamp,"%Y-%m-%dT%H:%M:%SZ",tz="EST5EDT")) ,format = "%H:%M:%S")
head(Time)
time_data$log <- Dates
time_data$TimePart <- Time
head(time_data)

# Q3. Convert the machine log into text corpus.
# Converting the machine log into text corpus
text_corpus <- Corpus(VectorSource(data$content))
head(text_corpus)
inspect(text_corpus[1:6])
# converting to plain text document
text_corpus <- tm_map(text_corpus, PlainTextDocument)
head(text_corpus)
inspect(text_corpus[1:6])

# Q4. Convert to Lower Case. 
to_lower <- tm_map(text_corpus, tolower)
#to_lower <- tm_map(text_corpus,content_transformer( tolower))
to_lower[[174]]
inspect(to_lower[1:6])

# Q5. Remove the Stopwords. 
remove_stop_words <- tm_map(to_lower, removeWords, stopwords("english"))

inspect(remove_stop_words[1:6])

# Q6. Remove Punctuations. 
remove_Punctuation <- tm_map(remove_stop_words,removePunctuation)
inspect(remove_Punctuation[1:6])

# Q7. Remove Numbers. 
remove_Numbers <- tm_map(remove_Punctuation,removeNumbers)
remove_Numbers[[6]]

# Q8. Eliminate the white spaces
strip_Whitespace <- tm_map(remove_Numbers,stripWhitespace)
inspect(strip_Whitespace[1:6])
#install.packages("SnowballC")
#library(SnowballC)
#stem_Document<-tm_map(strip_Whitespace, stemDocument) 
#stem_Document[[173]]
strip_Whitespace <- tm_map(strip_Whitespace, stemDocument, language="english")
Plain_TextDocument <- tm_map(strip_Whitespace, PlainTextDocument)
inspect(Plain_TextDocument[1:6])

#Q9. Create a dtm (Document Term Matrix) 
new_corpus <- Plain_TextDocument
dtm <- DocumentTermMatrix(new_corpus)
inspect(dtm)
inspect(dtm[1:30, 1001:1010])

# Q10. Determine the Term Frequency and tfxidf 
findFreqTerms(dtm,10)

sparse = removeSparseTerms(dtm, 0.9999)

inspect(sparse)

inspect(sparse[1:30, 1:6])

dtm_tfxidf <- weightTfIdf(dtm)

inspect(dtm_tfxidf[1:30, 1:6])


#dtm_tfxidf <- DocumentTermMatrix(new_corpus, control = list(weighting = weightTfIdf))
#inspect(dtm_tfxidf[1:10, 1001:1010])
# OR
#dtm_tfxidf <- weightTfIdf(dtm)
#inspect(dtm_tfxidf[1:10, 1001:1010])

# Q11. Use the K-means package to do document clustering.

m <- as.matrix(dtm_tfxidf)

colnames(m) = make.names(colnames(m))
rownames(m) <- 1:nrow(m)

m

# Q12. Normalize the Vectors so that Euclidean makes sense. 
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
#is.na(m_norm)
m_norm[is.na(m_norm)]<-0

# Q13. Cluster the data into 10 clusters. 
#setting seed to get random results
set.seed(123)
#kmeancluster <- kmeans(na.omit(m_norm), 10)
kmeancluster <- kmeans(m_norm, 10)
table(kmeancluster$cluster)

# Q14. Point towards the folder containing the Interim Data i.e. "Cluster_Out.csv" 
Cluster_Outfile <- data.frame(Log = data$content, Cluster = kmeancluster$cluster)
write.csv(Cluster_Outfile, file = "Cluster_Out.csv", row.names=TRUE)

dataCluster <- read.csv("Cluster_Out.csv")

# Q15. Find the top 5 words discussed in each of the 10 clusters. Write these cluster wise top words into the "TopWords.csv" file and generate the word cloud for each of the cluster
sparce_dtm <- removeSparseTerms(dtm, sparse= 0.9999)
newdtm <- as.matrix(sparce_dtm)

for (N in 1:length(kmeancluster$withinss)) {
  a <- sort(colSums(newdtm[kmeancluster$cluster == N, ]),
            decreasing = TRUE)
  df <- data.frame(names(a), a)
  colnames(df) <- c("word","count")
  
  if (N == 1){
    x <- data.frame(N, length(which(dataCluster$Cluster == N )), df$word[1:5], 
                    df$count[1:5], as.numeric(rownames(Cluster_Outfile))[1:5])
    colnames(x) = c("Log Group", "Log Count", "Top Words", "Word Count", "Counter")
  } else {
    y <- data.frame(N, length(which(dataCluster$Cluster == N )), df$word[1:5],
                    df$count[1:5], as.numeric(rownames(Cluster_Outfile))[1:5]) 
    colnames(y) = c("Log Group", "Log Count", "Top Words", "Word Count", "Counter")
    x <- rbind(x, y)
  }
}

# Writing these cluster wise top words into the "TopWords.csv" file
write.csv(x, file = "Topwords.csv", row.names=FALSE)


#install.packages("SnowballC") 
#install.packages("wordcloud") 
#install.packages("RColorBrewer")
#install.packages("Rcpp") 

library(tm)
library(tmap)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 

for (n in 1:10){
  i <- as.character(n)
  print(c("WordCount for Cluster", n), quote=F)
  png(paste("WordCount for Cluster", i, ".png"), width=12, height=8, units="in", res=300)
  color <- brewer.pal(8,"Paired")
  wordcloud(new_corpus[kmeancluster$cluster==n],  min.freq=5, 
            max.words=30, colors=color, random.order=F)
  dev.off()
}


