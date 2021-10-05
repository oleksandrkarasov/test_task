# This code is compiled for topic modelling procedure and 
# analysis of prevailing topics for selected Twitter users.
#
# Author: Oleksandr Karasov, adapted from  Schweinberger, Martin. 2021. 
# Topic Modeling with R. Brisbane: The University of Queensland. 
# url: https://slcladal.github.io/topicmodels.html (Version 2021.10.01).

# install packages, if necessary
# install.packages("tm")
# install.packages("topicmodels")
# install.packages("reshape2")
# install.packages("wordcloud")
# install.packages("ggplot2")
# install.packages("dplyr")


library(dplyr)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)


#working directory to put the text data there, or set it, if necessary
getwd() 

# Read data to construct and further process the corpus of words
tweetsData = read.csv("twitter_sample_data.csv", header= TRUE, encoding="UTF-8")
englishStopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
summary(englishStopwords)
summary(tweetsData)

tweetsData<-tweetsData%>%rename(text=text1, doc_id=rand_id)
corpus <- Corpus(DataframeSource(tweetsData))
str(corpus)

# Quick clean up noizy text data
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, englishStopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

# Create document term matrix (DTM) to keep only commonly used words 
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# number of documents and terms in the matrix
dim(DTM)

# Remove occasional empty rows from DTM
selIdx <- slam::row_sums(DTM) > 0
DTM <- DTM[selIdx, ]

# Number of topics to use. The optimal value is beyond the scope of quick analysis
K <- 20
# Set random number generator seed to ensure replicable runs of the algorithm
set.seed(1)
# Compute the LDA model, Gibbs sampling (other methods possible)
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# Resulting posterior distribution (probability of topics)
tmResult <- posterior(topicModel)
# Format of the resulting object
attributes(tmResult)

nTerms(DTM)              # lengthOfVocab

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms

rowSums(beta)            # rows in beta sum to 1
nDocs(DTM)               # size of collection

# Exploring probability distributions (beta and theta) and terms of topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics

rowSums(theta)[1:10]     # rows in theta sum to 1
terms(topicModel, 10)


exampleTermData <- terms(topicModel, 10)
exampleTermData[, 1:8]

# Concatenate the most common topic terms to use as pseudo-names for topics
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# Get unique value of ids
exampleIds <- unique(tweetsData$doc_id)

# Create data frame with ids and their row's number for linking with input data
id_df <- data.frame(exampleIds)
id_df<- id_df%>% mutate(document= as.factor(row_number()))

# Get the length of the ids
N <- length(exampleIds)

# Preparing topic proportions per user for visualisation
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
vizDataFrame <-full_join(vizDataFrame ,id_df, by="document")#join vizDataFrame and id_df to get the id of users
str(vizDataFrame)

# Final visualisation
windows(70,50)
ggplot(data = vizDataFrame, aes(topic, value), ylab = "proportion") + 
  geom_bar(stat="identity", fill="red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
	legend.position = "none",
	axis.text = element_text(size=10)) +  
  coord_flip() +
  facet_wrap(~ exampleIds, ncol = 25,nrow =4)


#