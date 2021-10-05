# This code is created for sentiment polarity assessment
# with sentimentr package. 
# Author: Oleksandr Karasov, R community and contributors

install.packages("hrbrthemes") # themes for ggplot
install.packages("sentimentr") # package for sentiment polarity estimation

library(ggplot2) # for visualisation
library(dplyr) # to create sequences of tasks
library(hrbrthemes)
library(sentimentr)

# Reading and exploring the source file
tweetsData = read.csv("twitter_sample_data.csv", header= TRUE)
head(tweetsData)

# Getting sentiment polarity by each tweet
sentiment=sentiment_by(tweetsData$text1)
summary(sentiment$ave_sentiment)

# Exploring sentiment polarity 
qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Twitter Sentiment Histogram")
tweetsData$ave_sentiment=sentiment$ave_sentiment
tweetsData$sd_sentiment=sentiment$sd
str(tweetsData)
tweetsData$rand_id<-as.factor(tweetsData$rand_id)

#Median calculation for tweets by rand_id
tweetsData<-tweetsData%>%
	group_by(rand_id)%>%
	mutate(ave_sentiment_median=median(ave_sentiment,na.rm=FALSE))%>%
	ungroup()

#Twitter users ranged by sentiment polarity of their tweets
ggplot(tweetsData, aes(x=reorder(rand_id,ave_sentiment_median)))+
	geom_violin(aes(y=ave_sentiment),fill="purple", width=1.5, color="#502c6b")+
	theme_minimal()+
	theme(axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14))+
	ggtitle("Twitter's sentiment polarity") +
	xlab("User ID")+
	ylab("Sentiment score")

windows(30,15)