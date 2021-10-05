# This code is created for sentiment polarity assessment
# with sentimentr package. 
# Author: Oleksandr Karasov, R community and contributors

# install.packages("sentimentr")
# install.packages("tidyverse")
# install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(sentimentr)
library(tidyverse)
library(lubridate)
library(zoo)

#Open data (workind directory is assumed)
twitter_df = read.csv("twitter_sample_data.csv", header= TRUE, encoding="UTF-8")
head(twitter_df)

sentiment=sentiment_by(twitter_df$text1)
summary(sentiment$ave_sentiment)
qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Twitter Sentiment Histogram")

twitter_df$ave_sentiment<-sentiment$ave_sentiment
twitter_df$sd_sentiment<-sentiment$sd
str(twitter_df)

twitter_df$rand_id<-as.factor(twitter_df$rand_id)

#Median calculation for tweets by rand_id
twitter_df<-twitter_df%>%
	group_by(rand_id)%>%
	mutate(ave_sentiment_median=median(ave_sentiment,na.rm=FALSE))%>%
	ungroup()

#Twitter users ranged by sentiment polarity of their tweets
windows(45,15)
ggplot(twitter_df, aes(x=reorder(rand_id,ave_sentiment_median)))+
	geom_violin(aes(y=ave_sentiment),fill="purple", width=1.5, color="#502c6b")+
	theme_minimal()+
	theme(axis.text.x=element_text(size=8,angle=90),
        axis.title=element_text(size=11))+
	ggtitle("Twitter's sentiment polarity")+
	xlab("User ID")+
	ylab("Sentiment score")

##Plot by date
#get the date column
twitter_df$date<-ymd_hms(twitter_df$created_at)
twitter_df$my<-format(twitter_df$date, format="%Y-%m")
twitter_df$my<-as.yearmon(twitter_df$my)

#median ave_sentiment per month
twitter_df<-twitter_df%>%
		group_by(my)%>%
		mutate(median_sentiment=median(ave_sentiment,na.rm =TRUE),
			sd_sentiment=sd(ave_sentiment,na.rm =TRUE))%>%
		ungroup()
str(twitter_df)

#get unique values for plotting
twitter_df2<-twitter_df%>%select(median_sentiment,my,sd_sentiment )%>%unique()

windows(30,20)
ggplot(data=twitter_df2,aes(x=my))+
	geom_segment(aes(x = my, y = (median_sentiment-sd_sentiment), xend = my, 
				yend = (median_sentiment+sd_sentiment), colour = "segment"), size=3, color='#ededed', alpha=0.5)+
	geom_point(aes(y=median_sentiment), color='#ed2b2b', size=2)+
	geom_line(aes(y=median_sentiment),color='#ed2b2b', alpha=0.5)+
	theme_minimal()+ 
	scale_x_yearmon()+
	xlab("Date") + 
	ylab("Median sentiment polarity per month")+
	theme(axis.text=element_text(size=10),
		axis.title=element_text(size=12))


