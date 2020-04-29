library(twitteR)
library(plyr)
library(stringr)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tidyr)
#library(ThinkToStartR)
#library(thinkr)




consumer_key <- "ApDVk0K8YVqi0PmKeZd8GklE6"
consumer_secret <- "jplfn8wVuDOZ6HvkHBWDhn4BENr3Fs9e9W7Qg4cozEHRHANAdn"
access_token <- "612524983-xadT2u0cf2CZee5iZzl4rHTCfPnposYtMvq90iEy"
access_secret <- "yYrauBDDmZnl7wkY9tTFMksI8fh9NQtIeIYJQq8nENsDq"



setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



#Get tweets about "House of Cards", due to the limitation, we'll set n=3000
netflix.tweets<- searchTwitter("#chick-fil-a",n=1000,lang="en") 





tweet=netflix.tweets[[1]]
tweet$getScreenName()




tweet$getText()




netflix.text=laply(netflix.tweets,function(t)t$getText())
length(netflix.text)



head(netflix.text) 





#performing data cleaning and store in csv.file
netflix.text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", netflix.text)
netflix.text = gsub("@\\w+", "", netflix.text)
netflix.text = gsub("[[:punct:]]", "", netflix.text)
netflix.text = gsub("[[:digit:]]", "", netflix.text)
netflix.text = gsub("http\\w+", "", netflix.text)
netflix.text = gsub("[ \t]{2,}", "", netflix.text)
netflix.text = gsub("^\\s+|\\s+$", "", netflix.text)
netflix.text <- str_replace_all(netflix.text," "," ")
netflix.text <- str_replace_all(netflix.text,"#[a-z,A-Z]*","")
netflix.text <- str_replace_all(netflix.text,"@[a-z,A-Z]*","")  
netflix.text <- tolower(netflix.text)
head(netflix.text)




write(netflix.text, "HouseofCards_Tweets.csv",ncolumn=1)



#perform sentimental analysis 
sentiment <- get_nrc_sentiment(netflix.text)
head(sentiment)



#perform data transformation and add the sentimental analysis into tweets
netflix.text <- cbind(netflix.text, sentiment)
head(netflix.text)



netflix.text$negative<- ifelse(netflix.text$negative>=1,1,0)
netflix.text$positive<- ifelse(netflix.text$positive>=1,1,0)
netflix.text$positive<- ifelse(netflix.text$positive>=1 & netflix.text$negative>=1,0,netflix.text$positive)
netflix.text$negative<- ifelse(netflix.text$positive>=1 & netflix.text$negative>=1,0,netflix.text$negative)




#use the graphic to present the findings



#set up y = count
Totals<- data.frame(colSums(netflix.text[,c(11:10)]))
names(Totals) <- "score"



#set up x = sentiment
Totals <- cbind("sentiment" = rownames(Totals), Totals)
rownames(Totals) <- NULL



ggplot(Totals, aes(x = sentiment, y = score)) +
  geom_bar(aes(fill = sentiment), stat = "identity", position = "dodge", width = 1) +
  xlab("sentiment") + ylab("sentiment Scores") + ggtitle("Sentiment Scores for All Tweets")




head(sentiment)



Totals