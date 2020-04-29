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
library(ThinkToStartR)
#library(thinkr)



library(sentimentr)
library(plyr)
library(RCurl)
library(wordcloud)



consumer_key <- "ApDVk0K8YVqi0PmKeZd8GklE6"
consumer_secret <- "jplfn8wVuDOZ6HvkHBWDhn4BENr3Fs9e9W7Qg4cozEHRHANAdn"
access_token <- "612524983-xadT2u0cf2CZee5iZzl4rHTCfPnposYtMvq90iEy"
access_secret <- "yYrauBDDmZnl7wkY9tTFMksI8fh9NQtIeIYJQq8nENsDq"



setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



#Get tweets about "House of Cards", due to the limitation, we'll set n=3000
netflix.tweets<- searchTwitter("#mcdonald's",n=1000,lang="en") 





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






# Calculate the mean sentiment score of each tweet and its result (positive, negative or neutral)
sen_score <- c()
sen_result <- c()
for (i in netflix.text){
  # Calculate the sentiment
  sen <- sentiment(i,n.before=0, n.after=0, amplifier.weight=0)
  
  # Calculate the mean sentiment
  mean_sen <- mean(sen$sentiment)
  
  # Determine if sentiment is positive or negative
  # Neautral set as within 0.05 of zero
  if (mean_sen < -0.01){
    sen_result <- c(sen_result, "negative")
  } else if (mean_sen > 0.01) {
    sen_result <- c(sen_result, "positive")
  } else {
    sen_result <- c(sen_result, "neutral")
  }
  
  # Add sen score to sen_score
  sen_score <- c(sen_score, mean_sen)
}





# Display results in a dataframe
sen_analysis <- data.frame(netflix.text, sen_score, sen_result)
sen_analysis




sen_analysis['positive']<-ifelse(sen_analysis$sen_result=='positive',1,0)
sen_analysis['negative']<-ifelse(sen_analysis$sen_result=='negative',1,0)



#for (i in 1:nrow(sen_analysis)){
#if (sen_analysis$sen_result[i]=='positive'){
#  sen_analysis$positive[i]=1
#}else if (sen_analysis$sen_result[i]=='negative'){
# sen_analysis$positive[i]=0
#}else {
# sen_analysis$positive[i]=2
#}
#}
#sen_analysis<-unlist(sen_analysis)
sen_analysis



Totals<- data.frame(colSums(sen_analysis[,c(4:5)]))
names(Totals) <- "score"



#set up x = sentiment
Totals <- cbind("sentiment" = rownames(Totals), Totals)
rownames(Totals) <- NULL



Totals



#sen_analysis<-as.data.frame(sen_analysis)



ggplot(Totals, aes(x = sentiment, y = score)) +
  geom_bar(aes(fill = sentiment), stat = "identity", position = "dodge", width = 1) +
  xlab("sentiment") + ylab("sentiment Scores") + ggtitle("Sentiment Scores for All Tweets")