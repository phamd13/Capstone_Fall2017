###################################################################
## Twitter Sick Data Analysis
## Description: For Fall 2017 CMDA Capstone Project - Sick Tweets
## Managed by Daniel Pham
## Last Updated: 11/10/17
###################################################################

setwd("~/VirginiaTechDocs/Class/Fall 2017/CMDA4864/TwitterScraper")
rm(list=ls())

# Loading libraries
library(twitteR)
library(ggplot2)
library(MASS)
library(plyr)
library(dplyr)
library(stringr)
library(grid)
library(maps)
library(dismo)
library(RColorBrewer)
library(wordcloud)
library(tm)

# Connect to Twitter
load("oauth_token.Rdata")

## setup twitter handshake
setup_twitter_oauth(my_oauth$consumerKey, my_oauth$consumerSecret,
                    my_oauth$oauthKey, my_oauth$oauthSecret)
###############################################

# read and merge csv tweets datasets
tweets.f1= read.csv("Data/sicktweets_11-9.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
tweets.f2 = read.csv("Data/sicktweets_11-13.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
tweets.df = rbind(tweets.f1, tweets.df)    # Combine all datasets into one
tweets.df = tweets.f2[,-1]

#################################################
### DATA CLEANING (duplicates, bots, replies) ###
#################################################
tweets.df = tweets.df[is.na(tweets.df$replyToSN),]     # subset of "pure" tweets (no reply)
tweets.df = tweets.df[!duplicated(tweets.df$text), ]   # Remove duplicates
tweets.df$text = gsub("<.*$", "", tweets.df$text)   # Remove emojis tags

# Bot Cleaning
names.df = as.data.frame(sort(table(tweets.df$screenName), decreasing = T))
names.df$Var1 = as.character(names.df$Var1)
bots.df = data.frame(name = character(), postN = integer(), followersN = integer(), stringsAsFactors = F)

for(i in 1:nrow(names.df)) {
  if(names.df[i,2] > 5) user = getUser(names.df[i,1])   # freq > 5 only
  if(user$statusesCount > 10000 && user$followersCount < 100) {
    bots.df[i,1] = user$screenName
    bots.df[i,2] = user$statusesCount
    bots.df[i,3] = user$followersCount
  }
}
bots.df = bots.df[rowSums(is.na(bots.df)) == 0,]
tweets.df = subset(tweets.df, !(tweets.df$screenName %in% bots.df$name)) # remove bots from main dataset

write.csv(tweets.df, file = "Data/sicktweets_All.csv")

###############################################
### WORDCLOUD FROM TWEETS             			###
###############################################
tweets_cloud = paste(tweets.df$text, collapse = " ")   # merge tweets text into one string
#convert all text to lower case
tweets_cloud = tolower(tweets_cloud)
# Replace blank space ("rt")
tweets_cloud = gsub("rt", "", tweets_cloud)
# Replace @UserName
tweets_cloud = gsub("@\\w+", "", tweets_cloud)
# Remove punctuation
tweets_cloud = gsub("[[:punct:]]", "", tweets_cloud)
# Remove links
tweets_cloud = gsub("http\\w+", "", tweets_cloud)
# Remove tabs
tweets_cloud = gsub("[ |\t]{2,}", " ", tweets_cloud)
# Remove blank spaces at the beginning
tweets_cloud = gsub("^ ", "", tweets_cloud)
# Remove blank spaces at the end
tweets_cloud = gsub(" $", "", tweets_cloud)

tweets_cloud_corpus = Corpus(VectorSource(tweets_cloud))
tdm = TermDocumentMatrix(tweets_cloud_corpus,
                         control = list(stopwords = stopwords("english")))
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing = TRUE)
dm = data.frame(word = names(word_freqs), freq = word_freqs)

# plot and save wordcloud in png format
png("sickTweetsCloud.png", width=5, height=5, units="in", res=300)
wordcloud(dm$word, dm$freq, min.freq = 80, colors=brewer.pal(8, "Dark2"), random.order = FALSE)
dev.off()

###############################################
### WORKING WITH GEOLOCATED TWEETS			###
###############################################

# read in memory the geolocated tweets we collected before
tweets = parseTweets("tweets_geo.json")

# keeping only geolocated tweets with precise long/lat information
tweets = tweets[!is.na(tweets$lon),]

## Now we create a data frame with the map data 
map.data = map_data("state")

# And finally we use ggplot2 to draw the map:
# 1) map base
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "grey90", 
    color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
    # 2) limits for x and y axis
    scale_x_continuous(limits=c(-125,-66)) + scale_y_continuous(limits=c(25,50)) +
    # 3) adding the dot for each tweet
    geom_point(data = tweets, 
    aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "darkblue") +
    # 4) removing unnecessary graph elements
    theme(axis.line = element_blank(), 
    	axis.text = element_blank(), 
    	axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) 

# How many tweets are coming from each state?
states = map.where(database="state", x=tweets$lon, y=tweets$lat)
head(sort(table(states), decreasing=TRUE))

###############################################
### SENTIMENT ANALYSIS						###
###############################################

# Loading tweets we will use
tweets = read.csv("Data/sicktweets_All.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# loading lexicon of severe and common sickness words
lexicon = read.csv("Data/sicklexicon.csv", header = TRUE, sep = ",", stringsAsFactors=F)
sev = lexicon$word[lexicon$severity == "severe"]
com = lexicon$word[lexicon$severity == "common"]

# a look at a random sample of severe and common words
sample(sev, 10)
sample(com, 10)

score.sentiment = function(sentences, sev.words, com.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, sev.words, com.words) {
    sentence = gsub('[[:punct:]]', "", sentence)
    sentence = gsub('[[:cntrl:]]', "", sentence)
    sentence = gsub('\\d+', "", sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    sev.matches = match(words, sev.words)
    com.matches = match(words, com.words)
    sev.matches = !is.na(sev.matches)
    com.matches = !is.na(com.matches)
    score = sum(sev.matches) - sum(com.matches)
    return(score)
  }, sev.words, com.words, .progress=.progress)
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

tweets$text = as.factor(tweets$text)
scores = score.sentiment(tweets$text, sev, com, .progress = 'text')
write.csv(scores, "Data/sentscores.csv", row.names = TRUE)

stat = scores
stat$created = tweets$created
stat$created = as.Date(sapply(strsplit(stat$created,' '),'[',1),'%m/%d/%Y') # remove timestamp
stat = mutate(stat, tweet = ifelse(stat$score > 0, 'severe', 
                                   ifelse(stat$score < 0 & stat$score > -3, 'common', 'severe')))
by.tweet = group_by(stat, tweet, created)
by.tweet = summarise(by.tweet, number = n())
by.tweet = by.tweet[order(by.tweet$created),]
by.tweet = `colnames=`(by.tweet, c("tweet", "created", "count"))

# Plot
ggplot(by.tweet[1:52,], aes(created, count)) + geom_line(aes(group=tweet, color=tweet), size = 1.2) +
  geom_point(aes(group=tweet, color=tweet), size = 4) +
  #theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
  ggtitle("Severity of Tweets")

# percent of severe and common tweets
n_tot = sum(by.tweet$count[1:52])
n_sev = sum(by.tweet$count[which(by.tweet$tweet[1:52] == "severe")])
n_com = sum(by.tweet$count[which(by.tweet$tweet[1:52] == "common")])
per_sev = n_sev/n_tot*100
per_com = n_com/n_tot*100

######################################
# function to clean the text
clean_tweets = function(text){
    # loading required packages
    lapply(c("tm", "Rstem", "stringr"), require, c=T, q=T)
    # avoid encoding issues by dropping non-unicode characters
    #utf8text = iconv(text, to='UTF-8-MAC', sub = "byte")
    # remove punctuation and convert to lower case
    words = gsub('[[:punct:]]', "", text)
    words = tolower(words)
    # spliting in words
    words = str_split(words, " ")
    return(words)
}

# now we clean the text
tweets$text[1]
tweets$text[7]

text = clean_tweets(tweets$text)
text[[1]]
text[[7]]

# a function to classify individual tweets
classify = function(words, sev.words, comm.words){
    # count number of positive and negative word matches
    sev.matches = sum(words %in% sev.words)
    comm.matches = sum(words %in% comm.words)
    return(sev.matches - comm.matches)
}

# this is how we would apply it
classify(text[[1]], sev, com)
classify(text[[7]], sev, com)

# but we want to aggregate over many tweets...
classifier = function(text, sev.words, comm.words){
    # classifier
    scores = unlist(lapply(text, classify, sev.words, comm.words))
    n = length(scores)
    severe = as.integer(length(which(scores>0))/n*100)
    common = as.integer(length(which(scores<0))/n*100)
    neutral = 100 - severe - common
    cat(n, "tweets:", severe, "% severe,",
        common, "% common,", neutral, "% neutral")
}

# applying classifier function
classifier(text, sev, com)

###############################################
### MISC ANALYSIS & PLOTS             			###
###############################################

# Merge followersN and tweets Freq into one data frame by user screenName
tweets.freq = as.data.frame(table(tweets.All$screenName))
colnames(tweets.freq) = c("screenName", "N")
user.followersN$freq = tweets.freq$N[match(row.names(user.followersN), tweets.freq$screenName)]


### BAR PLOT OF FREQUENCY MEAN BASED ON FOLLOWERS COUNT RANGES ###
data = read.csv("Data/followersCount.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# Split data into groups of followers count ranges
data.grouped = split(data, cut(data$N, 
                               c(0, 500, 1000, 5000, 10000, 20000, 30000, 40000, 50000, 60000), 
                               include.lowest=TRUE))
followers.mean = data.frame(followers = as.numeric(), mean = as.numeric(), stringsAsFactors = FALSE)

# Compute mean for each followers count range
for(i in 1:length(data.grouped)) {
  followers.mean[i,1] = names(data.grouped[i])
  followers.mean[i,2] = mean(sapply(data.grouped[i], '[[', 3))
}

followers.mean$followers = factor(followers.mean$followers, order = TRUE, 
                                  levels = followers.mean$followers)
levels(followers.mean$followers) = c("0-500", "501-1000", "1001-5000", "5001-10000", 
                                     "10001-20000", "20001-30000", "30001-40000", 
                                     "40001-50000", "50001-60000")
ggplot(followers.mean, aes(x=followers, y=mean)) + geom_bar(stat="identity")

### CHI SQUARE TEST BETWEEN NUMBER OF FOLLOWERS VS TWEETS FREQ ###
tbl = table(data$N, data$freq)
chisq.test(tbl)
