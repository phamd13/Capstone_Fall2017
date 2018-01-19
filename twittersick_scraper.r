###################################################################
## Twitter Sick Data Scraping
## Description: For Fall 2017 CMDA Capstone Project - Sick Tweets
## Managed by Daniel Pham
## Last Updated: 11/1/17
###################################################################
library(twitteR)
library(streamR)
library(ROAuth)
library(xlsx)
library(RCurl)

rm(list=ls())

setwd("~/VirginiaTechDocs/Class/Fall 2017/CMDA4864/TwitterScraper")

# Skip next section if already created token

#################################################################################################
#####################################
### CREATING YOUR OWN OAUTH TOKEN ###
#####################################

# Go to apps.twitter.com and create Twitter application

requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = " "
consumerSecret = " "
accessToken = " "
accessSecret = " "

my_oauth = OAuthFactory$new(consumerKey=consumerKey,
  consumerSecret=consumerSecret, requestURL=requestURL,
  accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## now you can save oauth token for use in future sessions with twitteR or streamR
# (uncomment to save)
#save(my_oauth, file="oauth_token.Rdata")

#################################################################################################

###########################################
#### START HERE ONCE OAUTH DATA SAVED   ###
###########################################
load("oauth_token.Rdata")

## setup twitter handshake
setup_twitter_oauth(my_oauth$consumerKey, my_oauth$consumerSecret,
	my_oauth$oauthKey, my_oauth$oauthSecret)

# Relevant keyword dictionary and phrases (excludes retweets)
rel.words = "\"cold sweats\" | \"high fever\" | \"nausea\" | \"migraine\" -filter:retweets"

############################################################################
## SEARCH TWITTER HISTORY
## Edit 'since' parameter to pull tweets after a certain date (NULL if ignore)
############################################################################
# extract tweets from Twitter history
tweets = searchTwitter(rel.words, n = 15000, since = "2017-11-01", lang = "en")
tweets = twListToDF(tweets)
############################################################################

# Remove duplicates
tweets = tweets[!duplicated(tweets$text), ]

# Subset of tweets that contain hyperlinks
tweets_http = tweets[grep("http[[:alnum:]]*", tweets$text), ]
tweets = tweets[!grepl("http[[:alnum:]]*", tweets$text), ]      # remove tweets containing web links from master list

# Write datasets to csv
write.csv(tweets, file = "Data/sicktweets_11-13.csv")
write.csv(tweets_http, file = "Data/sicktweets_http_11-13.csv")

# read csv datasets
read.csv("Data/sicktweets_##-#.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

###############################################
### COLLECTING TWEETS FILTERING BY LOCATION ###
###############################################
# (lat, long, radius) geocode
geo_USA = "39.8, -95.58, 1553mi"    # entire USA
geo_VA = "36.98,-78.99,326km"    # entire USA

tweets_geoloc = searchTwitter("hokies", n = 100, lang = "en", geocode = geo_USA)
tweets_geoloc = twListToDF(tweets_geoloc)

###############################################
### COLLECTING USER TWEETS, FOLLOWERS COUNT ###
###############################################
users = read.csv("Data/allNames.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
file.remove("Data/followersCount.csv")

user.followersN = data.frame(N = as.numeric(), stringsAsFactors = FALSE)

for(i in 6:nrow(users)) {
  # For Debugging: message(users[i,1])
  
  # check if user account still active
  validUser = url.exists(paste("https://twitter.com/", users[i,1], sep = ""))
  
  if(validUser == TRUE) {
    
    # Get user most recent 100 tweets from timeline
    #user.tweets = userTimeline(users[i,1], n = 100, excludeReplies = TRUE)
    #user.tweets = twListToDF(user.tweets)
    
    # Get user followers Count
    user.obj = getUser(users[i,1])
    user.followersN[i-5, 1] = user.obj$followersCount
    row.names(user.followersN)[i-5] = users[i,1]      # set row name to user screenName
  }
  else {
    message(paste("User:", users[i,1], "not found, skipped."))
    
    user.followersN[i-5, 1] = NA
    row.names(user.followersN)[i-5] = users[i,1]      # set row name to user screenName
    next
  }
  
  # write to csv with each user as separate sheet
 #write.xlsx(user.tweets, file="Data/test.xlsx", sheetName = users[i,1], append=TRUE, row.names=FALSE)
  
  # write followers count data to csv
  write.csv(user.followersN, file = "Data/followersCount.csv")
}

