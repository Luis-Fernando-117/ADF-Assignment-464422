#Load Packages

library("httr")
library("jsonlite")
library("tidyverse")
library("twitteR")
library("rtweet")
library("plyr")
library("dplyr")

################################################
# -------- Step 1: Test API connection --------
################################################

bearer_token <- Sys.getenv("BEARER_TOKEN")
if(is.null(bearer_token)) {
  cat("The bearer token is empty. Fix this before you continue!")
} else {
  cat("The bearer token has a value. Let's see if it's the correct value.\n")
  headers <- c(Authorization = paste0('Bearer ', bearer_token))
  source("f_aux_functions_Luis.R")
  f_test_API(use_header = headers)
  # put the headers in a list so it doesn't show up on screen
  my_header <- list(headers = headers)
  remove(headers)
  remove(bearer_token)
  remove(f_test_API)
}

################################################
# --------- SECTION 2 : Collect Data  ------------
################################################

##set name of tweeter to look at (this can be changed)
targettwittername <- "lopezobrador_"

##get this tweeter's timeline
tmls <- get_timeline(targettwittername, n=50, retryonratelimit=TRUE)

##get their user id
targettwitteruserid <- as.numeric(select(lookup_users(targettwittername), user_id))

##get ids of their tweets
tweetids <- select(tmls, status_id)
tweetids <- transform(tweetids, status_id_num=as.numeric(status_id))

##get list of followers (who are most likely to reply)
targetfollowers.50 <- data.frame(get_followers(targettwittername, n = 50))

v.obradorfollowers.50 <- as.vector(targetfollowers.50$user_id)

##clean up follower list to exclude those that have never tweeted and restricted access

user_lookup <- lookup_users(v.obradorfollowers.50)
users_with_tweets_and_unprotected <- filter(user_lookup, statuses_count != 0)
users_with_tweets_and_unprotected <- select(filter(users_with_tweets_and_unprotected, protected != "TRUE"), user_id)

targetfollowers.50 <- filter(targetfollowers, user_id %in% users_with_tweets_and_unprotected$user_id)




########################################
# Data Analyis part - to be in Rmd file
#####################################

# Get some features out of the tmls data frame

average_text_with <- mean(tmls$display_text_width)
average_retweet_count <- mean(tmls$retweet_count)


