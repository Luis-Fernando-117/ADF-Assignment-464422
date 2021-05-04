
# I have to push all the files to this repo to Ana can see them
#https://github.com/Digital-Footprints/dsrp_ia-Luis-Fernando-117

#Load Packages

library("httr")
library("jsonlite")
library("tidyverse")
library("twitteR")
library("rtweet")
library("plyr")
library("dplyr")
library("data.table")

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
load("timeline-obrador-50.RData")
load("obrador-followers-cleaned.RData")
load("user_id_obrador.RData")
load("obradorfollowers.RData")
load("df.obradorfollowers.RData")

### --------See how many of the followers of a political leader interact with his/her tweets
# Additionally we can see out of his followers, whats the proportion that has replied to him
# at least once, and analyze the overall sentiment of the replied tweets to that target user
# we could further split the tweets between those tweets that are replies to target users
# and normal tweets of followers and see if there is a difference in sentiment between 
# these two.

##set name of tweeter to look at (this can be changed)
targettwittername <- "lopezobrador_"

##get this tweeter's timeline
tmls <- get_timeline(targettwittername, n=50, retryonratelimit=TRUE)

# get their user id
  targettwitteruserid <- as.numeric(select(lookup_users(targettwittername), user_id))

# Get ids of their tweets
  tweetids <- select(tmls, status_id)
  tweetids <- transform(tweetids, status_id_num=as.numeric(status_id))

# Get list of followers (who are most likely to reply)
  targetfollowers <- data.frame(get_followers(targettwittername, n = 10000))

  v.obradorfollowers <- as.vector(targetfollowers$user_id)

# Clean up follower list to exclude those that have never tweeted and restricted access
  user_lookup <- lookup_users(v.obradorfollowers)
  users_with_tweets_and_unprotected <- filter(user_lookup, statuses_count != 0)
  users_with_tweets_and_unprotected <- select(filter(users_with_tweets_and_unprotected, protected != "TRUE"), user_id)

  targetfollowers.filtered <- filter(targetfollowers, user_id %in% users_with_tweets_and_unprotected$user_id)





## ----Get a loop to get statuses of all the followers

# Create a vector with filtered followers id's
  v.filtered.followers.ids <- as.vector(as.numeric(targetfollowers.filtered$user_id))

#Get columns names of what "get_timeline" function extracts and create an empty data table
  df.followers.tm <- data.frame(matrix(ncol = 90, nrow = 0))
  my.col.names <- colnames(tmls)
  colnames(df.followers.tm) <- my.col.names


# Create a loop that gets some tweets of the followers fo the target user
for (i in v.followers.ids) {
  follower_id <- as.numeric(i)
  df.follower_tm <- as.data.frame((get_timeline(follower_id, n=5)))
  df.followers.tm <- rbind(df.followers.tm, df.follower_tm)
}

# Filter the observations to include only those tweets that are replies to tweets 
# made by our target
  dt.followers.replies.to.target <- as.data.table(df.followers.tm)
  dt.followers.replies.to.target <- dt.followers.replies.to.target[reply_to_user_id == targettwitteruserid, ]
  
# It works til here !!!!
  
statuses_of_followers <- lookup_statuses(v.followers.ids)

#rm(test.timeline.user, dt.followers.responses.to.target)
#test.timeline.user <- as.data.table(get_timeline(follower_id, n=2))
#test.timeline.user <- test.timeline.user[, list(user_id, text, reply_to_user_id)]

#dt.followers.responses.to.target <- rbind(dt.followers.responses.to.target, test.timeline.user)



#Save important things
#save(v.obradorfollowers, file = "obradorfollowers.RData")
#save(targetfollowers, file = "df.obradorfollowers.RData")
#save(targetfollowers.filtered, file ="obrador-followers-cleaned.RData")
#save(targettwitteruserid, file = "user_id_obrador.RData")
#save(df.followers.tm, file = "df.followers.tm.RData")

########################################
# Data Analyis part - to be in Rmd file
#####################################


# Get some features out of the tmls data frame

average_text_with <- mean(tmls$display_text_width)
average_retweet_count <- mean(tmls$retweet_count)

