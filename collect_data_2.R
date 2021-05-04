
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

### See how much followers of a political leader interact with his/her tweets

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


#Save important things
#save(tmls, file = "timeline-obrador-50.RData")
#save(targetfollowers.50, file ="obrador-followers-cleaned.RData")
#save(targettwitteruserid, file = "user_id_obrador.RData")

load("timeline-obrador-50.RData")
load("obrador-followers-cleaned.RData")
load("user_id_obrador.RData")


##custom function to search all followers timelines one by one
getfollowersreplies <- function(x){
  follower <- as.numeric(x[1])
  followertl <- data.frame(get_timeline(follower, n=2, retryonratelimit=TRUE))
  followertl <- filter(followertl, in_reply_to_status_user_id == targettwitteruserid)
  followertl <- transform(followertl, reply_to_status_id_num=as.numeric(in_reply_to_status_status_id))
  join <- inner_join(followertl, tweetids, by=c("reply_to_status_id_num"="status_id_num"))
  replycounts <- data.frame(
    join %>%
      group_by(user_id, reply_to_status_id_num) %>%
      summarise(n=n())
  )
  return(replycounts)
}


tweet_replies_test <- do.call("rbind", lapply(targetfollowers.50$user_id, getfollowersreplies))

## Get a loop to get statuses of all the followers

  v.followers.ids <- as.vector(as.numeric(targetfollowers.50$user_id))
#l.followers.responses.to.target <- as.list(NULL)

#Get columns names of what "get_timeline" function extracts and
# create an empty data table
  df.followers.responses.to.target <- data.frame(matrix(ncol = 90, nrow = 0))
  my.col.names <- colnames(tmls)
  colnames(df.followers.responses.to.target) <- my.col.names
  
# Create a loop that gets some tweets of the followers fo the target user
for (i in v.followers.ids) {
  follower_id <- as.numeric(i)
  df.follower_tm <- as.data.frame((get_timeline(follower_id, n=2)))
  df.followers.responses.to.target <- rbind(df.followers.responses.to.target, df.follower_tm)
}

#It works til here !!!!
  
statuses_of_followers <- lookup_statuses(v.followers.ids)

#rm(test.timeline.user, dt.followers.responses.to.target)
#test.timeline.user <- as.data.table(get_timeline(follower_id, n=2))
#test.timeline.user <- test.timeline.user[, list(user_id, text, reply_to_user_id)]

#dt.followers.responses.to.target <- rbind(dt.followers.responses.to.target, test.timeline.user)

########################################
# Data Analyis part - to be in Rmd file
#####################################


# Get some features out of the tmls data frame

average_text_with <- mean(tmls$display_text_width)
average_retweet_count <- mean(tmls$retweet_count)

