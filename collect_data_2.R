
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
# -------- Step 2: Set up apps and keys --------
################################################

api_key <-      Sys.getenv("API_KEY")
api_secret <-   Sys.getenv("API_SECRET")
token <-        Sys.getenv("TOKEN")
token_secret <- Sys.getenv("TOKEN_SECRET")

# Twitter authentication using the twitteR package function
setup_twitter_oauth(api_key, api_secret, token, token_secret)


################################################
# --------- SECTION 3 : Collect Data  ----------
################################################

### --------See how many of the followers of a political leader interact with his/her tweets
# Additionally we can see out of his followers, whats the proportion that has replied to him
# at least once, and analyze the overall sentiment of the replied tweets to that target user
# we could further split the tweets between those tweets that are replies to target users
# and normal tweets of followers and see if there is a difference in sentiment between 
# these two.


# Twitter names of targets
  targettwittername.obrador <- "lopezobrador_"
  targettwittername.pinera <- "sebastianpinera"
  targettwittername.lenin <- "Lenin"
  targettwittername.bukele <- "nayibbukele"
  
# Get information about the presidents
  l.info.obrador <- getUser(targettwittername.obrador)
  l.info.pinera <- getUser(targettwittername.pinera)
  l.info.lenin <- getUser(targettwittername.lenin)
  l.info.bukele <- getUser(targettwittername.bukele)
  
# get their user id
  id.obrador <- l.info.obrador$id
  id.pinera <- l.info.pinera$id
  id.lenin <- l.info.lenin$id
  id.bukele <- l.info.bukele$id
  
# Get followers count to see how 
  df.info.obrador <- str(user.info.obrador$toDataFrame())
  df.info.obrador <- as.data.table(df.info.obrador)
  
# Targets timelines
  timeline.obrador <- get_timeline(targettwittername.obrador, n=5, retryonratelimit=TRUE)
  timeline.pinera <- get_timeline(targettwittername.pinera, n=5, retryonratelimit=TRUE)
  timeline.lenin <- get_timeline(targettwittername.lenin, n=5, retryonratelimit=TRUE)
  timeline.bukele <- get_timeline(targettwittername.bukele, n=5, retryonratelimit=TRUE)
  
# get their user id
 
  
  obrador.userid <- as.numeric(select(lookup_users(targettwittername.obrador), user_id))
  pinera.userid <- as.numeric(select(lookup_users(targettwittername.pinera), user_id))
  lenin.userid <- as.numeric(select(lookup_users(targettwittername.lenin), user_id))
  bukele.userid <- as.numeric(select(lookup_users(targettwittername.bukele), user_id))
  
# Get ids of their tweets
  obrador.tweetids <-  as.numeric(timeline.obrador$status_id)
  fernandez.tweetsids <- as.numeric(timeline.fernandez$status_id)
  bukele.tweetsids <- as.numeric(timeline.bukele$status_id)
  
# Get a list of followers as vectors (who are most likely to reply)
  
  # Of Lopez Obrador
  v.obrador.followers <- as.vector(get_followers(targettwittername.obrador, n = 10))

  # Of Alberto Fernandez
  v.fernandez.followers <- as.vector(get_followers(targettwittername.fernandez, n = 10))

 # Of Nayib Bukele
  v.bukele.followers <- as.vector(get_followers(targettwittername.bukele, n = 10))



# Save important things
  save(v.obradorfollowers, file = "v.obradorfollowers.RData")
  save(df.obrador.followers, file = "df.obradorfollowers.RData")
  save(obrador.userid, file = "user_id_obrador.RData")
  save(timeline.obrador, file = "timeline-obrador.RData")



########################################
# Data Analyis part - to be in Rmd file
#####################################
load("obrador-followers-cleaned.RData")
load("df-obrador-followers-tm.RData")

# Get some features out of the tmls data frame

average_text_with <- mean(tmls$display_text_width)
average_retweet_count <- mean(tmls$retweet_count)

