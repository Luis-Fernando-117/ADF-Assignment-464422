
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
# --------- Step 3 : Collect Data  ----------
################################################

### --------See how many of the followers of a political leader interact with his/her tweets
# Additionally we can see out of his followers, whats the proportion that has replied to him
# at least once, and analyze the overall sentiment of the replied tweets to that target user
# we could further split the tweets between those tweets that are replies to target users
# and normal tweets of followers and see if there is a difference in sentiment between 
# these two.

# Twitter names of targets
  targettwittername.obrador <-  "lopezobrador_"
  targettwittername.pinera  <-  "sebastianpinera"
  targettwittername.lenin   <-  "Lenin"
  targettwittername.bukele  <-  "nayibbukele"
  
# Get information about the presidents
  l.info.obrador <- getUser(targettwittername.obrador)
  l.info.pinera  <- getUser(targettwittername.pinera)
  l.info.lenin   <- getUser(targettwittername.lenin)
  l.info.bukele  <- getUser(targettwittername.bukele)
  
# Get their user id
  id.obrador <- l.info.obrador$id
  id.pinera  <- l.info.pinera$id
  id.lenin   <- l.info.lenin$id
  id.bukele  <- l.info.bukele$id
  
# Get the id's into a dataframe
  presidents_ids <- data.frame(id.obrador,id.pinera,id.lenin,id.bukele)

# We will see the actual number of followers of these presidents to get 
# appropriate proportions on the number of tweets from their followers we will scrape

# Get followers count per president
  nr.followers.presidents <- data.frame(l.info.obrador$followersCount, 
                                        l.info.pinera$followersCount,
                                        l.info.lenin$followersCount,
                                        l.info.bukele$followersCount)
# Write better names of columns
  names(nr.followers.presidents)[1] <- "Mexico"
  names(nr.followers.presidents)[2] <- "Chile"
  names(nr.followers.presidents)[3] <- "Ecuador"
  names(nr.followers.presidents)[4] <- "El_Salvador"
  
# Calculate total sum of followers
  tot.followers <- (nr.followers.presidents$Mexico  + 
                    nr.followers.presidents$Chile   + 
                    nr.followers.presidents$Ecuador + 
                    nr.followers.presidents$El_Salvador)
  
# Calculate proportion of followers per president out of total
  prop_followers <- data.frame(round(nr.followers.presidents$Mexico/sum.followers,2),
                               round(nr.followers.presidents$Chile/sum.followers,2),
                               round(nr.followers.presidents$Ecuador/sum.followers,2),
                               round(nr.followers.presidents$El_Salvador/sum.followers,2))
 
  # Write better names of columns
  names(prop_followers)[1] <- "Mexico"
  names(prop_followers)[2] <- "Chile"
  names(prop_followers)[3] <- "Ecuador"
  names(prop_followers)[4] <- "El_Salvador"
  
# Presidents time lines
  timeline.obrador <- get_timeline(targettwittername.obrador, n=3200, retryonratelimit=TRUE)
  timeline.pinera  <- get_timeline(targettwittername.pinera,  n=3200, retryonratelimit=TRUE)
  timeline.lenin   <- get_timeline(targettwittername.lenin,   n=3200, retryonratelimit=TRUE)
  timeline.bukele  <- get_timeline(targettwittername.bukele,  n=3200, retryonratelimit=TRUE)
  
# Get id status of their tweets
  #obrador.tweetids <- as.numeric(timeline.obrador$status_id)
  #pinera.tweetsids <- as.numeric(timeline.pinera$status_id)
  #lenin.tweetsids  <- as.numeric(timeline.lenin$status_id)
  #bukele.tweetsids <- as.numeric(timeline.bukele$status_id)
  
  # Get a sample of 5,000 followers of the total followers of the presidents

# Get a vector with of their followers id's 
  v.obrador.followers <- as.vector(get_followers(targettwittername.obrador, n = 5000))
  v.pinera.followers  <- as.vector(get_followers(targettwittername.pinera, n = 5000))
  v.lenin.follwers    <- as.vector(get_followers(targettwittername.lenin, n = 5000))
  v.bukele.followers  <- as.vector(get_followers(targettwittername.bukele, n = 5000))


# Save president's time lines
  save(timeline.obrador, file = "timeline.obrador.RData")
  save(timeline.pinera,  file = "timeline.pinera.RData")
  save(timeline.lenin,   file = "timeline.lenin.RData")
  save(timeline.bukele,  file = "timeline.bukele.RData")
  
# ----------------------------------------------------
  
  # Combine all the followers ids of the respective presidents into a single data frame
  df.presidents.followers <- data.frame(v.obrador.followers, v.pinera.followers,
                                        v.lenin.follwers, v.bukele.followers)
  
  names(df.presidents.followers)[1] <- "Mexico"
  names(df.presidents.followers)[2] <- "Chile"
  names(df.presidents.followers)[3] <- "Ecuador"
  names(df.presidents.followers)[4] <- "El_Salvador"
  
  # ------ Lopez Obrador ------
  
  # Clean up follower lists to exclude those followers that have never tweeted and 
  # restricted access
  
  lst_clean_ids <- list()
  for (k in 1:ncol(df.presidents.followers)) {
    user_lookup <- lookup_users(df.presidents.followers[ ,k])
    users_with_tweets_and_unprotected <- filter(user_lookup, statuses_count != 0)
    users_with_tweets_and_unprotected <- select(filter(users_with_tweets_and_unprotected, 
                                                       protected != "TRUE"), user_id)
    lst_clean_ids[[k]] <- users_with_tweets_and_unprotected
  }
  
  followers.cleaned.obrador <- (lst_clean_ids[[1]])
  followers.cleaned.pinera  <- (lst_clean_ids[[2]])
  followers.cleaned.lenin   <- (lst_clean_ids[[3]])
  followers.cleaned.bukele  <- (lst_clean_ids[[4]])
  
## ---- Get a loop to get statuses of all the cleaned followers of all the presidents 
  
  # Let's first create some empty data frames which will be filled with 
  # the loops, with followers time lines information
  df.followers.tm.obrador <- data.frame(matrix(ncol = 90, nrow = 0))
  df.followers.tm.pinera <- data.frame(matrix(ncol = 90, nrow = 0))
  df.followers.tm.lenin <- data.frame(matrix(ncol = 90, nrow = 0))
  df.followers.tm.bukele <- data.frame(matrix(ncol = 90, nrow = 0))
  
# --- Create a loops that gets some tweets of the followers fo the target users
  
###### Lopez Obrador #####
# Given the max amout of tweets you can request per 15 minutes, we 
# split the follower id's into 4 chunks 
  
  followers.cleaned.obrador_1 <- slice(followers.cleaned.obrador, 1:600)
  followers.cleaned.obrador_2 <- slice(followers.cleaned.obrador, 601:1200)
  followers.cleaned.obrador_3 <- slice(followers.cleaned.obrador, 1201:1800)
  followers.cleaned.obrador_4 <- slice(followers.cleaned.obrador, 1801:2162)
  
  # 1st chunk
  for (i in followers.cleaned.obrador_1) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=25)))
    df.followers.tm.obrador <- rbind(df.followers.tm.obrador, df.follower.tm)
  }
  
  #2nd chunk
  for (i in followers.cleaned.obrador_2) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=25)))
    df.followers.tm.obrador <- rbind(df.followers.tm.obrador, df.follower.tm)
  }
  
  #3rd chunk
  for (i in followers.cleaned.obrador_3) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=30)))
    df.followers.tm.obrador <- rbind(df.followers.tm.obrador, df.follower.tm)
  }
  
  # Last chunk
  for (i in followers.cleaned.obrador_4) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=30)))
    df.followers.tm.obrador <- rbind(df.followers.tm.obrador, df.follower.tm)
  }
  
  # Save the followers timeline
  save(df.followers.tm.obrador, file = "df.followers.tm.obrador.RData")

  # Filter the observations to include only those tweets that are replies to tweets 
  # made by our target
  dt.followers.replies.to.obrador <- as.data.table(df.followers.tm.obrador)
  dt.followers.replies.to.obrador <- dt.followers.replies.to.obrador[reply_to_user_id == targettwittername.obrador, ]
  

  ###### ---- Pinera ---- #####
  
  followers.cleaned.pinera_1 <- slice(followers.cleaned.pinera, 1:1200)
  followers.cleaned.pinera_2 <- slice(followers.cleaned.pinera, 1201:2400)
  followers.cleaned.pinera_3 <- slice(followers.cleaned.pinera, 2401:3010)
  
  # 1st chunk
  for (i in followers.cleaned.pinera_1) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
    df.followers.tm.pinera <- rbind(df.followers.tm.pinera, df.follower.tm)
  }
  
  #2nd chunk
  for (i in followers.cleaned.pinera_2) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
    df.followers.tm.pinera <- rbind(df.followers.tm.pinera, df.follower.tm)
  }
  
  #3d chunk
  for (i in followers.cleaned.pinera_3) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
    df.followers.tm.pinera <- rbind(df.followers.tm.pinera, df.follower.tm)
  }
  
  # Save the followers timeline
  save(df.followers.tm.pinera, file = "df.followers.tm.pinera.RData")
  
  ###### ---- Lenin ---- #####
  
  followers.cleaned.lenin_1 <- slice(followers.cleaned.lenin, 1:1200)
  followers.cleaned.lenin_2 <- slice(followers.cleaned.lenin, 1201:2400)
  followers.cleaned.lenin_3 <- slice(followers.cleaned.lenin, 2401:2701)
  
  # 1st chunk
  for (i in followers.cleaned.lenin_1) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
    df.followers.tm.lenin <- rbind(df.followers.tm.lenin, df.follower.tm)
  }
  
  #2nd chunk
  for (i in followers.cleaned.lenin_2) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
    df.followers.tm.lenin <- rbind(df.followers.tm.lenin, df.follower.tm)
  }
  
  #3d chunk
 for (i in followers.cleaned.lenin_3) {
    follower_id <- as.numeric(i)
    df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
    df.followers.tm.lenin <- rbind(df.followers.tm.lenin, df.follower.tm)
  }
  
  # Save the followers timeline
  save(df.followers.tm.lenin, file = "df.followers.tm.lenin.RData")
  
  
###### ---- Bukele ---- #####
  
followers.cleaned.bukele_1 <- slice(followers.cleaned.bukele, 1:900)
followers.cleaned.bukele_2 <- slice(followers.cleaned.bukele, 901:1800)
followers.cleaned.bukele_3 <- slice(followers.cleaned.bukele, 1801:2155)

  
# 1st chunk
for (i in followers.cleaned.bukele_1) {
  follower_id <- as.numeric(i)
  df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
  df.followers.tm.bukele <- rbind(df.followers.tm.bukele, df.follower.tm)
}

# 2nd chunk
for (i in followers.cleaned.bukele_2) {
  follower_id <- as.numeric(i)
  df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
  df.followers.tm.bukele <- rbind(df.followers.tm.bukele, df.follower.tm)
}

# 3rd chunk
for (i in followers.cleaned.bukele_3) {
  follower_id <- as.numeric(i)
  df.follower.tm <- as.data.frame((get_timeline(follower_id, n=15)))
  df.followers.tm.bukele <- rbind(df.followers.tm.bukele, df.follower.tm)
}

# Save the followers timeline
save(df.followers.tm.bukele, file = "df.followers.tm.bukele.RData")




