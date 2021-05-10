######################################
# -------- Data Processing -----------
######################################

# --- Load libraries
library("httr")
library("jsonlite")
library("tidyverse")
library("twitteR")
library("rtweet")
library("plyr")
library("dplyr")
library("data.table")
library("tidytext")

# --- Load files
load("df.followers.tm.bukele.RData")
load("df.followers.tm.obrador.RData")
load("df.followers.tm.lenin.RData")
load("df.followers.tm.pinera.RData")
load("timeline.obrador.RData")
load("timeline.pinera.RData")
load("timeline.lenin.RData")
load("timeline.bukele.RData")

#################################
# --- Tweets processing 
################################

# Create a subset of the president's timelines with only columns with relevant info

data.obrador <- subset(timeline.obrador, select = c("text", "display_text_width",
                                                    "reply_to_status_id","reply_to_user_id",
                                                    "favorite_count", "retweet_count"))

data.pinera  <- subset(timeline.pinera, select = c("text", "display_text_width",
                                                    "reply_to_status_id","reply_to_user_id",
                                                    "favorite_count", "retweet_count"))

data.lenin   <- subset(timeline.lenin, select = c("text", "display_text_width",
                                                    "reply_to_status_id","reply_to_user_id",
                                                    "favorite_count", "retweet_count"))

data.bukele  <- subset(timeline.bukele, select = c("text", "display_text_width",
                                                    "reply_to_status_id","reply_to_user_id",
                                                    "favorite_count", "retweet_count"))

# From the collected tweets of followers, get only those in spanish and are not re-tweets
  df.followers.tm.obrador.es <- filter(df.followers.tm.obrador, 
                                       lang == "es" & is_retweet == "FALSE")
  df.followers.tm.pinera.es  <- filter(df.followers.tm.pinera,
                                       lang == "es" & is_retweet == "FALSE")
  df.followers.tm.lenin.es   <- filter(df.followers.tm.lenin,
                                       lang == "es" & is_retweet == "FALSE")
  df.followers.tm.bukele.es  <- filter(df.followers.tm.bukele,
                                       lang == "es" & is_retweet == "FALSE")


#############################
# -- Text Mining Preparations
#############################

# Put tweets into an character object
  text.tweets.obrador.followers <- c(df.followers.tm.obrador.es$text)
  text.tweets.pinera.followers  <- c(df.followers.tm.pinera.es$text)
  text.tweets.lenin.followers   <- c(df.followers.tm.lenin.es$text)
  text.tweets.bukele.followers  <- c(df.followers.tm.bukele.es$text)
  
# Turn Tweets into tidy data sets
  text.tweets.obrador.followers <- tibble(line = 1:2114, text = text.tweets.obrador.followers)
  text.tweets.pinera.followers  <- tibble(line = 1:1268, text = text.tweets.pinera.followers)
  text.tweets.lenin.followers   <- tibble(line = 1:1216, text = text.tweets.lenin.followers)
  text.tweets.bukele.followers  <- tibble(line = 1:1168, text = text.tweets.bukele.followers)
  

# Create a tibble with the words and their count 
  word.count.obrador.followers <- text.tweets.obrador.followers %>% unnest_tokens(word, text)
  word.count.pinera.followers  <- text.tweets.pinera.followers  %>% unnest_tokens(word, text)
  word.count.lenin.followers   <- text.tweets.lenin.followers   %>% unnest_tokens(word, text)
  word.count.bukele.followers  <- text.tweets.bukele.followers  %>% unnest_tokens(word, text)
  
