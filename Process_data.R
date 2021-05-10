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


# Get a test df, to explore text mining, with a reduced sample
obrador.followers.tms.test <- slice(df.followers.tm.obrador, 1:50)

# Put tweets into an character object
text_tweets_obrador <- c(obrador.followers.tms.test$text)

# Turn Tweets into tidy data sets
 
text_tweets_to_obrador <- tibble(line = 1:50, text = text_tweets_obrador)


# Create a tibble with the words and their count 
text_tweets_to_obrador %>%
  unnest_tokens(word, text)

