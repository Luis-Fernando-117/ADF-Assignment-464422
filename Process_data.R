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
### --- Process tweets from followers  

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
  

  
### --- Process tweets from presidents accounts
  
text.tweets.obrador <- c(data.obrador$text)
text.tweets.pinera  <- c(data.pinera$text)
text.tweets.lenin   <- c(data.lenin$text)
text.tweets.bukele  <- c(data.bukele$text)
  
# -- Turning them into tidy data frames
text.tweets.obrador <- tibble(line = 1:3200, text = text.tweets.obrador)
text.tweets.pinera  <- tibble(line = 1:3197, text = text.tweets.pinera)
text.tweets.lenin   <- tibble(line = 1:3199, text = text.tweets.lenin)
text.tweets.bukele  <- tibble(line = 1:3194, text = text.tweets.bukele)


# Additional column adding the president the observations belong to
# For followers tweets
text.tweets.obrador.followers$doc_id <- "Obrador_followers"
text.tweets.pinera.followers$doc_id  <- "Pinera_followers"
text.tweets.lenin.followers$doc_id   <- "Lenin_followers"
text.tweets.bukele.followers$doc_id  <- "Bukele_followers"

# For presidents tweets 
text.tweets.obrador$doc_id <- "President of Mexico"
text.tweets.pinera$doc_id  <- "President of Chile"
text.tweets.lenin$doc_id   <- "President of Ecuador"
text.tweets.bukele$doc_id  <- "President of El Salvador"

# --- Convert all the tweets text to lowercase
# For followers
text.tweets.obrador.followers$text <- tolower(text.tweets.obrador.followers$text)
text.tweets.pinera.followers$text  <- tolower(text.tweets.pinera.followers$text)
text.tweets.lenin.followers$text   <- tolower(text.tweets.lenin.followers$text)
text.tweets.bukele.followers$text  <- tolower(text.tweets.bukele.followers$text)

# For presidents tweets
text.tweets.obrador$text <- tolower(text.tweets.obrador$text)
text.tweets.pinera$text  <- tolower(text.tweets.pinera$text)
text.tweets.lenin$text   <- tolower(text.tweets.lenin$text)
text.tweets.bukele$text  <- tolower(text.tweets.bukele$text)


# --- Calculate the length of each tweets

# For followers tweets 
text.tweets.obrador.followers$length <- 
  sapply(strsplit(text.tweets.obrador.followers$text, " "), length)

text.tweets.pinera.followers$length <- 
  sapply(strsplit(text.tweets.pinera.followers$text, " "), length)

text.tweets.lenin.followers$length <- 
  sapply(strsplit(text.tweets.lenin.followers$text, " "), length)

text.tweets.bukele.followers$length <- 
  sapply(strsplit(text.tweets.bukele.followers$text, " "), length)

# For presidents tweets 
text.tweets.obrador$length <- 
  sapply(strsplit(text.tweets.obrador$text, " "), length)

text.tweets.pinera$length <- 
  sapply(strsplit(text.tweets.pinera$text, " "), length)

text.tweets.lenin$length <- 
  sapply(strsplit(text.tweets.lenin$text, " "), length)

text.tweets.bukele$length <- 
  sapply(strsplit(text.tweets.bukele$text, " "), length)

# --- Merge presidents timelines into a single dataframe 
presidents.timelines <- rbind(timeline.obrador, timeline.lenin)
presidents.timelines <- rbind(presidents.timelines, timeline.pinera)
presidents.timelines <- rbind(presidents.timelines, timeline.bukele)

#########################
# Save Objetcs for Report 
#########################

# Save presidents timelines 
  save(presidents.timelines, file = "presidents.timelines.RData")

# Save the tidy df's with tweets from presidents 
  save(text.tweets.obrador, file = "text.tweets.obrador.RData")
  save(text.tweets.pinera,  file = "text.tweets.pinera.RData")
  save(text.tweets.lenin,   file = "text.tweets.lenin.RData")
  save(text.tweets.bukele,  file = "text.tweets.bukele.RData")

# Save the tidy dataframes with tweets
  save(text.tweets.obrador.followers, file = "text.tweets.obrador.followers.RData")
  save(text.tweets.pinera.followers,  file = "text.tweets.pinera.followers.RData")
  save(text.tweets.lenin.followers,   file = "text.tweets.lenin.followers.RData")
  save(text.tweets.bukele.followers,  file = "text.tweets.bukele.followers.RData")
