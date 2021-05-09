######################################
# -------- Data Processing -----------
######################################

#Load libraries
library("httr")
library("jsonlite")
library("tidyverse")
library("twitteR")
library("rtweet")
library("plyr")
library("dplyr")
library("data.table")
library("tidytext")


#############################
# ------- Text Mining Preparations
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

