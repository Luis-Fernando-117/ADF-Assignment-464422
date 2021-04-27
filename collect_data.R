# Twitter Data Collection - Luis Repository
library("httr")
library("jsonlite")
library("tidyverse")

################################################

# -------- Step 1: Test API connection --------

################################################

# if you have correctly set your bearer token as an environment variable, 
# this retrieved the value of the token and assigns it to "bearer_token"
bearer_token <- Sys.getenv("BEARER_TOKEN")

# if you didn't manage to create the environment variable, then copy paste the 
# token below and comment out the line


# the authorization header is composed of the text Bearer + space + the token
headers <- c(Authorization = paste0('Bearer ', bearer_token))

# f_aux_functions.R is in the same directory as collect_data.R, which is the
# same as the working directory
# f_aux_functions.R contains two functions that you can use to test the token
# source("f_aux_functions.R") brings these in the current workspace 
source("f_aux_functions_Luis.R")
# you should now see f_test_API and f_test_token_API in the Environment pane
# type ?source in the console to learn more

f_test_token_API()
f_test_API(use_header = headers)

# if you want to use the test functions, you need to uncomment the two lines above
# you can also take a look at examples/example_collect_all_tweets_from_one_user.R 
#		for another way of testing if you can connect to the API

################################################
################################################
#
# Does the bearer token allow you to collect data?
# if "Yes" -> continue
# else -> fix the error(s)
#
################################################
################################################

################################################
################################################
#
# Collect the data you need for your project
#
################################################
################################################

# here you add all the code you use to collect data
#		and nothing but the code you use to collect data
# I recommend that at first, you collect a small number of observations (max 100)
# this way, you first check what data you get, 
#				figure out how to process the API result
# after you are sure you get all the types of data you need, and you know how 
# to process the results from the API, you can collect more observations

################################################
# Save the data in the assignment repository
################################################

# save the "raw" dataset -> commit -> push to your assignment repository 
# if the dataset is too large to be pushed to GitHub:
#			1. split it in smaller files that you commit -> push to GitHub OR
#			2. let me know and I’ll set up another way for you to share data


################################################

# --------- SECTION 2 : Analyses ------------

################################################

# ---  TEST DATASET FOR STEP 3 --
# just 100 observations but that are representative of the tweets I'll analyze

library(twitteR)
library(rtweet)

# you need to set up your own app and keys
# these keys should no longer work and are left here just as an example
api_key <- "4o0TVozr4Zv9FGTm3GBS05UHX" #in the quotes, put your API key 
api_secret <- "IYU2Mvz5qk3l2a0EShIjSFkN8xq7cTxUuAxqL4RMMMv6kSgu21" #in the quotes, put your API secret token 
token <- "3695770529-2iI42ynXl11nsSWuPMK1uiHpoFN6cf1wxbTkbmk" #in the quotes, put your token 
token_secret <- "zPgmnwsl0p1DaIl7Olo4lDkBcLscNyvBU1Ukd7GD2H2Rx" #in the quotes, put your token secret

# this sets up twitter authentification using the twitteR package function
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# this sets up twitter authentification using the rtweet package function
token <- create_token(app = "dfcp", 
                      consumer_key = api_key,
                      consumer_secret = api_secret,
                      access_token = token,
                      access_secret = token_secret)
identical(token, get_token())


# start with a small test to make sure everything works fine
# this should use a search string that is very likely to return results
# searchTwitter is part of the twitteR package
test <- searchTwitter("pizza", n = 100, lang = "en")
# transform to data frame
test_tweets <- twListToDF(test)

# test also the function provided by rtweet
test <- search_tweets("pizza", n = 100, lang = "en")
# remove the test objects if all went fine
remove(test, test_tweets)

# how many tweets do you want to extract?
ntweets_to_extract <- 18000

# any restrictions for dates or location? Keep in mind that the search API index includes only the past 6-9 days
date_since <- "2019-04-27"
date_until <- "2019-04-28"

# Rdam coordinates
# 51.9244° N, 4.4777° E
rdam_geocode <- '51.9244,4.4777,10km'

# this returns 0 results because the time interval is already too far back in time
tweets <- searchTwitter("#kingsday", n = ntweets_to_extract, lang = "en", since = date_since, until = date_until, geocode = rdam_geocode)

# looking into tweets about Ajax
# search_tweets already arranges data in a data.frame format
rt_ajax <- search_tweets("#AJAX", n = ntweets_to_extract, include_rts = TRUE)

# you can now check summarize the information in the tweets
# for example, by looking into the frequency of tweets with "#Ajax" over 3-hour intervals
library(ggplot2)
ts_plot(rt_ajax, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #AJAX Twitter statuses",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


# the limit is set at 18000 tweets per 15 minutes, but if you need more than that, you can use "retryonratelimit = TRUE"
rt <- search_tweets("#Ajax", n = 250000, retryonratelimit = TRUE)


######