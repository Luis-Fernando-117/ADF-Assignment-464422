# Twitter Data Collection - Luis Repository
library("httr")
library("jsonlite")
library("tidyverse")
library("twitteR")
library("rtweet")

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

# --------- SECTION 2 : Collect Data  ------------

################################################

#### TEST DATASET FOR STEP 3 ###
# just 100 observations but that are representative of the tweets I'll analyze

# --- Preparations for Data extraction ---

# Set up app and keys
  api_key <-      Sys.getenv("API_KEY")
  api_secret <-   Sys.getenv("API_SECRET")
  token <-        Sys.getenv("TOKEN")
  token_secret <- Sys.getenv("TOKEN_SECRET")

# Twitter authentication using the twitteR package function
  setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Sets up twitter authentication using the rtweet package function
  token <- create_token(app = "dfcp", 
                      consumer_key = api_key,
                      consumer_secret = api_secret,
                      access_token = token,
                      access_secret = token_secret)
  identical(token, get_token())


# start with a small test to make sure everything works fine
# this should use a search string that is very likely to return results
  

  
# ----------- Preliminary data extraction -------------
  
# The end goal is to extract all the replies to  tweets from political leaders 
# and do text mining on these replies

# As a first MVP lets start with a single account of a president, this time from
# Mexico. 
  
# Step 1: Get user id, by making use of the handle - Copy paste from Public Repo

# In the data processing we need tweets that replied to tweets of the political 
# leader, so replies to original tweets coming from the user id of the political leader
  
  # check the "type_of_tweet_extension"
  
  # Think of the stoping criteria , how many tweets are enough
  
  
  ################################################
  # Step 1: collect the user_id for this handle
  ################################################
  handle <- '@lopezobrador_'
  url_handle <- paste0('https://api.twitter.com/2/users/by?usernames=', handle)
  response <-	httr::GET(url = url_handle,
                        config = httr::add_headers(.headers = my_header[["headers"]]))
  # always check the HTTP response before doing anything else
  httr::status_code(response)
  # if 200 (Success), then continue.
  # else, fix the issues first
  
  # convert the output to text and then to a data frame
  obj <- httr::content(response, as = "text")
  df_obj <- jsonlite::fromJSON(obj, flatten = TRUE) %>% as.data.frame
  print(df_obj)
  # data.id is the user_id I need
  user_id <- df_obj[["data.id"]]
  
  ################################################
  # Step 2: get tweets for this user_id
  ################################################
  
  url_handle <- paste0('https://api.twitter.com/2/users/', user_id, "/tweets")
  # by default, the number of tweets retrieved per request is 10
  # you can ask for more tweets (check the documentation for exact info)
  params <- list(max_results = '50')
  response <-	httr::GET(url = url_handle,
                        config = httr::add_headers(.headers = my_header[["headers"]]),
                        query = params)
  httr::status_code(response)
  obj <- httr::content(response, as = "text")
  df_obj <- jsonlite::fromJSON(obj, flatten = TRUE) %>% as.data.frame
  tweets_1_to_50 <- df_obj
  
  # there are more tweets to collect, so we need to use the pagination token 
  # to make sure that you're doing this correctly and are not missing any tweets, 
  #		you can try this:
  #		collect tweets 1-25
  #		use the pagination token to collect tweets 26-50
  #		compare the results against tweets_1_to_50
  
  #		collect tweets 1-25
  params <- list(max_results = '25')
  response <-	httr::GET(url = url_handle,
                        config = httr::add_headers(.headers = my_header[["headers"]]),
                        query = params)
  httr::status_code(response)
  obj <- httr::content(response, as = "text")
  df_obj <- jsonlite::fromJSON(obj, flatten = TRUE) %>% as.data.frame
  tweets_1_to_25 <- df_obj
  
  #		use the pagination token to collect tweets 26-50
  next_token <- distinct(tweets_1_to_25 %>% select(meta.next_token))
  next_token
  
  params <- list(max_results = '25',
                 pagination_token = next_token$meta.next_token)
  response <-	httr::GET(url = url_handle,
                        config = httr::add_headers(.headers = my_header[["headers"]]),
                        query = params)
  httr::status_code(response)
  obj <- httr::content(response, as = "text")
  df_obj <- jsonlite::fromJSON(obj, flatten = TRUE) %>% as.data.frame
  tweets_26_to_50 <- df_obj
  
  #		compare the results against tweets_1_to_50
  tweets_two_steps <- rbind(tweets_1_to_25, 
                            tweets_26_to_50 %>% select(-meta.previous_token))
  # are any tweet ids in the initial df of 50 that are not available in the two step procedure?
  anti_join(tweets_1_to_50, tweets_two_steps, by = c("data.id", "data.text"))
  # are any tweet ids in the two step procedure that are not available in the initial df of 50?
  anti_join(tweets_two_steps, tweets_1_to_50, by = c("data.id", "data.text"))
  
  remove(df_obj, next_token, params, response, tweets_1_to_25, tweets_1_to_50,
         tweets_26_to_50, tweets_two_steps, handle, obj, url_handle)
  
  ################################################
  # Step 3: get ALL tweets for this user_id
  ################################################
  
  # get the first batch
  url_handle <- paste0('https://api.twitter.com/2/users/', user_id, "/tweets")
  n_tweets_per_request <- '50'
  params <- list(max_results = n_tweets_per_request)
  response <-	httr::GET(url = url_handle,
                        config = httr::add_headers(.headers = my_header[["headers"]]),
                        query = params)
  httr::status_code(response)
  obj <- httr::content(response, as = "text")
  df_obj <- jsonlite::fromJSON(obj, flatten = TRUE) %>% as.data.frame
  
  ALL_tweets <- df_obj %>% select(data.id, data.text)
  
  # as long as there are more tweets to collect, meta.next_token has a value
  # otherwise, if meta.next_token is null, this means you've collected all
  # tweets from this user
  while(!is.null(df_obj[["meta.next_token"]])) {
    # this is where I left
    next_token <- distinct(df_obj %>% select(meta.next_token))
    
    params <- list(max_results = n_tweets_per_request,
                   pagination_token = next_token$meta.next_token)
    response <-	httr::GET(url = url_handle,
                          config = httr::add_headers(.headers = my_header[["headers"]]),
                          query = params)
    httr::status_code(response)
    obj <- httr::content(response, as = "text")
    df_obj <- jsonlite::fromJSON(obj, flatten = TRUE) %>% as.data.frame
    ALL_tweets <- rbind(ALL_tweets, df_obj %>% select(data.id, data.text))
  }
  
  
  ################################################
  # Step 4: add twitter fields and expansions
  ################################################
  
  remove(df_obj, next_token, params, response, 
         obj, n_tweets_per_request)
  
  # I choose 7 because based on the tweets I have on my page, this will include:
  #		replies to tweets written by other people
  #		quoted retweets
  #		retweets
  #		original tweets (that I wrote without replying to someone else)
  params <- list(max_results = '7',
                 tweet.fields = 'created_at,author_id,conversation_id',
                 expansions = 'referenced_tweets.id')
  # referenced_tweets.id will return a Tweet object that the focal Tweet is referencing
  # focal Tweet = the tweet that includes the target_hashtag
  
  response <-	httr::GET(url = url_handle,
                        config = httr::add_headers(.headers = my_header[["headers"]]),
                        query = params)
  # always check the HTTP response before doing anything else
  httr::status_code(response)
  # if 200 (Success), then continue.
  # else, fix the issues first
  # convert the output
  obj <- httr::content(response)
  names(obj)
  
  # check obj -> this is a nested list now, so you need to take additional steps to process it
  table(sapply(obj[["data"]], length))
  table(sapply(obj[["includes"]], length))
  
  # always check your data!
  # this is a tweet that I wrote in reply to a tweet written by another user
  obj[["data"]][[1]]
  # the author_id is my own ID (Ana_Martinovici), so the ID of the user who wrote the "focal tweet"
  obj[["data"]][[1]][["author_id"]]
  user_id
  # this gives info about the id of the tweet I've replied to
  obj[["data"]][[1]][["referenced_tweets"]]
  # notice that the id of the referenced_tweet matches the conversation id
  obj[["data"]][[1]][["referenced_tweets"]][[1]][["id"]]
  obj[["data"]][[1]][["conversation_id"]]
  
  # rearrange the data
  df_data <- obj[["data"]] %>% 
    {tibble(created_at      = map_chr(., "created_at"),
            text            = map_chr(., "text"),
            tweet_id        = map_chr(., "id"),
            author_id       = map_chr(., "author_id"),
            conversation_id = map_chr(., "conversation_id"),
            ref_tweet       = map(., "referenced_tweets"))}
  # check: https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html#data_frame_output
  # for info about what the {} around tibble do
  # you can also try it out without {} and see how df_data differs
  
  
  # some tweets are retweets or quoted, others are neither (ref_tweet is null)
  f_get_tweet_type <- function(input_list) {
    if(is.null(input_list)) {
      # you can change the label to use for a tweet that is neither a quote or a retweet
      return("original_tweet")
    } else {
      return(input_list[[1]][["type"]])	
    }
  }
  
  f_get_ref_tweet_id <- function(input_list) {
    if(is.null(input_list)) {
      # you can change the label to use for a tweet that is neither a quote or a retweet
      return("original_tweet")
    } else {
      return(input_list[[1]][["id"]])	
    }
  }
  
  # for those tweets that are retweets or quoted, extract their type and id
  df_data <- df_data %>%
    mutate(tweet_type          = map_chr(ref_tweet, f_get_tweet_type),
           referenced_tweet_id = map_chr(ref_tweet, f_get_ref_tweet_id))
  
  # now that you've extracted all info from ref_tweet, you can remove it
  df_data <- df_data %>% select(-ref_tweet)
  # how many tweets if each type do I have?
  table(df_data[["tweet_type"]])
  
  # includes contains info about those tweets that are not "original"
  #		that is, tweets that I've replied to or retweeted
  length(obj[["includes"]])
  length(obj[["includes"]][["tweets"]])
  # for example, this is the tweet written by Robert Rooderkerk that I have replied to
  obj[["includes"]][["tweets"]][[1]]
  # this is a tweet that I have retweeted and that included a quote of another tweet
  obj[["includes"]][["tweets"]][[3]]
  
  ################################################
  # Step 5: add even more twitter fields and expansions
  ################################################
  remove(df_data, obj, response)
  
  # I choose 7 because based on the tweets I have on my page, this will include:
  #		replies to tweets writen by other people
  #		quoted retweets
  #		retweets
  #		original tweets (that I wrote without replying to someone else)
  params <- list(max_results = '7',
                 tweet.fields = 'created_at,author_id,conversation_id',
                 expansions = 'referenced_tweets.id,in_reply_to_user_id')
  # referenced_tweets.id will return a Tweet object that the focal Tweet is referencing
  # in_reply_to_user_id will return a user object representing the Tweet author this requested Tweet is a reply of
  # focal Tweet = the tweet that includes the target_hashtag
  
  response <-	httr::GET(url = url_handle,
                        config = httr::add_headers(.headers = my_header[["headers"]]),
                        query = params)
  # always check the HTTP response before doing anything else
  httr::status_code(response)
  # if 200 (Success), then continue.
  # else, fix the issues first
  # convert the output
  obj <- httr::content(response)
  names(obj)
  
  # check obj -> this is a nested list now, so you need to take additional steps to process it
  table(sapply(obj[["data"]], length))
  table(sapply(obj[["includes"]], length))
  
  # always check your data!
  # this is a tweet that I wrote in reply to a tweet written by another user
  obj[["data"]][[1]]
  # the author_id is my own ID (Ana_Martinovici), so the ID of the user who wrote the "focal tweet"
  obj[["data"]][[1]][["author_id"]]
  user_id
  # this gives info about the id of the tweet I've replied to
  obj[["data"]][[1]][["referenced_tweets"]]
  # notice that the id of the referenced_tweet matches the conversation id
  obj[["data"]][[1]][["referenced_tweets"]][[1]][["id"]]
  obj[["data"]][[1]][["conversation_id"]]
  
  # rearrange the data
  df_data <- obj[["data"]] %>% 
    tibble(created_at       = map_chr(., "created_at"),
           text             = map_chr(., "text"),
           tweet_id         = map_chr(., "id"),
           author_id        = map_chr(., "author_id"),
           conversation_id  = map_chr(., "conversation_id"),
           l_rpl_to_user_id = map(., "in_reply_to_user_id"),
           ref_tweet        = map(., "referenced_tweets"))
  
  # some tweets are retweets or quoted, others are neither (ref_tweet is null)
  f_get_tweet_type <- function(input_list) {
    if(is.null(input_list)) {
      # you can change the label to use for a tweet that is neither a quote or a retweet
      return("original_tweet")
    } else {
      return(input_list[[1]][["type"]])	
    }
  }
  
  f_get_ref_tweet_id <- function(input_list) {
    if(is.null(input_list)) {
      # you can change the label to use for a tweet that is neither a quote or a retweet
      return("original_tweet")
    } else {
      return(input_list[[1]][["id"]])	
    }
  }
  
  f_get_reply_to_user_id <- function(input_list) {
    if(is.null(input_list)) {
      # you can change the label to use for a tweet that is neither a quote or a retweet
      return("original_tweet")
    } else {
      return(input_list[[1]])	
    }
  }
  
  # for those tweets that are retweets or quoted, extract their type and id
  df_data <- df_data %>%
    mutate(tweet_type          = map_chr(ref_tweet, f_get_tweet_type),
           referenced_tweet_id = map_chr(ref_tweet, f_get_ref_tweet_id),
           reply_to_user_id    = map_chr(l_rpl_to_user_id, f_get_reply_to_user_id))
  
  # now that you've extracted all info from ref_tweet, you can remove it
  df_data <- df_data %>% select(-ref_tweet, -l_rpl_to_user_id)
  # how many tweets if each type do I have?
  table(df_data[["tweet_type"]])
  
  # includes contains info about those tweets that are not "original"
  #		that is, tweets that I've replied to or retweeted
  length(obj[["includes"]])
  length(obj[["includes"]][["tweets"]])
  # for example, this is the tweet written by Robert Rooderkerk that I have replied to
  obj[["includes"]][["tweets"]][[1]]
  # this is a tweet that I have retweeted and that included a quote of another tweet
  obj[["includes"]][["tweets"]][[3]]
  
  
  
  
  
  
  
# ---- My Test ----------- 
# Amount of tweets to extract
  ntweets_to_extract <- 100

# Define language
  language <- "es"
  
# Specify dates. Note the API includes only info for the past 6-9 days
  date_since <- "2021-04-01"
  date_until <- "2021-04-07"

# Coordinates
# Rotteram coordinates 51.9244° N, 4.4777° E
  rdam_geocode <- '51.9244,4.4777,10km'
# Geocode Mexico City
  mexicocity_geocode <- "19.432608,-99.133208,30km"

# Tweet Search
  l.tweets <- searchTwitter("corrupcion", n = ntweets_to_extract, 
                            lang = language, geocode = mexicocity_geocode)
  df.tweets <- twListToDF(l.tweets)
  
# Save tweets
  save(df.tweets, file = "test-tweets-step3.RData")

  # -------- Extracting tweets: Ana examples -------
  
  # -- Using "searchTwitter" from twitteR package
  
  l.mytest <- searchTwitter("pizza", n = 5, lang = "en")
  # We can use this function "twListToDF" to convert the list into a df.
  df.mytest <- twListToDF(l.mytest)
  
  # -- Using "search_tweets" from rtweet package
  
  l.mytest.rtweet <- search_tweets("", n = 100, lang = "en")
  # When using this package you can just use as.data.frame to convert the list to df
  df.test.rtweet <- as.data.frame(test)
  
  # -- looking into tweets about Ajax
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
  rt <- search_tweets("#Ajax", n = 250, retryonratelimit = TRUE)
  

