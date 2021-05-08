######################################
# -------- Data Processing -----------
######################################

# Load objects created in "collect_data.R" file
load("v.obrador.followers.RData")
load("v.pinera.followers.RData")
load("v.lenin.follwers.RData")
load("v.bukele.followers.RData")


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

## ----Get a loop to get statuses of all the cleaned followers

# Create a vector with filtered followers id's
v.filtered.followers.ids <- as.vector(as.numeric(obrador.followers.cleaned$user_id))

# Get columns names of what "get_timeline" function extracts and create an empty data table
df.followers.tm <- data.frame(matrix(ncol = 90, nrow = 0))
my.col.names <- colnames(timeline.obrador)
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

#It works til here !!!!

statuses_of_followers <- lookup_statuses(v.followers.ids)

#rm(test.timeline.user, dt.followers.responses.to.target)
#test.timeline.user <- as.data.table(get_timeline(follower_id, n=2))
#test.timeline.user <- test.timeline.user[, list(user_id, text, reply_to_user_id)]

#dt.followers.responses.to.target <- rbind(dt.followers.responses.to.target, test.timeline.user)

obrador.followers.cleaned <- targetfollowers.filtered
df.obrador.followers.tm <- df.followers.tm

save(obrador.followers.cleaned, file ="obrador-followers-cleaned.RData")
save(df.obrador.followers.tm, file = "df-obrador-followers-tm.RData")
