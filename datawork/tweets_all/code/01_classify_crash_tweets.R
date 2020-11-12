# Classify Tweets

# Load Tweets ------------------------------------------------------------------
tweets_all <- readRDS(file.path(tweets_all_dir, "data", "raw_data", "tweets.Rds"))

#### Clean Tweets
tweets_all$text <- tweets_all$text %>% as.character()
Encoding(tweets_all$text) <- "latin1"

tweets_all$text <- tweets_all$text %>%
  str_to_lower %>%
  str_replace_all("\\br/about\\b", "round about") %>%
  str_replace_all("\\.", " ") %>%
  str_replace_all("via @[a-z_,A-Z_,0-9_]*", "") %>%
  str_replace_all("@[a-z_,A-Z_,0-9_]*", "") %>%
  str_replace_all(","," , ") %>% # Add space between commas (eg, "road,allsops") 
  str_replace_all("\n", "") %>%
  str_replace_all("~", "") %>%
  str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]*\\b", "") %>%
  str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]", "") %>%
  str_replace_all("\\b(http|https)://t.co\\b", "") %>%
  str_replace_all("\\b(http|https):", "") %>%
  str_replace_all("~more*", "") %>%
  str_replace_all("(RT|rt) @[a-z,A-Z,0-9, _]*:", "") %>%
  str_replace_all("^[0-9][0-9]\\:[0-9][0-9]", "") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("\\bamp\\b", "and") %>%
  str_squish

# Implement SVM Model ----------------------------------------------------------
#### Load Model
model <- readRDS(file.path(tweets_classif_dir,
                           "model",
                           "tweet_classification_best_model.Rds"))

#### Model Parameters
model_params <- readRDS(file.path(tweets_classif_dir,
                                  "model",
                                  "tweet_classification_best_model_parameters.Rds"))

# Dfm --------------------------------------------------------------------------
tweets_all <- tweets_all %>%
  dplyr::rename(tweet = text)
tweets_all$tweet <- tweets_all$tweet %>% as.character()

dfm <- prep_dfm(tweets_all, model_params)

tweets_all$crash_tweet_algorithm <- predict(model, newdata = dfm, type="class") %>% as.vector()

# Identify Potentially Crash Related Tweets ------------------------------------
# Identify all potentially crash related tweets. Useful to determine which tweets
# should be coded -- likely catches nearly all of crash tweets.

tweets_all$potentially_accident_related <- class_potnt_crash(tweets_all$tweet)

tweets_all$crash_tweet_algorithm[tweets_all$potentially_accident_related %in% F] <- F

# Export -----------------------------------------------------------------------
saveRDS(tweets_all, file.path(tweets_all_dir, "data", "processed_data", "tweets_classified.Rds"))



