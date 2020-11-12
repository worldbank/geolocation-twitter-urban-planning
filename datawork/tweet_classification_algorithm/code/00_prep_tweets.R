# Tweet Classification: Classify Tweets as Crash Related or Not
# Prep Tweets

# Clean text of tweet and subset tweets for tweet classification

# Load Data --------------------------------------------------------------------
truth_data <- readRDS(file.path(tweets_truth_dir, "data", "raw_data", "tweets_truth.Rds"))

# Clean Tweet Text -------------------------------------------------------------
truth_data$tweet <- iconv(truth_data$tweet, "latin1", "ASCII", sub="")
truth_data$tweet <- truth_data$tweet %>%
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

# Restrict to Potentially Accident Related Tweets ------------------------------
truth_data <- truth_data[!is.na(truth_data$tweet),]
truth_data$potentially_accident_related <- class_potnt_crash(truth_data$tweet)

truth_data <- truth_data[truth_data$potentially_accident_related %in% T,]

# Export -----------------------------------------------------------------------
saveRDS(truth_data, file.path(tweets_classif_dir, 
                              "data",
                              "tweets_truth_for_classification.Rds"))



