# Patterns of words/phrases that come before location word

# Load Data --------------------------------------------------------------------
# Restrict to observations included in the truth dataset and where we know how the
# landmark used to geocode the tweet was written in the tweet.
truth_data_all <- readRDS(file.path(tweets_truth_dir, "data", "raw_data", "tweets_truth.Rds"))

truth_data_alg <- truth_data_all %>%
  filter(!is.na(crash_landmark),
         accident_truth %in% T)

# Prep Data --------------------------------------------------------------------
# The "crash_landmark" variable contains the landmark used to geocode the tweet
# that is written the same way as in the tweet. We replace the landmark (word or
# phrase) in the tweet with "landmark_here", where we then examine words that come
# before "landmark_here"
truth_data_alg$tweet <- truth_data_alg$tweet %>% 
  str_replace_all(truth_data_alg$crash_landmark, "landmark_here") %>%
  str_replace_all("\\bthe\\b", " ") %>%
  str_squish()

# Function ---------------------------------------------------------------------
text_before_landmark_df <- lapply(1:nrow(truth_data_alg), function(i){
  # Create a dataset at the tweet level that indicates:
  # (1) 1-grams, 2-grams and 3-grams before landmark word
  # (2) 1-grams, 2-grams and 3-grams two words before landmark word
  # Additional, it creates variables indicating whether the ngrams occur
  # somewhere else in the tweet besides infront on of the landmark. Here, 
  # we count the number of times the ngram occurs -- if it occurs more than
  # once we know it occured somewhere else.
  
  if((i %% 500) == 0) print(i)
  
  #### Restrict to tweet i
  truth_data_alg_i <- truth_data_alg[i,]
  
  #### Text Original
  # We need the original tweet text to determine whether the ngram occurs more than
  # just in front of the ngram. For this, we look both before and after the landmark 
  # word.
  #text_original <- truth_data_alg_i$tweet
  
  #### Text of tweet of just words that come before landmark and break into individual words. 
  tweet_words_before_landmark <- truth_data_alg_i$tweet %>% 
    str_replace_all("landmark_here.*", "") %>% 
    str_squish() %>%
    strsplit(" ") %>%
    unlist() %>%
    
    # Reversing of order of words will make it easier to grab words and phrase. 
    # Because of this, when grabbing ngram will need to reverse again to get
    # orginal text.
    rev()
  
  #### Extract grams
  ngram1_1beforelandmark <- tweet_words_before_landmark[1]
  ngram1_2beforelandmark <- tweet_words_before_landmark[2]
  ngram1_3beforelandmark <- tweet_words_before_landmark[3]
  
  ngram2_1beforelandmark <- tweet_words_before_landmark[1:2] %>% rev() %>% paste(collapse=" ")
  ngram2_2beforelandmark <- tweet_words_before_landmark[2:3] %>% rev() %>% paste(collapse=" ")
  ngram2_3beforelandmark <- tweet_words_before_landmark[3:4] %>% rev() %>% paste(collapse=" ")
  
  #### Determine whether gram appears elsewhere in tweet
  ngram1_1beforelandmark_N <- str_count(truth_data_alg_i$tweet, paste0("\\b", ngram1_1beforelandmark, "\\b")) %>% na_if(0)
  ngram1_2beforelandmark_N <- str_count(truth_data_alg_i$tweet, paste0("\\b", ngram1_2beforelandmark, "\\b")) %>% na_if(0)
  ngram1_3beforelandmark_N <- str_count(truth_data_alg_i$tweet, paste0("\\b", ngram1_3beforelandmark, "\\b")) %>% na_if(0)
  
  ngram2_1beforelandmark_N <- str_count(truth_data_alg_i$tweet, paste0("\\b", ngram2_1beforelandmark, "\\b")) %>% na_if(0)
  ngram2_2beforelandmark_N <- str_count(truth_data_alg_i$tweet, paste0("\\b", ngram2_2beforelandmark, "\\b")) %>% na_if(0)
  ngram2_3beforelandmark_N <- str_count(truth_data_alg_i$tweet, paste0("\\b", ngram2_3beforelandmark, "\\b")) %>% na_if(0)
  
  #### Dataframe to Ouput
  df_out <- data.frame(status_id_str = truth_data_alg_i$status_id_str,
                       ngram1_1beforelandmark,
                       ngram1_2beforelandmark,
                       ngram1_3beforelandmark,
                       ngram2_1beforelandmark,
                       ngram2_2beforelandmark,
                       ngram2_3beforelandmark,
                       ngram1_1beforelandmark_N,
                       ngram1_2beforelandmark_N,
                       ngram1_3beforelandmark_N,
                       ngram2_1beforelandmark_N,
                       ngram2_2beforelandmark_N,
                       ngram2_3beforelandmark_N)
  
  return(df_out)
  
}) %>% bind_rows

truth_data_all <- merge(truth_data_all,
                        text_before_landmark_df,
                        by = "status_id_str",
                        all.x = T,
                        all.y = F)

# Export -----------------------------------------------------------------------
saveRDS(truth_data_all, file.path(tweets_truth_dir, "data", "processed_data",
                                  "word_before_landmark_word_data",
                                  "tweets_truth_word_b4_landmark.Rds"))


