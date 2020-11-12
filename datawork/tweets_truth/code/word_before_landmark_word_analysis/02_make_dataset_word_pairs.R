# Patterns of words/phrases that come before location word

# For each ngram, we are interested in:
# N times ngram appears before word
# N times ngram appears somewhere else

# Load Data --------------------------------------------------------------------
# Restrict to observations included in the truth dataset and where we know how the
# landmark used to geocode the tweet was written in the tweet.
truth_data_alg <- readRDS(file.path(tweets_truth_dir, "data", "processed_data",
                                    "word_before_landmark_word_data",
                                    "tweets_truth_word_b4_landmark.Rds"))
truth_data_alg <- truth_data_alg[!is.na(truth_data_alg$ngram1_1beforelandmark),]

# Remove roads
rm_roads <- c("road", "rd", "street", "st", "way", "avenue", "ave")
rm_roads <- paste0("\\b", rm_roads, "\\b") %>% paste(collapse = "|")

truth_data_alg <- truth_data_alg[!grepl(rm_roads, truth_data_alg$landmark_c1),]
truth_data_alg <- truth_data_alg[!grepl(rm_roads, truth_data_alg$landmark_c2),]

truth_data_alg$tweet <- truth_data_alg$tweet %>% 
  str_replace_all(truth_data_alg$crash_landmark, "landmark_here") %>%
  str_replace_all("\\bthe\\b", " ") %>%
  str_squish()

### Top words
top_words <- c(truth_data_alg$ngram1_1beforelandmark,
               truth_data_alg$ngram2_1beforelandmark) %>%
  table() %>%
  as.data.frame() %>%
  dplyr::rename(word = ".",
                N = Freq) %>%
  mutate(word = word %>% as.character()) %>%
  filter(N >= 10)
top_words$prop <- top_words$N / nrow(truth_data_alg)
top_words <- top_words[!(top_words$word %in% c("and", "waiyaki way", "way")),] # remove select words

### Word combinations
word_combn <- combn(top_words$word, 2) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(word_1 = V1,
                word_2 = V2) %>%
  mutate(word_1 = word_1 %>% as.character(),
         word_2 = word_2 %>% as.character()) %>%
  
  # Remove cases where word within another word
  filter(!str_detect(word_1, word_2),
         !str_detect(word_2, word_1)) %>%
  
  filter(!str_detect(word_1, "road|overturn|\\brd\\b|\\bcar\\b|bypass"),
         !str_detect(word_2, "road|overturn|\\brd\\b|\\bcar\\b|bypass")) #%>%

#filter(!str_detect(word_1, "accident|crash"),
#       !str_detect(word_2, "accident|crash"))

### Dataframe of words
results_df <- lapply(1:nrow(word_combn), function(i){
  
  if((i %% 20) == 0) print(i)
  
  word_combn_i <- word_combn[i,]
  word1 <- word_combn_i$word_1
  word2 <- word_combn_i$word_2
  
  tweet_i <- truth_data_alg %>%
    filter(grepl(word1, truth_data_alg$tweet),
           grepl(word2, truth_data_alg$tweet)) %>%
    pull(tweet)
  
  N_both <- length(tweet_i)
  word1_landmark_N <- grepl(paste("\\b", word1, "landmark_here"), tweet_i) %>% sum()
  word2_landmark_N <- grepl(paste("\\b", word2, "landmark_here"), tweet_i) %>% sum()
  word1or2_landmark_N <- word1_landmark_N + word2_landmark_N
  
  df_out <- data.frame(word1 = word1,
                       word2 = word2,
                       N_both = N_both,
                       word1_landmark_N = word1_landmark_N,
                       word2_landmark_N = word2_landmark_N,
                       word1or2_landmark_N = word1or2_landmark_N)
  
  return(df_out)
}) %>%
  bind_rows() 

results_df <- results_df %>%
  mutate(word1_landmark_prop = word1_landmark_N / word1or2_landmark_N,
         word2_landmark_prop = word2_landmark_N / word1or2_landmark_N)

#### Correlation-Like Dataframe
# swap 1 <--> 2 in somewhat of a hacky way
# Make into essentially correlation-like table. Each row repeated, but where
# word1 and word2 swapped
results_df_12swap <- results_df
names(results_df_12swap) <- names(results_df_12swap) %>%
  str_replace_all("word1", "word3") %>%
  str_replace_all("word2", "word4") %>%
  str_replace_all("word3", "word2") %>%
  str_replace_all("word4", "word1")
results_df_12swap$word2or2_landmark_N <- NULL
results_df_12swap$word1or2_landmark_N <- results_df_12swap$word1_landmark_N + results_df_12swap$word2_landmark_N

results_all_df <- bind_rows(results_df,
                            results_df_12swap)

# Export -----------------------------------------------------------------------
saveRDS(results_all_df, file.path(tweets_truth_dir, "data", 
                                  "processed_data",
                                  "word_before_landmark_word_data",
                                  "tweets_word_pairs.Rds"))

saveRDS(top_words, file.path(tweets_truth_dir, "data", 
                             "processed_data",
                             "word_before_landmark_word_data",
                             "words_before_landmarks.Rds"))




