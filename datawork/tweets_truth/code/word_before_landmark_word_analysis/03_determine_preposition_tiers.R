# Patterns of words/phrases that come before location word

# Load Data --------------------------------------------------------------------
results_all_df <- readRDS(file.path(tweets_truth_dir, 
                                    "data", 
                                    "processed_data",
                                    "word_before_landmark_word_data",
                                    "tweets_word_pairs.Rds"))

# Word Pairings ----------------------------------------------------------------
word_results <- results_all_df %>%
  filter(word1or2_landmark_N >= 20) %>%
  group_by(word1) %>%
  summarise(prop_min = min(word1_landmark_prop),
            prop_max = max(word1_landmark_prop),
            prop_mean = mean(word1_landmark_prop),
            prop_median = median(word1_landmark_prop),
            prop_above_50 = mean(word1_landmark_prop > 0.5),
            N = sum(word1or2_landmark_N))

at_wins <- results_all_df %>%
  filter(word1 %in% "at") %>%
  filter(word1_landmark_prop >= 0.5) %>%
  pull(word2)

at_looses <- results_all_df %>%
  filter(word1 %in% "at") %>%
  filter(word1_landmark_prop < 0.5) %>%
  pull(word2)

word_results$tier <- NA
word_results$tier[word_results$prop_above_50 %in% 1] <- 1
word_results$tier[is.na(word_results$tier) & word_results$prop_above_50 > 0.9] <- 2
word_results$tier[is.na(word_results$tier) & word_results$word1 %in% at_looses] <- 3
word_results$tier[word_results$word1 %in% "at"] <- 4
word_results$tier[is.na(word_results$tier) & word_results$prop_above_50 >= 0.5] <- 5
word_results$tier[is.na(word_results$tier) & word_results$prop_max >= 0.5] <- 6
word_results$tier[is.na(word_results$tier)] <- 7

# Print ------------------------------------------------------------------------
for(i in 1:7){
  print(paste("Tier", i))
  word_results$word1[word_results$tier %in% i] %>% sort() %>% print()
  print("----------------------")
} 


