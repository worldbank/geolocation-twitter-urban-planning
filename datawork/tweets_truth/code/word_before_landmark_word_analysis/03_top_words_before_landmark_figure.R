# Patterns of words/phrases that come before location word

# Load Data --------------------------------------------------------------------
top_words <- readRDS(file.path(tweets_truth_dir, 
                               "data",
                               "processed_data",
                               "word_before_landmark_word_data",
                               "words_before_landmarks.Rds"))

top_words <- top_words[top_words$N >= 20,]
top_words$percent <- paste0(round(top_words$prop, 4)*100, "%")
top_words$prop <- top_words$prop * 100

p <- ggplot(data=top_words, aes(x=fct_reorder(word, prop), y=prop)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = percent), hjust=-.2) +
  coord_flip() +
  labs(x = "",
       y = "") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 50)) +
  labs(y = "Percent of Tweets")

ggsave(p, filename = file.path(figures_dir, "figure_s3.png"),
       height = 8, width = 8)

