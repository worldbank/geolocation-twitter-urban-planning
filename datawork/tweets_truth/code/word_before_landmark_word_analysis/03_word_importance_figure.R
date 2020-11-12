# Patterns of words/phrases that come before location word

# Load Data --------------------------------------------------------------------
results_all_df <- readRDS(file.path(tweets_truth_dir, 
                                    "data", 
                                    "processed_data",
                                    "word_before_landmark_word_data",
                                    "tweets_word_pairs.Rds"))

results_all_df <- results_all_df %>%
  filter(word1or2_landmark_N >= 30) %>%
  mutate(word1_landmark_prop = round(word1_landmark_prop, digits = 2))

sqrt(nrow(results_all_df))

p <- ggplot(data = results_all_df, aes(x=word2, y=fct_rev(word1), fill=word1_landmark_prop)) + 
  geom_tile(color = "white") +
  geom_vline(xintercept = seq(from = 1, to = 50, by=1), alpha = 0.04) +
  geom_text(aes(label = word1_landmark_prop), color = "black", size = 3.25) +
  geom_hline(yintercept = seq(from = 1.5, to = 55.5, by=1), alpha = 0.5) +
  scale_fill_gradient2(low = "skyblue2",
                       mid = "lightyellow",
                       high = "tomato",
                       midpoint = 0.5,
                       limit = c(0,1),
                       name = "Proportion") +
  labs(y = "Word 1",
       x = "Word 2",
       title = "The proportion of times word 1 precedes the correct landmark\nwhen both word 1 and word 2 are in a tweet and either word precedes the correct landmark") +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    axis.text = element_text(size=11),
    axis.title = element_text(size = 16),
    #axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90,
                               hjust=1,
                               vjust = .4),
    #panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    #legend.justification = c(1, 0),
    legend.position = "none",
    legend.direction = "horizontal") +
  ggsave(filename = file.path(figures_dir, "figure_s4.png"),
         height = 12, width = 12)

