# Provide Basic Summary Stats from Waze Data

# Tweets -----------------------------------------------------------------------
tweets <- readRDS(file.path(tweets_all_dir, "data", "raw_data", "tweets.Rds"))

## Daily Total
tweets <- tweets %>%
  mutate(date = created_at_nairobitime %>% as.Date()) %>%
  group_by(date) %>% 
  summarise(N = n())

ggplot() +
  geom_col(data=tweets, aes(x=date, y=N), color = "dodgerblue3",
           fill = "dodgerblue3") +
  labs(x=NULL, y="Daily\nTweets") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  ggsave(filename = file.path(figures_dir, "figure_s1.png"),
         height = 3, width=6.5)


