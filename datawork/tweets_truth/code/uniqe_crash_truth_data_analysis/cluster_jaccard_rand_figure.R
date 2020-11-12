# Optimize Clustering Stats: Table fo Results

# Load Data --------------------------------------------------------------------
results_all <- readRDS(file.path(tweets_truth_dir, 
                                 "data",
                                 "processed_data",
                                 "uniqe_crash_truth_data_analysis",
                                 "clustering_eval_jaccard_rand_results.Rds"))


results_all$dataset <- results_all$dataset %>% as.character()
results_all$dataset[results_all$dataset %in% "1"] <- "Truth Dataset 1"
results_all$dataset[results_all$dataset %in% "2"] <- "Truth Dataset 2"

#### Prep variables
results_all$hour <- results_all$hour %>% as.factor()
results_all$km <- results_all$km %>% as.factor()

results_all$jaccard_shrt <- results_all$jaccard %>% round(3)
results_all$ari_shrt <- results_all$ari %>% round(3)

#### ggplot parameters
ari_min <- min(results_all$ari)
ari_max <- max(results_all$ari)
ari_middle <- (ari_min + ari_max) / 2

jaccard_min <- min(results_all$jaccard)
jaccard_max <- max(results_all$jaccard)
jaccard_middle <- (jaccard_min + jaccard_max) / 2

# ARI
p_ari <- ggplot(data = results_all,
                aes(x = hour, y = km, fill = ari)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ari_shrt), color = "black", size = 3.25) +
  scale_fill_gradient2(low = "skyblue2",
                       mid = "lightyellow",
                       high = "tomato",
                       midpoint = ari_middle,
                       limit = c(ari_min, ari_max),
                       name = "") +
  labs(title = "Adjusted Rand Index",
       x = "Hours",
       y = "Kilometers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none") +
  facet_wrap(~dataset)

# Jaccard
p_jaccard <- ggplot(data = results_all,
                    aes(x = hour, y = km, fill = jaccard)) +
  geom_tile(color = "white") +
  geom_text(aes(label = jaccard_shrt), color = "black", size = 3.25) +
  scale_fill_gradient2(low = "skyblue2",
                       mid = "lightyellow",
                       high = "tomato",
                       midpoint = jaccard_middle,
                       limit = c(jaccard_min, jaccard_max),
                       name = "") +
  labs(title = "Jaccard Coefficient",
       x = "Hours",
       y = "Kilometers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none") +
  facet_wrap(~dataset)

# Export -----------------------------------------------------------------------
p_all <- ggarrange(p_ari,
                   p_jaccard,
                   nrow = 2)
ggsave(p_all, filename = file.path(figures_dir,
                                   "figure_s5.png"),
       height = 8, width = 8)





