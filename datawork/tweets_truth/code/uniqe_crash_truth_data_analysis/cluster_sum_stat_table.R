# Summary Statics of Clusters from Twitter

# Load Data --------------------------------------------------------------------
tweets_df <- readRDS(file.path(tweets_truth_dir, "data", "raw_data", "tweets_truth.Rds"))
tweets_df <- tweets_df %>%
  filter(!is.na(latitude_truth),
         !is.na(longitude_truth))

clusters_v1 <- tweets_df %>% 
  dplyr::rename(crash_cluster_id = crash_cluster_id_v1) %>%
  group_by(crash_cluster_id) %>%
  mutate(crash_cluster_id_freq = n()) %>%
  ungroup() %>%
  filter(!is.na(crash_cluster_id))

clusters_v2 <- tweets_df %>% 
  dplyr::rename(crash_cluster_id = crash_cluster_id_v2) %>%
  group_by(crash_cluster_id) %>%
  mutate(crash_cluster_id_freq = n()) %>%
  ungroup() %>%
  filter(!is.na(crash_cluster_id))

# Separate Tweets: Clustered vs Unique -----------------------------------------
tweets_clustered_v1 <- clusters_v1[clusters_v1$crash_cluster_id_freq >= 2,]
tweets_unique_v1 <- clusters_v1[clusters_v1$crash_cluster_id_freq == 1,]

tweets_clustered_v2 <- clusters_v2[clusters_v2$crash_cluster_id_freq >= 2,]
tweets_unique_v2 <- clusters_v2[clusters_v2$crash_cluster_id_freq == 1,]

# Create Dataframes for Summary Stats for Clustered ----------------------------
cluster_summarize <- function(id, tweets_clustered){

  tweets_clustered_i <- tweets_clustered[tweets_clustered$crash_cluster_id %in% id,]
  
  diff_hours <- difftime(max(tweets_clustered_i$created_at_nairobitime), min(tweets_clustered_i$created_at_nairobitime), units="hours") %>% as.numeric()
  
  coordinates(tweets_clustered_i) <- ~longitude_truth+latitude_truth
  diff_kms <- max(gDistance(tweets_clustered_i, byid=T))*111.12
  
  df_out <- data.frame(crash_cluster_id = id,
                       diff_hours = diff_hours,
                       diff_kms = diff_kms,
                       N_tweets = nrow(tweets_clustered_i))
  
  return(df_out)
}

cluster_sumstat_v1 <- lapply(unique(tweets_clustered_v1$crash_cluster_id), cluster_summarize, tweets_clustered_v1) %>% bind_rows
cluster_sumstat_v2 <- lapply(unique(tweets_clustered_v2$crash_cluster_id), cluster_summarize, tweets_clustered_v2) %>% bind_rows

# Remove Outliers of Clustered Tweets DFs --------------------------------------
table(cluster_sumstat_v1$diff_hours < 24)
table(cluster_sumstat_v1$diff_kms < 5)

cluster_sumstat_v1 <- cluster_sumstat_v1[cluster_sumstat_v1$diff_hours < 24,]
cluster_sumstat_v2 <- cluster_sumstat_v2[cluster_sumstat_v2$diff_hours < 24,]

cluster_sumstat_v1 <- cluster_sumstat_v1[cluster_sumstat_v1$diff_kms < 5,]
cluster_sumstat_v2 <- cluster_sumstat_v2[cluster_sumstat_v2$diff_kms < 5,]

# Summary Stats ----------------------------------------------------------------
#### Clustered Tweets
clustered_sum_results <- bind_rows(
  cluster_sumstat_v1$diff_hours %>% summary %>% as.numeric %>% t %>% as.data.frame %>% mutate(variable="Hours Diff") %>% mutate(dataset=1),
  cluster_sumstat_v2$diff_hours %>% summary %>% as.numeric %>% t %>% as.data.frame %>% mutate(variable="Hours Diff") %>% mutate(dataset=2),
  cluster_sumstat_v1$diff_kms %>% summary %>% as.numeric %>% t %>% as.data.frame %>% mutate(variable="KMs Diff") %>% mutate(dataset=1),
  cluster_sumstat_v2$diff_kms %>% summary %>% as.numeric %>% t %>% as.data.frame %>% mutate(variable="KMs Diff") %>% mutate(dataset=2),
  cluster_sumstat_v1$N_tweets %>% summary %>% as.numeric %>% t %>% as.data.frame %>% mutate(variable="N Tweets") %>% mutate(dataset=1),
  cluster_sumstat_v2$N_tweets %>% summary %>% as.numeric %>% t %>% as.data.frame %>% mutate(variable="N Tweets") %>% mutate(dataset=2)
) %>%
  dplyr::rename(min=V1,
                q1=V2,
                median=V3,
                mean=V4,
                q3=V5,
                max=V6)


sink(file.path(tables_dir, "table_s6.tex"))
cat("\\begin{tabular}{lcccccc} ")
cat("\\hline ")
cat("Variable & Min & Quartile 1 & Median & Mean & Quartile 3 & Max \\\\ ")
cat("\\hline ")
cat("\\multicolumn{7}{c}{Truth Dataset 1} \\\\ ")

for(i in 1:nrow(clustered_sum_results[clustered_sum_results$dataset %in% 1,])){
  df_i <- clustered_sum_results[clustered_sum_results$dataset %in% 1,][i,]
  
  cat(df_i$variable, " & ")
  cat(df_i$min %>% round(3), " & ")
  cat(df_i$q1 %>% round(3), " & ")
  cat(df_i$median %>% round(3), " & ")
  cat(df_i$mean %>% round(3), " & ")
  cat(df_i$q3 %>% round(3), " & ")
  cat(df_i$max %>% round(3), " \\\\ ")
}

cat("\\hline ")
cat("\\multicolumn{7}{c}{Truth Dataset 2} \\\\ ")

for(i in 1:nrow(clustered_sum_results[clustered_sum_results$dataset %in% 2,])){
  df_i <- clustered_sum_results[clustered_sum_results$dataset %in% 2,][i,]
  
  cat(df_i$variable, " & ")
  cat(df_i$min %>% round(3), " & ")
  cat(df_i$q1 %>% round(3), " & ")
  cat(df_i$median %>% round(3), " & ")
  cat(df_i$mean %>% round(3), " & ")
  cat(df_i$q3 %>% round(3), " & ")
  cat(df_i$max %>% round(3), " \\\\ ")
}

cat("\\hline ")
cat("\\end{tabular} ")
sink()






