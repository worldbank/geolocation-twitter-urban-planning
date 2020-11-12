# Optimize Clustering Stats

# Load Data --------------------------------------------------------------------
tweets_df <- readRDS(file.path(tweets_truth_dir, "data", "raw_data", "tweets_truth.Rds"))
tweets_df <- tweets_df %>%
  filter(!is.na(latitude_truth),
         !is.na(longitude_truth))

## Prep cluster var
tweets_df$crash_cluster_id_v1 <- tweets_df$crash_cluster_id_v1 %>% str_squish() %>% as.numeric()
tweets_df$crash_cluster_id_v2 <- tweets_df$crash_cluster_id_v2 %>% str_squish() %>% as.numeric()

## Split into two datasets
tweets_df1 <- tweets_df %>%
  filter(!is.na(crash_cluster_id_v1)) %>%
  dplyr::rename(cluster_id = crash_cluster_id_v1)

tweets_df2 <- tweets_df %>%
  filter(!is.na(crash_cluster_id_v2)) %>%
  dplyr::rename(cluster_id = crash_cluster_id_v2)

# Cluster Metric Calc ----------------------------------------------------------
results_all <- data.frame(NULL)

for(hour in c(1,2,4,12,24)){
  for(km in c(.1,.5,1,2,3)){
    for(dataset in 1:2){
      
      print(paste(hour, km, dataset))
      
      if(dataset == 1) tweets_df <- tweets_df1
      if(dataset == 2) tweets_df <- tweets_df2
      
      tweets_df$cluster_id_alg <- cluster_crashes_one_dataset(crashes_df = tweets_df,
                                                              time_var = "created_at_nairobitime",
                                                              lat_var = "latitude_truth",
                                                              lon_var = "longitude_truth",
                                                              vars_to_keep = c("text"),
                                                              time_thresh_hrs=hour, 
                                                              cluster_km=km, 
                                                              cluster_id_only=T)
      
      jaccard <- cluster_similarity(tweets_df$cluster_id, 
                                    tweets_df$cluster_id_alg,
                                    similarity = c("jaccard"),
                                    method = "independence")

      ari <- ARI(tweets_df$cluster_id, 
                 tweets_df$cluster_id_alg)
    
      results_i <- data.frame(dataset = dataset,
                              hour = hour,
                              km = km,
                              jaccard = jaccard,
                              ari = ari)
      
      results_all <- bind_rows(results_all, results_i)
      
    }
  }
}

# Export -----------------------------------------------------------------------
saveRDS(results_all, file.path(tweets_truth_dir, 
                               "data",
                               "processed_data",
                               "uniqe_crash_truth_data_analysis",
                               "clustering_eval_jaccard_rand_results.Rds"))




