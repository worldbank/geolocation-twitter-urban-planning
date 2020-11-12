# Cluster Tweets

# Load Tweets ------------------------------------------------------------------
tweets_truth <- readRDS(file.path(tweets_truth_dir, "data", "raw_data", "tweets_truth.Rds"))

tweets_truth <- tweets_truth %>%
  filter(geocoded %in% T,
         accident_truth %in% T)

tweets_truth_clustered <- cluster_crashes_one_dataset(crashes_df = tweets_truth,
                                                      time_var = "created_at_nairobitime",
                                                      lat_var = "latitude_truth",
                                                      lon_var = "longitude_truth",
                                                      vars_to_keep = c("tweet"),
                                                      time_thresh_hrs=4, 
                                                      cluster_km=0.5, 
                                                      cluster_id_only=F)

crs(tweets_truth_clustered) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Export -----------------------------------------------------------------------
saveRDS(tweets_truth_clustered,
        file.path(tweets_truth_dir, "data", "processed_data", "crash_cluster_analysis", 
                  "tweets_truth_uniquecrash.Rds"))




