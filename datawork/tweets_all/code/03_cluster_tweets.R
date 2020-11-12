# Cluster Tweets

# Load Tweets ------------------------------------------------------------------
tweets_all <- readRDS(file.path(tweets_all_dir, "data", "processed_data", "tweets_classified_geoparsed.Rds"))

### Subset for Tweets All
tweets_all <- tweets_all[tweets_all$crash_tweet_algorithm %in% TRUE,]
tweets_all <- tweets_all[tweets_all$potentially_accident_related %in% TRUE,]
tweets_all <- tweets_all[!is.na(tweets_all$lat_alg),]
tweets_all <- tweets_all[!is.na(tweets_all$lon_alg),]

# Cluster Tweets ---------------------------------------------------------------
tweets_all_clustered <- cluster_crashes_one_dataset(crashes_df = tweets_all,
                            time_var = "created_at_nairobitime",
                            lat_var = "lat_alg",
                            lon_var = "lon_alg",
                            vars_to_keep = c("tweet"),
                            time_thresh_hrs=4, 
                            cluster_km=0.5, 
                            cluster_id_only=F)

# Add CRS To Dataset -----------------------------------------------------------
crs(tweets_all_clustered) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Id
tweets_all_clustered$crash_id   <- 1:nrow(tweets_all_clustered)

# Use Coordinates, not polygon -------------------------------------------------
tweets_all_clustered_coords <- tweets_all_clustered %>% 
  coordinates() %>% 
  as.data.frame() %>%
  dplyr::rename(longitude = V1,
                latitude = V2)
tweets_all_clustered_df <- bind_cols(tweets_all_clustered %>% as.data.frame(), 
                                     tweets_all_clustered_coords)

# Export -----------------------------------------------------------------------
saveRDS(tweets_all_clustered_df,   file.path(tweets_all_dir, "data", "processed_data", "tweets_classified_geoparsed_uniquecrashes.Rds"))



