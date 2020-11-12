# Merge Geolocation

# Load Tweets ------------------------------------------------------------------
tweets_df <- readRDS(file.path(tweets_all_dir, "data", "processed_data", "tweets_classified.Rds"))

tweets_alg_geo <- file.path(tweets_all_dir, "data", "processed_data", "tweets_geocoded_chunks") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  dplyr::select(lon_alg, lat_alg, status_id_str,
                matched_words_correct_spelling, matched_words_tweet_spelling,
                landmarks_all_tweet_spelling,landmarks_all_correct_spelling,
                how_determined_landmark, dist_closest_crash_word,
                roads_all_tweet_spelling, roads_all_correct_spelling) %>%
  distinct(status_id_str, .keep_all = T)

#### Filter select geocoded tweets
tweets_alg_geo$rm <- F
tweets_alg_geo$rm[grepl("tried_to_snapped_to_road_but_road_too_far", tweets_alg_geo$how_determined_landmark)] <- T
tweets_alg_geo$rm[grepl("multiple_landmarks_choose_closest_crashword|tier_5|tier_6", tweets_alg_geo$how_determined_landmark) &
                    tweets_alg_geo$dist_closest_crash_word %in% as.character(3:26)] <- T

tweets_alg_geo <- tweets_alg_geo[tweets_alg_geo$rm %in% F,]
tweets_alg_geo$rm <- NULL

#### Merge geocodes with main tweets
tweets_all_geo <- merge(tweets_df, tweets_alg_geo, by = "status_id_str",
                        all.x = T, all.y = F)

# Export -----------------------------------------------------------------------
saveRDS(tweets_all_geo, file.path(tweets_all_dir, "data", "processed_data", "tweets_classified_geoparsed.Rds"))





