# Geoparse Tweets
# Calculate precision and recall

# Loads dataframes of estimated locations from the algorithm, using different
# gazetteers. Calculates precision and recall:
#   (1) of the one location outputted by the algorithm
#   (2) of any landmark location found by the algoruthm

# Outline
# 1. Tweets to Long: each row is a location found, using any location found by algorithm
# 2. Load and prep LNEx
# 3. Load truth tweets
# 4. Append data from our algorithm and LNEX
# 5. Calculate distance to truth
# 6. Collapse to tweet level
# 7. Tweet level precision and recall
# 8. Cluster level precision and recall
# 9. Export results

# 1. Tweets to Long ============================================================
# For each tweet, the algorithm gives: (1) the estimated crash location and (2)
# all locations found by the algorithm. Creates a "long" dataframe where each
# row is any location found by algorithm. This is needed in order to estimate
# the minimum distance to any location found.

## Load Tweets
df_aug          <- readRDS(file.path(tweets_geoparse_dir, "processed_data", "tweet_geoparse_gaz_aug.Rds"))
df_raw          <- readRDS(file.path(tweets_geoparse_dir, "processed_data", "tweet_geoparse_gaz_raw.Rds"))
df_aug_geonames <- readRDS(file.path(tweets_geoparse_dir, "processed_data", "tweet_geoparse_gaz_aug_geonames.Rds"))
df_aug_google   <- readRDS(file.path(tweets_geoparse_dir, "processed_data", "tweet_geoparse_gaz_aug_google.Rds"))
df_aug_osm      <- readRDS(file.path(tweets_geoparse_dir, "processed_data", "tweet_geoparse_gaz_aug_osm.Rds"))

## Data to Long
df_aug          <- df_aug %>% prep_alg_data(type_name = "aug")
df_raw          <- df_raw %>% prep_alg_data(type_name = "raw")
df_aug_geonames <- df_aug_geonames %>% prep_alg_data(type_name = "aug_geonames")
df_aug_google   <- df_aug_google %>% prep_alg_data(type_name = "aug_google")
df_aug_osm      <- df_aug_osm %>% prep_alg_data(type_name = "aug_osm")

## Append
df_alg <- bind_rows(df_aug,
                    df_raw,
                    df_aug_google,
                    df_aug_osm,
                    df_aug_geonames)

# 2. Load / Prep LNEx ----------------------------------------------------------
df_lnex <- read.csv(file.path(tweets_geoparse_dir, "processed_data", "tweet_geoparse_lnex.csv"),
                    stringsAsFactors = F) %>% 
  dplyr::select(status_id_str, lat, lon) %>%
  group_by(status_id_str) %>%
  mutate(lat_alg = mean(lat),
         lon_alg = mean(lon)) %>%
  mutate(type = "lnex")

# 3. Load truth tweets ---------------------------------------------------------
tweets_df <- readRDS(file.path(tweets_geoparse_dir, "processed_data", "tweets_for_geolocation.Rds"))
tweets_df <- tweets_df %>%
  dplyr::select(status_id_str, latitude_truth, longitude_truth, created_at_nairobitime) %>%
  dplyr::rename(lat_truth = latitude_truth,
                lon_truth = longitude_truth)

# 4. Append and prep appended tweets -------------------------------------------
## Append and merge with truth tweets
df_all <- bind_rows(df_alg,
                    df_lnex) %>%
  left_join(tweets_df, by = "status_id_str")

## Project coordinates
df_all <- df_all %>%
  reproj_coords("lat",       "lon") %>%
  reproj_coords("lat_alg",   "lon_alg") %>%
  reproj_coords("lat_truth", "lon_truth")

# 5. Distance to Truth ---------------------------------------------------------
df_all$alg_dist <- sqrt((df_all$lon_alg - df_all$lon_truth)^2 +
                          (df_all$lat_alg - df_all$lat_truth)^2)

df_all$anyloc_dist <- sqrt((df_all$lon - df_all$lon_truth)^2 +
                             (df_all$lat - df_all$lat_truth)^2)

# 6. Collapse to Tweet Level ---------------------------------------------------
df_all_twt <- df_all %>%
  group_by(status_id_str, type, created_at_nairobitime) %>%
  dplyr::summarise(alg_dist = min(alg_dist),
                   anyloc_dist = min(anyloc_dist),
                   
                   lon_alg = lon_alg[1],
                   lat_alg = lat_alg[1],
                   
                   lat_truth = lat_truth[1],
                   lon_truth = lon_truth[1]) %>%
  ungroup()

# 7. Tweet level: precision/recall ---------------------------------------------
# Calculate precision and recall on (1) algorithm finding any location near the
# truth crash location and (2) estimated crash location determined by algorithm
# near the true crash location

twt_prec_rec_df <- df_all_twt %>%
  group_by(type) %>%
  summarise(alg_precision = mean(alg_dist <= 500, na.rm=T),
            anyloc_precision = mean(anyloc_dist <= 500, na.rm=T),
            
            alg_recall = sum(alg_dist <= 500, na.rm=T) / n(),
            anyloc_recall = sum(anyloc_dist <= 500, na.rm=T) / n())

# 8. Cluster Level: Precision/Recall -------------------------------------------
# Cluster tweets to individual crashes (more than one tweet can refer to the 
# same crash), then estimate precision and recall at the crash level.

clstr_prec_rec_df <- lapply(unique(df_all_twt$type), 
                            calc_cluster_results, 
                            df_all_twt, 
                            4, 
                            500) %>%
  bind_rows()

# 9. Export --------------------------------------------------------------------
## Merge tweet level and cluster level results
results_all <- merge(twt_prec_rec_df, clstr_prec_rec_df, by = "type")

## Export
saveRDS(results_all, file.path(tweets_geoparse_dir, "results", "tweet_geoparse_results.Rds"))
