# Analysis of Choosen Types

# NOTES
# tweets and crashes are full data, while crash_clusters are restricted to 
# Nairobi city limits

# Load Data --------------------------------------------------------------------
tweets <- readRDS(file.path(tweets_truth_dir, "data", "raw_data", "tweets_truth.Rds"))

crashes <- readRDS(file.path(tweets_truth_dir, "data", "processed_data", "crash_cluster_analysis", 
                             "tweets_truth_uniquecrash.Rds"))

crash_clusters <- readRDS(file.path(tweets_truth_dir, "data", "processed_data", 
                                    "crash_cluster_analysis", 
                                    "tweets_truth_crashcluster.Rds"))
crash_clusters <- crash_clusters %>%
  arrange(desc(N_crashes)) %>%
  mutate(N_crashes_cumsum = cumsum(N_crashes))

# Stats ------------------------------------------------------------------------

# ** 1. Tweet Level ------------------------------------------------------------

## Number of tweets
nrow(tweets)

## N and % tweets reporting crash
tweets$accident_truth %>% sum()
tweets$accident_truth %>% mean()

## Of crash tweets, N and % geolocated
tweets_crash <- tweets %>%
  filter(accident_truth %in% T)
tweets_crash$geocoded %>% sum()
tweets_crash$geocoded %>% mean()

## Average geolocated crashes daily
N_days <- seq(from = tweets$created_at_nairobitime %>% min() %>% as.Date(),
              to = tweets$created_at_nairobitime %>% max() %>% as.Date(),
              by=1)
sum(tweets_crash$geocoded) / length(N_days)

## Clustered 
tweets$accident_truth %>% sum()

tweets_cl1 <- tweets[!is.na(tweets$crash_cluster_id_v1),]
tweets_cl2 <- tweets[!is.na(tweets$crash_cluster_id_v2),]

# Proportion of tweets clustered with 1 other tweet
(tweets_cl1$crash_cluster_id_v1 %>% table %>% table()) / 
  nrow(tweets_cl1) 

(tweets_cl2$crash_cluster_id_v2 %>% table %>% table()) / 
  nrow(tweets_cl2)

# ** 2. Crash Level ------------------------------------------------------------

## Number of unique crashes
nrow(crashes)

# ** 3. Cluster Level -----------------------------------------------------------

## Number of crash clusters
nrow(crash_clusters)

## Number of crashes considered in clusteres
sum(crash_clusters$N_crashes)

## Number of crash clusters that account for half of crashes
half_crashes <- sum(crash_clusters$N_crashes)/2
N_cluster_half_crashes <- sum(crash_clusters$N_crashes_cumsum <= half_crashes)
prop_cluster_half_crashes <- sum(crash_clusters$N_crashes_cumsum <= half_crashes) / nrow(crash_clusters)

N_cluster_half_crashes
prop_cluster_half_crashes

# km distance (max cluster size is 300 meters)
N_cluster_half_crashes*300/1000 #km

## Percent with >=2 crashes
mean(crash_clusters$N_crashes >= 2)

## Number with 10 or more crashes
sum(crash_clusters$N_crashes >= 10)


