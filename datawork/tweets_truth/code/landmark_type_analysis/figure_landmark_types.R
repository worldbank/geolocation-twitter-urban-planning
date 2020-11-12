# Analysis of Choosen Types

# Load Data --------------------------------------------------------------------
#### Tweets
tweet_df <- readRDS(file.path(tweets_all_dir, "data", "processed_data", "tweets_classified_geoparsed.Rds"))

#### Landmark
landmarks <- readRDS(file.path(landmarkgaz_dir, "data", "gazetteers", "landmark_gazetter_aug.Rds"))
landmarks <- spTransform(landmarks, CRS(NAIROBI_UTM_PROJ))

# Prep Tweet Data --------------------------------------------------------------
tweet_df <- tweet_df %>%
  
  # Filter to truth data and where algorithm used landmark
  filter(!is.na(matched_words_correct_spelling),
         !is.na(latitude_truth),
         !is.na(longitude_truth)) %>%
  
  # Filter to ones where algorithm was correct
  mutate(dist = sqrt((latitude_truth - lat_alg)^2 + (longitude_truth - lon_alg)^2) * 111.12) %>%
  filter(dist <= 0.5) %>%
  
  # Filter to ones where only one landmark used
  filter(!grepl(";", matched_words_correct_spelling))
  # filter(type %in% "landmark")

coordinates(tweet_df) <- ~longitude_truth + latitude_truth
crs(tweet_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
tweet_df <- spTransform(tweet_df, CRS(NAIROBI_UTM_PROJ))

# Prep Landmarks ---------------------------------------------------------------
# Restrict landmarks to those where landmark found in algorithm used; makes
# subsetting faster later
all_locs <- tweet_df$matched_words_correct_spelling %>% str_split(";") %>% unlist() %>% unique()
landmarks <- landmarks[landmarks$name %in% all_locs,]

# Landmark in tweet dataframe --------------------------------------------------
lndmrks_dst_truth <- lapply(1:nrow(tweet_df), function(i){
  # For each tweet, grab matched landmarks. Determine distance from truth. Append
  # landmark dataframe
  
  if((i %% 100) %in% 0) print(i)
  
  tweet_df_i <- tweet_df[i,]
  
  landmark_words <- tweet_df_i$matched_words_correct_spelling %>% str_split(";") %>% unlist() %>% unique()
  
  landmarks_i <- landmarks[landmarks$name %in% landmark_words,]

  near <- as.numeric(gDistance(tweet_df_i, landmarks_i, byid=T)) <= 500
  landmarks_near <- landmarks_i$type[near] %>% str_split(";") %>% unlist() %>% unique()
  landmarks_far <- landmarks_i$type[!near] %>% str_split(";") %>% unlist() %>% unique()
  
  if(length(landmarks_near) %in% 0 & length(landmarks_far) %in% 0){
    df_out <- data.frame(NULL)
  } else if(length(landmarks_near) %in% 0){
    df_out <- data.frame(far = landmarks_far)
  } else if(length(landmarks_far) %in% 0){
    df_out <- data.frame(near = landmarks_near)
  } else{
    df_out <- list(near = landmarks_near, far = landmarks_far) %>% cross_df()
  }
  
  if(nrow(df_out) > 0) df_out$status_id_str <- tweet_df_i$status_id_str
  
  return(df_out)
  
}) %>%
  bind_rows()

# ANALYSIS =====================================================================

# Prep Data for Figure ---------------------------------------------------------
# Subset to cases where a landmark name is mapped to multiple locations, and 
# locations are both near and far from a crash cite
lndmrks_dst_truth <- lndmrks_dst_truth %>%
  filter(!is.na(near)) %>%
  filter(!is.na(far))

## Number of times landmark types are near the correct location
near_df <- lndmrks_dst_truth %>%
  dplyr::select(near, status_id_str) %>%
  distinct(near, status_id_str) %>%
  group_by(near) %>%
  summarise(N_near = n()) %>%
  dplyr::rename(word = near)

## Number of times landmark types are far from the correct location
far_df <- lndmrks_dst_truth %>%
  dplyr::select(far, status_id_str) %>%
  distinct(far, status_id_str) %>%
  group_by(far) %>%
  summarise(N_far = n()) %>%
  dplyr::rename(word = far)

## Merge Near/Far 
nf_df <- merge(near_df, far_df, by = "word")

## Numbe of tweets
nf_df$N_near_obs <- unique(lndmrks_dst_truth$status_id_str) %>% length()
nf_df$N_far_obs <- unique(lndmrks_dst_truth$status_id_str) %>% length()

## Proportion and difference
nf_df <- nf_df %>%
  mutate(prop_near = N_near / N_near_obs,
         prop_far = N_far / N_far_obs) %>%
  mutate(diff = prop_near - prop_far,
         per_diff = (prop_near - prop_far) / prop_far * 100)

## Subet
nf_df_sub <- nf_df %>%
  filter(prop_near > 0.1) %>%
  filter(per_diff > 0) %>%
  filter(diff > 0.02) %>%
  mutate(diff_ratio = prop_near / prop_far) %>%
  mutate(word = word %>% str_replace_all("_", " ") %>% tools::toTitleCase(),
         diff_ratio_lab = diff_ratio %>% round(1))

# Figure -----------------------------------------------------------------------
p_likelihood <- ggplot(nf_df_sub,
                       aes(x = diff_ratio, y = reorder(word, diff_ratio))) +
  geom_col(fill = "lightskyblue3") +
  geom_text(aes(label = diff_ratio_lab), nudge_x = 1, fontface = "bold") +
  labs(title = "Proportion Near / Proportion Far",
       x = "",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(face = "bold", color = "black"))

nf_df_sub_stack <- nf_df_sub %>%
  dplyr::select(word, prop_near, prop_far, per_diff) %>%
  pivot_longer(-c(word, per_diff))

p_prop <- ggplot(nf_df_sub_stack) +
  geom_col(aes(x = value, y = reorder(word, per_diff), 
               group = name, fill = name),
           position = "dodge",
           color = "white",
           size = .1) +
  labs(x = "", y = "", title = "Proportion of Tweets where Landmark Type is\nNear or Far from Crash Location",
       subtitle = "",
       fill = "") +
  scale_fill_manual(values = c("deepskyblue2", "darkorange1"),
                    labels = c("Far from Crash", "Near Crash"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 0.5, angle = 0),
        axis.text.y = element_text(face = "bold", color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

p_all <- ggarrange(p_prop,
                   p_likelihood,
                   labels = c("A", "B"))

ggsave(p_all, filename = file.path(figures_dir, "figure_s2.png"),
       height = 5, width = 10)
 




