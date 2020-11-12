# Add Geocodes to Tweets

#### Parameters
# Implement algorithm in chunks of tweets. After each chunk, exports data
chunk_size <- 100

# Checks if tweet/chunk data already processed. If FALSE, then skips
OVERWRITE_FILE <- FALSE

# Load Tweets ------------------------------------------------------------------
tweets_df <- readRDS(file.path(tweets_all_dir, "data", "processed_data", "tweets_classified.Rds"))

tweets_df <- tweets_df %>%
  filter(crash_tweet_algorithm %in% T) %>%
  arrange(desc(created_at_nairobitime))

# Algorithm Inputs/Parameters --------------------------------------------------
## Gazetteers
roads <- readRDS(file.path(osm_dir, "data", "processed_data", "roads", "osm_roads_aug.Rds"))
landmarks <- readRDS(file.path(landmarkgaz_dir, "data", "gazetteers", "landmark_gazetter_aug.Rds"))
estates <- readRDS(file.path(estates_dir, "data", "nairobi_estates.Rds"))

prepositions <- list(c("EVENT_WORD after", "EVENT_WORD near", "EVENT_WORD outside", 
                       "EVENT_WORD past", "around", "hapo", "just after", "just before", 
                       "just past", "near", "next to", "opposite", "outside", "past", 
                       "you approach", "apa", "apo", "hapa", "right after", 
                       "right before", "right past", "just before you reach"), 
                     c("EVENT_WORD at", "before"), 
                     c("after"),
                     c("at",
                       "happened at", "at the", "pale"), 
                     c("between", "from",
                       "btw", "btwn"), 
                     c("along", "approach", "in", "on", "opp", "to", "towards",
                       "toward") 
)

event <- c("accidents", "accident", "crash", "crush", "overturn", "overturned", 
           "collision", "wreck", "wreckage", "pile up", "pileup", "incident", 
           "hit and run", "hit", "roll", "rolled", "read end", "rear ended")
junction <- c("intersection", "junction")
false_positive <- c("githurai bus", "githurai matatu", 
                    "githurai 45 bus", "githurai 45 matatu",
                    "city hoppa bus", "hoppa bus",
                    "rongai bus", "rongai matatu", "rongai matatus",
                    "machakos bus", "machakos minibus", "machakos matatu",
                    "at ntsa kenya", 
                    "service lane", "star bus",
                    "prius", "mpya bus",
                    "heading towards") 
type_list <- list(c("bus_station","transit_station","stage_added", "stage", "bus_stop"),
                  c("mall", "shopping_mall"),
                  c("restaurant", "bakery", "cafe"),
                  c("building"),
                  c("parking"))

# Locate Crashes ---------------------------------------------------------------
starts <- seq(from = 1, to = length(tweets_df$tweet), by=chunk_size)

for(start_i in starts){
  
  print(paste(start_i, "-----------------------------------------------------"))
  
  out_file <- file.path(tweets_all_dir, "data", "processed_data", "tweets_geocoded_chunks",
                        paste0("tweets_geocoded_chunk_",start_i,".Rds"))
  
  if(!file.exists(out_file) | OVERWRITE_FILE){
    
    # Save blank file. If running script on multiple computers/R sessions, others
    # will skip and go to next.
    saveRDS(data.frame(NULL), out_file) 
    
    end_i <- min(start_i + chunk_size - 1, length(tweets_df$tweet))
    
    tweets_df_i <- tweets_df[start_i:end_i,]
    
    alg_out_sf <- locate_event(text = tweets_df_i$tweet,
                               landmark_gazetteer = landmarks, 
                               roads = roads, 
                               areas = estates, 
                               prepositions_list = prepositions, 
                               prep_check_order = "prep_then_pattern", # prep_then_pattern
                               event_words = event, 
                               junction_words = junction, 
                               false_positive_phrases = false_positive, 
                               type_list = type_list, 
                               clost_dist_thresh = 500,
                               fuzzy_match = TRUE,
                               fuzzy_match.min_word_length = c(5,11),
                               fuzzy_match.dist = c(1,2),
                               fuzzy_match.ngram_max = 3,
                               fuzzy_match.first_letters_same = TRUE,
                               fuzzy_match.last_letters_same = TRUE,
                               crs_distance = "+init=epsg:21037", 
                               crs_out = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                               quiet = F,
                               mc_cores = 1)
    
    print("Prepping Output")
    # Restrict to points and add to tweets
    alg_out_sf$status_id_str <- tweets_df_i$status_id_str
    alg_out_sf$geometry_type <- alg_out_sf$geometry %>% st_geometry_type() %>% as.character()
    alg_out_sf$geometry_dim <- alg_out_sf$geometry %>% st_dimension()
    
    alg_out_df <- alg_out_sf %>%
      filter(geometry_type %in% "POINT") %>%
      filter(!is.na(geometry_dim)) %>%
      dplyr::select(-text) %>%
      as("Spatial") %>%
      as.data.frame() %>%
      dplyr::rename(lon_alg = coords.x1,
                    lat_alg = coords.x2)
    
    # Add no geo back in
    alg_out_sf_nopoint <- alg_out_sf[!(alg_out_sf$status_id_str %in% alg_out_df$status_id_str),]
    alg_out_sf_nopoint$geometry <- NULL
    alg_out_sf_nopoint <- alg_out_sf_nopoint %>%
      dplyr::select(status_id_str)
    
    alg_out_df <- bind_rows(alg_out_df,
                            alg_out_sf_nopoint)
    
    # Export -----------------------------------------------------------------------
    saveRDS(alg_out_df, out_file)
  }
}




