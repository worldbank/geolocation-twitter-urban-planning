# Geoparse Tweets
# Clean Tweets

# Cleans text of tweets and subsets tweets for geoparsing

# Load Data --------------------------------------------------------------------
tweets_df <- readRDS(file.path(tweets_all_dir, "data", "raw_data", "tweets.Rds"))

tweets_df <- tweets_df %>%
  filter(accident_truth %in% T,
         !is.na(tweets_df$latitude_truth),
         !is.na(tweets_df$longitude_truth)) %>%
  dplyr::rename(tweet = text) %>%
  dplyr::select(tweet, latitude_truth, longitude_truth, status_id_str, name, created_at_nairobitime)

# Clean Tweets -----------------------------------------------------------------
# This is done by the algorithm, but important to do here so we get a more
# accurate comparison between the algorithm and LNEx. For example, LNEx picks
# up on some spurious landmarks based on time (e.g., 10 23..., finds the 
# landmark "10")

tweets_df$tweet <- tweets_df$tweet %>%
  
  iconv("latin1", "ASCII", sub="") %>%
  
  str_to_lower %>%
  
  # Remove numbers at beginning
  # Squish strings. Should be last thing done
  str_squish() %>%
  str_replace_all("^[:digit:]|^ [:digit:]", "") %>% 
  str_replace_all("^[:digit:]|^ [:digit:]", "") %>% 
  str_replace_all("^[:digit:]|^ [:digit:]", "") %>% 
  str_squish() %>%
  str_replace_all("^[:digit:]|^ [:digit:]", "") %>% 
  str_replace_all("^[:digit:]|^ [:digit:]", "") %>% 
  str_replace_all("^[:digit:]|^ [:digit:]", "") %>% 
  str_replace_all("^[:digit:]|^ [:digit:]", "") %>% 
  
  ### Remove @ selectively. Want to remove user mentions but keep others
  str_replace_all(" @ ", " at ") %>% # if @ not next to work, make "at"
  str_replace_all("via @[a-z_,A-Z_,0-9_]*", "") %>% # remove via @text
  
  str_squish() %>%
  str_replace_all("@[a-z_,A-Z_,0-9_]*$", "") %>% str_squish() %>% # remove @text [end of tweet]
  str_replace_all("@[a-z_,A-Z_,0-9_]*$", "") %>% str_squish() %>% # repeat in case multiple...
  str_replace_all("@[a-z_,A-Z_,0-9_]*$", "") %>% str_squish() %>%
  str_replace_all("@[a-z_,A-Z_,0-9_]*$", "") %>% str_squish() %>%
  str_replace_all("@[a-z_,A-Z_,0-9_]*$", "") %>% str_squish() %>%
  
  str_replace_all("^@[a-z_,A-Z_,0-9_]*", "") %>% str_squish() %>% # remove [beginning of tweet] @text 
  str_replace_all("^@[a-z_,A-Z_,0-9_]*", "") %>% str_squish() %>% # repeat in case multiple...
  str_replace_all("^@[a-z_,A-Z_,0-9_]*", "") %>% str_squish() %>%
  str_replace_all("^@[a-z_,A-Z_,0-9_]*", "") %>% str_squish() %>%
  str_replace_all("^@[a-z_,A-Z_,0-9_]*", "") %>% str_squish() %>%
  
  str_replace_all("@", " at ") %>% # all other @ turn to at
  
  #str_replace_all("\\@", "at ") %>% # "@cabanas, saying crash is at cabanas" OR, to deemphasize, could just replace @ with ""
  #str_replace_all("\\b@\\b", " at ") %>% ?? not working?
  str_replace_all(","," , ") %>% # Add space between commas (eg, "road,allsops") 
  str_replace_all("-", " ") %>%
  str_replace_all("\\.", " ") %>%
  str_replace_all("\n", "") %>%
  str_replace_all("~", "") %>%
  str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]*\\b", "") %>%
  str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]", "") %>%
  str_replace_all("\\b(http|https)://t.co\\b", "") %>%
  str_replace_all("\\b(http|https):", "") %>%
  str_replace_all("~more*", "") %>%
  str_replace_all("(RT|rt) @[a-z,A-Z,0-9, _]*:", "") %>%
  str_replace_all("^[0-9][0-9]\\:[0-9][0-9]", "") %>%
  str_replace_all("\\+", " ") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("\\bamp\\b", "and") %>%
  str_replace_all("via at.*", "") %>% # remove everything include and after 
  #                                     "via at", which comes at end
  str_replace_all("https.*", "") %>% # remove everything include and after 
  #                                   "https", which comes at end
  
  # Squish strings. Should be last thing done
  str_squish()

# Remove short tweets
tweets_df <- tweets_df[nchar(tweets_df$tweet) >= 3,]

# Export ------------------------------------------------------------------  
saveRDS(tweets_df, file.path(tweets_geoparse_dir, "processed_data", "tweets_for_geolocation.Rds"))
write.csv(tweets_df, file.path(tweets_geoparse_dir, "processed_data", "tweets_for_geolocation.csv"),
          row.names = F)


