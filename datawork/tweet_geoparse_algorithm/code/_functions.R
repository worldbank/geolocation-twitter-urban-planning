# Geoparse Tweets
# Functions

spread_coords <- function(df,
                          var,
                          crs_in = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                          crs_out){
  # From a string of coordinates separated by a semicolon, converts to long format.
  # The algorithm output includes a variable of all locations found. The variable is
  # structured as coordinates, separated by a semicolon. So locations -1.1,10.1
  # and -3.1,11.1 would be written as: -1.1,10.1;-3.1,11.1 in the variable.
  # This function splits the coordinates and creates a dataframe with a row
  # for each location found.
  # PARAMS
  # df: Dataframe
  # var: Name of variable with coordinates in dataframe
  # crs_in: Coordinate reference system of coordinates
  # crs_out: Coordinate reference system to transform coordinates to
  
  alg_coords <- df[[var]] %>% 
    as.character() %>%
    str_replace_all(" ", "") %>%
    str_replace_all("-[[:alpha:]]", "") %>%
    str_replace_all("[[:alpha:]]", "") %>%
    str_replace_all(" ", "") %>%
    str_replace_all(";,", ";") %>%
    str_replace_all("^;", "") %>%
    str_replace_all("^,", "") %>%
    str_replace_all("^,", "") %>%
    str_split(";") %>%
    lapply(t %>% as.data.frame) %>%
    bind_rows() %>%
    bind_cols(df) %>% 
    dplyr::select(contains("value"), "status_id_str") %>%
    pivot_longer(cols=starts_with("value")) %>%
    filter(!is.na(value)) %>%
    separate(col = "value", into = c("lat", "lon"), sep=",") %>%
    filter(!is.na(lat),
           !is.na(lon),
           !(lat %in% ""),
           !(lon %in% ""))
  
  alg_coords$lat <- alg_coords$lat %>% as.numeric()
  alg_coords$lon <- alg_coords$lon %>% as.numeric()
  
  coordinates(alg_coords) <- ~lon+lat
  crs(alg_coords) <- CRS(crs_in)
  
  alg_coords <- alg_coords %>% spTransform(CRS(crs_out)) %>% as.data.frame()
  alg_coords <- alg_coords %>%
    dplyr::select(status_id_str, lat, lon)
  
  return(alg_coords)
}

prep_alg_data <- function(df, type_name){
  # Preps algorithm data. The algorithm output contains a variable which includes
  # coordinates of all landmarks found and an additional variable of all
  # road intersections found. This function creates a long dataframe, where
  # each row is one of those coordinates.
  # PARAMS
  # df: dataframe from algorithm
  # type_name: string. Functions adds variable "type" with this text
  
  ## Alg Coordinates
  df_alg <- df %>%
    dplyr::select(status_id_str, lat_alg, lon_alg) %>%
    dplyr::rename(lat = lat_alg,
                  lon = lon_alg)
  
  df_landmarks <- spread_coords(df = df[df$landmarks_all_location != "",],
                                var = "landmarks_all_location",
                                crs_in = NAIROBI_UTM_PROJ,
                                crs_out = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  df_intersections <- spread_coords(df = df[df$intersection_all_location != "",],
                                    var = "intersection_all_location",
                                    crs_in = NAIROBI_UTM_PROJ,
                                    crs_out = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  #### Prep Data
  ## Remove coorindates
  
  df$dist_closest_crash_word <- df$dist_closest_crash_word %>% str_replace_all(";.*", "")
  df$dist_closest_crash_word[is.na(df$dist_closest_crash_word) | df$dist_closest_crash_word %in% "NA"] <- "none"
  
  
  df$rm <- F
  df$rm[grepl("tried_to_snapped_to_road_but_road_too_far", df$how_determined_landmark)] <- T
  df$rm[grepl("multiple_landmarks_choose_closest_crashword|tier_5|tier_6", df$how_determined_landmark) &
          df$dist_closest_crash_word %in% as.character(3:26)] <- T
  
  # df$rm[grepl("tried_to_snapped_to_road_but_road_too_far|tried_restricting_landmarks_close_to_road_but_none_close|tier_5", df$how_determined_landmark)] <- T
  # df$rm[grepl("restrict_by_type_shoe_store|restrict_by_type_liquor_store|multiple_landmarks_tried_restrict_to_near_roads_but_none_near_road|crashword_tier_7_preposition_intersection|crashword_tier_crashword_prepos_tier_4preposition_landmark", df$how_determined_landmark)] <- T
  # df$rm[grepl("multiple_landmarks_choose_closest_crashword", df$how_determined_landmark) &
  #         df$dist_closest_crash_word %in% as.character(3:26)] <- T
  # df$rm[grepl("landmark_ambiguous_pattern", df$how_determined_landmark) &
  #         df$dist_closest_crash_word %in% as.character(9:26, "none")] <- T
  
  df$lat_alg[df$rm %in% T] <- NA
  df$lon_alg[df$rm %in% T] <- NA
  
  ## Select Variables
  df_data <- df %>%
    dplyr::select(status_id_str, lat_alg, lon_alg)
  
  #### Bind/Merge
  df_coords <- bind_rows(df_alg,
                         df_landmarks,
                         df_intersections)
  
  df_coords <- df_coords %>%
    left_join(df_data, by = "status_id_str")
  
  df_coords$type <- type_name
  
  return(df_coords)
}

reproj_coords <- function(df,
                          lat_var,
                          lon_var,
                          crs_in = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                          crs_out = "+init=epsg:21037"){
  # Reprojects coordinates in a dataframe. Accounts for cases where the
  # lat/lon vars may be NA for some observations.
  # PARAMS
  # df: Dataframe
  # lat_var: Name of latitude variable
  # lon_var: Name of longitude variable
  # crs_in: Coordinate reference system of coordinates
  # crs_out: Coordinate reference system to project to
  
  #### Prep Df
  df$uid <- 1:nrow(df)
  
  df_coords <- df
  
  df_coords$lat_temp <- df[[lat_var]]
  df_coords$lon_temp <- df[[lon_var]]
  
  #df_coords <- df_coords[,]
  #df_coords <- df_coords[,]
  
  df_coords <- df_coords %>%
    dplyr::filter(!is.na(df_coords$lat_temp),
                  !is.na(df_coords$lon_temp)) %>%
    dplyr::select(uid, lat_temp, lon_temp)
  
  #### Reproject
  coordinates(df_coords) <- ~lon_temp + lat_temp
  crs(df_coords) <- CRS(crs_in)
  df_coords <- spTransform(df_coords, CRS(crs_out))
  df_coords_latlon <- as.data.frame(df_coords)
  
  #### Rename
  names(df_coords_latlon)[names(df_coords_latlon) %in% "lon_temp"] <- lon_var
  names(df_coords_latlon)[names(df_coords_latlon) %in% "lat_temp"] <- lat_var
  
  #### Add to dataframe
  df[[lat_var]] <- NULL
  df[[lon_var]] <- NULL
  
  df <- merge(df, df_coords_latlon, by = "uid", all.x=T)
  
  return(df)
}

#### Define function for clustering
cluster_truth_algorithm <- function(alg_truth_stacked_df, CLUSTER_HOURS_THRESH, CLUSTER_METERS){
  # For clustering, stack algorithm and truth tweets into one dataset. Implement 
  # clustering algorithm on this dataset. See the number of clusters: 
  #  (1) with both algorithm and truth locations [AT]
  #  (2) with just truth locations and [T]
  #  (3) with just algorithm locations [A.
  # From this we can determine recall and precision.
  #  -- Precision: AT / (T + AT)
  #  -- Recall: AT / (A + AT)
  
  alg_truth_stacked_df$id <- 1:nrow(alg_truth_stacked_df)
  alg_truth_stacked_df$cluster_id <- NA
  
  for(i in 1:nrow(alg_truth_stacked_df)){
    alg_truth_stacked_df_i <- alg_truth_stacked_df[i,]
    
    within_time <- abs(difftime(alg_truth_stacked_df_i$created_at_nairobitime, alg_truth_stacked_df$created_at_nairobitime, units="hours")) <= CLUSTER_HOURS_THRESH
    within_distance <- sqrt((alg_truth_stacked_df_i$longitude - alg_truth_stacked_df$longitude)^2 + (alg_truth_stacked_df_i$latitude - alg_truth_stacked_df$latitude)^2) <= (CLUSTER_METERS)
    
    # If has a cluster ID, give cluster id
    if(!is.na(alg_truth_stacked_df_i$cluster_id)){
      alg_truth_stacked_df$cluster_id[within_time & within_distance] <- alg_truth_stacked_df_i$cluster_id
      
      # If doesn't have cluster ID, give ID.
    } else{
      alg_truth_stacked_df$cluster_id[within_time & within_distance] <- alg_truth_stacked_df_i$id
    }
    
  }
  
  results <- alg_truth_stacked_df %>%
    group_by(cluster_id) %>%
    dplyr::summarise(type = type %>% unique %>% paste(collapse=";")) %>%
    ungroup %>%
    group_by(type) %>%
    dplyr::summarise(N = n()) %>%
    mutate(cluster_hrs = CLUSTER_HOURS_THRESH,
           cluster_meters = CLUSTER_METERS)
  
  #  -- Precision: AT / (T + AT)
  #  -- Recall: AT / (A + AT)
  
  precision <- results$N[results$type %in% "algorithm;truth"] / 
    (results$N[results$type %in% "algorithm"] + results$N[results$type %in% "algorithm;truth"])
  
  recall <- results$N[results$type %in% "algorithm;truth"] / 
    (results$N[results$type %in% "truth"] + results$N[results$type %in% "algorithm;truth"])
  
  cluster_prec_recall <- data.frame(clstr_precision = precision,
                                    clstr_recall = recall)
  
  return(cluster_prec_recall)
}


calc_cluster_results <- function(type, df, CLUSTER_HOURS_THRESH, CLUSTER_METERS){
  
  print(type)
  
  #### Type
  df <- df[df$type %in% type,]
  
  #### Stack datasets
  alg_truth_stacked_df <- bind_rows(
    df %>%
      dplyr::rename(latitude = lat_alg,
                    longitude = lon_alg) %>%
      dplyr::select(latitude, longitude, created_at_nairobitime) %>%
      filter(!is.na(latitude),
             !is.na(longitude)) %>%
      mutate(type = "algorithm"),
    df %>%
      dplyr::rename(latitude = lat_truth,
                    longitude = lon_truth) %>%
      dplyr::select(latitude, longitude, created_at_nairobitime) %>%
      mutate(type = "truth")
  )
  
  df_out <- cluster_truth_algorithm(alg_truth_stacked_df, CLUSTER_HOURS_THRESH, CLUSTER_METERS)
  df_out$type <- type
  
  return(df_out)
}


