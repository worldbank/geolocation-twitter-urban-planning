# Function to Cluster Crash Reports

# ***** Helper Functions -------------------------------------------------------

#### Define Function to Cluster Crashes
meanmin_ignore_na <- function(x,y,fun){
  # DESCRIPTION
    # Take a vector of numbers and returns the mean or min across vector pairings.
    # If one of the pairings is NA, ignore the NA and return the other number.
  
  if(length(x) != length(y)) stop("Length of x and y must be the same")
  
  if(is.na(x)){
    out <- y
  } else if(is.na(y)){
    out <- x
  } else{
    if(fun=="mean") out <- mean(c(x,y))
    if(fun=="min") out <- min(c(x,y))
  }
  
  return(out)
} 
meanmin_ignore_na <- Vectorize(meanmin_ignore_na)

#  ***** Crash from Dataset 1 Near Any Crash Dataset 2 -------------------------
crash1_near_crash2 <- function(crashes_df1, 
                               source_name_1, 
                               time_var_1 = "crash_nairobitime", 
                               lat_var_1 = "latitude",
                               lon_var_1 = "longitude",
                               crashes_df2, 
                               source_name_2, 
                               time_var_2 = "crash_nairobitime", 
                               lat_var_2 = "latitude",
                               lon_var_2 = "longitude",
                               time_thresh_hrs=4, 
                               cluster_km=0.5,
                               TF_vec = T,
                               keep_vars = NULL,
                               show_progress = F){
  # DESCRIPTION:
  # Determine if crash from dataset 1 is near any crashes from dataset 2. This 
  # uses a simple approach where crashes can be double counted; for example, 1
  # crash from dataset 2 could be near 2+ crashes from dataset 1.
  
  # PAREMTERS:
  # crashes_df1: Dataframe of crashes from source 1
  # source_name_1: String indicating crash source 1
  # lat_var_1: Name of variable from crash source 1 indicating latitude
  # lon_var_1: Name of variable from crash source 2 indicating longitude
  # time_thresh_hour: Time threshould (in hours) for clustering
  # cluster_km: Distance threshold (in kilometers) for clustering
  # TF_vec: Return a vector with TF; otherwise, returns df
  # keep_vars: Vars to keep in dataset two; if TF_vec is F, ignored
  
  # 1. Throw Errors ------------------------------------------------------------
  #### Check input types
  # Dataframes
  if(!is.data.frame(crashes_df1)) stop("crashes_df1 is not a data.frame")
  if(!is.data.frame(crashes_df2)) stop("crashes_df2 is not a data.frame")
  
  # Strings
  if(!is.character(source_name_1)) stop("source_name_1 is not a string")
  if(!is.character(source_name_2)) stop("source_name_2 is not a string")
  if(!is.character(time_var_1)) stop("time_var_1 is not a string")
  if(!is.character(time_var_2)) stop("time_var_2 is not a string")
  if(!is.character(lat_var_1)) stop("lat_var_1 is not a string")
  if(!is.character(lat_var_2)) stop("lat_var_2 is not a string")
  if(!is.character(lon_var_1)) stop("lon_var_1 is not a string")
  if(!is.character(lon_var_2)) stop("lon_var_2 is not a string")
  
  #### Check NAs in Variables
  if(sum(is.na(crashes_df1[[time_var_1]])) >= 1) stop("time_var_1 has NAs")
  if(sum(is.na(crashes_df2[[time_var_2]])) >= 1) stop("time_var_2 has NAs")
  
  if(sum(is.na(crashes_df1[[lat_var_1]])) >= 1) stop("lat_var_1 has NAs")
  if(sum(is.na(crashes_df2[[lat_var_2]])) >= 1) stop("lat_var_2 has NAs")
  
  if(sum(is.na(crashes_df1[[lon_var_1]])) >= 1) stop("lon_var_1 has NAs")
  if(sum(is.na(crashes_df2[[lon_var_2]])) >= 1) stop("lon_var_2 has NAs")
  
  # 2. Prep Datasets -----------------------------------------------------------
  crashes_df1$crash_id_1 <- paste0("c1_", 1:nrow(crashes_df1))
  crashes_df2$crash_id_2 <- paste0("c2_", 1:nrow(crashes_df2))
  
  crashes_df1$latitude <- crashes_df1[[lat_var_1]]
  crashes_df1$longitude <- crashes_df1[[lon_var_1]]
  crashes_df1$crash_time <- crashes_df1[[time_var_1]]
  
  crashes_df2$latitude <- crashes_df2[[lat_var_2]]
  crashes_df2$longitude <- crashes_df2[[lon_var_2]]
  crashes_df2$crash_time <- crashes_df2[[time_var_2]]
  
  # 3. Crashes Close Together --------------------------------------------------

  crash1_near_crash2 <- lapply(1:nrow(crashes_df1), function(i){
    df_out <- data.frame(matrix(nrow=1,ncol=0))
    
    if(show_progress){
      if((i %% 100)==0) print(i)
    }
    crashes_df1_i <- crashes_df1[i,]
    
    # Calculate difference in time and distance of crash_1_i to all crashes of crash_2
    crashes_df2$time_difference <- abs(difftime(crashes_df1_i$crash_time, crashes_df2$crash_time, units="hours"))
    crashes_df2$distance_difference <- sqrt((crashes_df1_i$latitude - crashes_df2$latitude)^2 + (crashes_df1_i$longitude - crashes_df2$longitude)^2)*111.12
    
    # Subset crash_2 to all crashes that are within time and distance threshold of crash_1_i
    crashes_df2_close <- crashes_df2[(crashes_df2$time_difference < time_thresh_hrs) & (crashes_df2$distance_difference < cluster_km),]
    
    df_out$crash_reported_othr_src <- (nrow(crashes_df2_close) > 0)
    
    # Keep vars
    if(!is.null(keep_vars) & !TF_vec & nrow(crashes_df2_close) > 0){
    
      for(var in keep_vars){
        df_out[[var]] <- crashes_df2_close[[var]] %>% paste(collapse = ";")
      }
      
    }
    
    return(df_out)
  }) %>% bind_rows()

  ## Return  
  if(TF_vec){
    return(crash1_near_crash2$crash_reported_othr_src)
  } else{
    return(crash1_near_crash2)
  }

} 

#  ***** Cluster Crashes Two Datasets ------------------------------------------
cluster_crashes_two_datasets <- function(crashes_df1, 
                            source_name_1, 
                            time_var_1 = "crash_nairobitime", 
                            lat_var_1 = "latitude",
                            lon_var_1 = "longitude",
                            crashes_df2, 
                            source_name_2, 
                            time_var_2 = "crash_nairobitime", 
                            lat_var_2 = "latitude",
                            lon_var_2 = "longitude",
                            time_thresh_hrs=4, 
                            cluster_km=0.5,
                            projected = F,
                            show_progress = F){
  # DESCRIPTION:
    # Clusters crashes from two datasets. Datasets may report the same crash. Each 
    # dataset is assumed to be a dataset of unique crashes. Clustering occurs by 
    # time and distance. Works in three steps:
    # 1. For each crash in dataset 1, grab all crashes close in dataset 2 
    # 2. For each crash in dataset 2, grab the closest crash from dataset 1
    # 3. For each crash in dataset 1, grab the closest crash from dataset 2

  # PAREMTERS:
    # crashes_df1: Dataframe of crashes from source 1
    # source_name_1: String indicating crash source 1
    # lat_var_1: Name of variable from crash source 1 indicating latitude
    # lon_var_1: Name of variable from crash source 2 indicating longitude
    # time_thresh_hour: Time threshould (in hours) for clustering
    # cluster_km: Distance threshold (in kilometers) for clustering
  
  # 1. Throw Errors ------------------------------------------------------------
  #### Check input types
  # Dataframes
  if(!is.data.frame(crashes_df1)) stop("crashes_df1 is not a data.frame")
  if(!is.data.frame(crashes_df2)) stop("crashes_df2 is not a data.frame")
  
  # Strings
  if(!is.character(source_name_1)) stop("source_name_1 is not a string")
  if(!is.character(source_name_2)) stop("source_name_2 is not a string")
  if(!is.character(time_var_1)) stop("time_var_1 is not a string")
  if(!is.character(time_var_2)) stop("time_var_2 is not a string")
  if(!is.character(lat_var_1)) stop("lat_var_1 is not a string")
  if(!is.character(lat_var_2)) stop("lat_var_2 is not a string")
  if(!is.character(lon_var_1)) stop("lon_var_1 is not a string")
  if(!is.character(lon_var_2)) stop("lon_var_2 is not a string")
  
  #### Check NAs in Variables
  if(sum(is.na(crashes_df1[[time_var_1]])) >= 1) stop("time_var_1 has NAs")
  if(sum(is.na(crashes_df2[[time_var_2]])) >= 1) stop("time_var_2 has NAs")
  
  if(sum(is.na(crashes_df1[[lat_var_1]])) >= 1) stop("lat_var_1 has NAs")
  if(sum(is.na(crashes_df2[[lat_var_2]])) >= 1) stop("lat_var_2 has NAs")
  
  if(sum(is.na(crashes_df1[[lon_var_1]])) >= 1) stop("lon_var_1 has NAs")
  if(sum(is.na(crashes_df2[[lon_var_2]])) >= 1) stop("lon_var_2 has NAs")
  
  # 2. Prep Datasets -----------------------------------------------------------
  crashes_df1$crash_id_1 <- paste0("c1_", 1:nrow(crashes_df1))
  crashes_df2$crash_id_2 <- paste0("c2_", 1:nrow(crashes_df2))
  
  crashes_df1$latitude <- crashes_df1[[lat_var_1]]
  crashes_df1$longitude <- crashes_df1[[lon_var_1]]
  crashes_df1$crash_time <- crashes_df1[[time_var_1]]

  crashes_df2$latitude <- crashes_df2[[lat_var_2]]
  crashes_df2$longitude <- crashes_df2[[lon_var_2]]
  crashes_df2$crash_time <- crashes_df2[[time_var_2]]
  
  # Rename varaibles with source name
  common_variables_1 <- c("crash_id_1", "latitude", "longitude", "crash_time")
  common_variables_2 <- c("crash_id_2", "latitude", "longitude", "crash_time")
  
  names(crashes_df1)[!(names(crashes_df1) %in% common_variables_1)] <- paste0(names(crashes_df1)[!(names(crashes_df1) %in% common_variables_1)],".",source_name_1)
  names(crashes_df2)[!(names(crashes_df2) %in% common_variables_2)] <- paste0(names(crashes_df2)[!(names(crashes_df2) %in% common_variables_2)],".",source_name_2)

  # 3. Crashes Close Together --------------------------------------------------
  # Create dataframe that includes
    # crash_id_1
    # crash_multiple_sources (if matched with any from crash_df2)
    # If crash_multiple_sources == T
      # crash_id_2*
      # time_difference
      # distance_difference
  crashes_to_cluster_df <- lapply(1:nrow(crashes_df1), function(i){
    if(show_progress){
      if((i %% 100)==0) print(i)
    }
    
    crashes_df1_i <- crashes_df1[i,]
    
    # Calculate difference in time and distance of crash_1_i to all crashes of crash_2
    crashes_df2$time_difference <- abs(difftime(crashes_df1_i$crash_time, crashes_df2$crash_time, units="hours"))
    if(projected){
      crashes_df2$distance_difference <- sqrt((crashes_df1_i$latitude - crashes_df2$latitude)^2 + (crashes_df1_i$longitude - crashes_df2$longitude)^2)/1000
    } else{
      crashes_df2$distance_difference <- sqrt((crashes_df1_i$latitude - crashes_df2$latitude)^2 + (crashes_df1_i$longitude - crashes_df2$longitude)^2)*111.12
    }
    
    # Subset crash_2 to all crashes that are within time and distance threshold of crash_1_i
    crashes_df2_close <- crashes_df2[(crashes_df2$time_difference < time_thresh_hrs) & (crashes_df2$distance_difference < (cluster_km)),]
    
    if(nrow(crashes_df2_close) > 0){
      df_out <- crashes_df2_close %>%
        dplyr::select(time_difference, distance_difference, crash_id_2) %>%
        dplyr::mutate(crash_id_1 = crashes_df1_i$crash_id_1,
                      crash_multiple_sources = TRUE)
    } else{
      df_out <- data.frame(matrix(nrow=1,ncol=0)) %>%
        dplyr::mutate(crash_id_1 = crashes_df1_i$crash_id_1,
                      crash_multiple_sources = FALSE)
    }
    
    return(df_out)
  }) %>% bind_rows
  crashes_to_cluster_df <- crashes_to_cluster_df[crashes_to_cluster_df$crash_multiple_sources %in% TRUE,]
  
  # 4. Cluster Crashes ---------------------------------------------------------
  clustered_crashes_all_df <- data.frame(NULL)
  
  while(nrow(crashes_to_cluster_df) > 0){
    ### Find closest pairs
    clustered_crashes_temp_df <- crashes_to_cluster_df %>%
      
      # For all observations in df_2 near df_1, find df_1 match
      group_by(crash_id_2) %>%
      slice(which.min(distance_difference)) %>%
      ungroup() %>%
      
      # For all observations in df_1 near df_2, find df_2 match
      group_by(crash_id_1) %>%
      slice(which.min(distance_difference)) %>%
      ungroup() 
    
    ### If ID is in a cluster, remove from to_cluster dataframe
    crashes_to_cluster_df <- crashes_to_cluster_df[!(crashes_to_cluster_df$crash_id_1 %in% clustered_crashes_temp_df$crash_id_1),]
    crashes_to_cluster_df <- crashes_to_cluster_df[!(crashes_to_cluster_df$crash_id_2 %in% clustered_crashes_temp_df$crash_id_2),]
    
    ### Update Clustered Dataframe
    clustered_crashes_all_df <- bind_rows(clustered_crashes_all_df, clustered_crashes_temp_df)
  }

  ### Add Source
  if(nrow(clustered_crashes_all_df) > 0) clustered_crashes_all_df$source <- paste0(source_name_1,",",source_name_2)
  
  # 5. Crashes only in df1 or df2 ----------------------------------------------
  crashed_only_in_df1 <- crashes_df1$crash_id_1[!(crashes_df1$crash_id_1 %in% clustered_crashes_all_df$crash_id_1)] %>%
    as.data.frame %>%
    dplyr::rename(crash_id_1 = ".") %>%
    dplyr::mutate(source = source_name_1)
  
  crashed_only_in_df2 <- crashes_df2$crash_id_2[!(crashes_df2$crash_id_2 %in% clustered_crashes_all_df$crash_id_2)] %>%
    as.data.frame %>%
    dplyr::rename(crash_id_2 = ".") %>%
    dplyr::mutate(source = source_name_2)
  
  crashes_df <- bind_rows(clustered_crashes_all_df, crashed_only_in_df1, crashed_only_in_df2)
  
  # 5. Add variables to dataframe ----------------------------------------------
  ### Just in case has a variable named source, remove it
  crashes_df1$source <- NULL
  crashes_df2$source <- NULL
  
  ### Merge data
  crashes_df <- merge(crashes_df, crashes_df1, all.x=T, all.y=F, by.x="crash_id_1", by.y="crash_id_1")
  crashes_df <- merge(crashes_df, crashes_df2, all.x=T, all.y=F, by.x="crash_id_2", by.y="crash_id_2")
  
  crashes_df$latitude <- meanmin_ignore_na(crashes_df$latitude.x, crashes_df$latitude.y,"mean")
  crashes_df$longitude <- meanmin_ignore_na(crashes_df$longitude.x, crashes_df$longitude.y,"mean")
  crashes_df$crash_time <- meanmin_ignore_na(crashes_df$crash_time.x, crashes_df$crash_time.y,"min")
  
  crashes_df$crash_time <- crashes_df$crash_time %>% as.POSIXct(origin="1970-01-01 00:00:00",tz="Africa/Nairobi")

  ### Remove variables ending in .x or .y
  # These are latitude, longitude and crash_time (variables created within algorithm)
  crashes_df <- crashes_df[,!grepl(".x$|.y$",names(crashes_df))]
  
  return(crashes_df)
} 

#  ***** Cluster Crashes One Dataset -------------------------------------------
cluster_crashes_one_dataset <- function(crashes_df, 
                                        time_var, 
                                        lat_var,
                                        lon_var ,
                                        vars_to_keep=NULL,
                                        time_thresh_hrs, 
                                        cluster_km ,
                                        cluster_id_only,
                                        show_progress = F){
  
  # 1. Prep Dataset ------------------------------------------------------------
  crashes_df$crash_time <- crashes_df[[time_var]]
  crashes_df$latitude <- crashes_df[[lat_var]]
  crashes_df$longitude <- crashes_df[[lon_var]]
  crashes_df$id <- 1:nrow(crashes_df)
  crashes_df$cluster_id <- NA
  
  # 2. Cluster all crashes within time/dist threshold --------------------------
  for(i in 1:nrow(crashes_df)){

    if(show_progress){
      if((i %% 100)==0) print(i)
    }
    
    
    crashes_df_i <- crashes_df[i,]
    within_time <- abs(difftime(crashes_df_i$crash_time, crashes_df$crash_time, units="hours")) <= time_thresh_hrs
    within_distance <- sqrt((crashes_df_i$longitude - crashes_df$longitude)^2 + (crashes_df_i$latitude - crashes_df$latitude)^2) <= (cluster_km/111.12)
    
    # If has a cluster ID, give cluster id
    if(!is.na(crashes_df_i$cluster_id)){
      crashes_df$cluster_id[within_time & within_distance] <- crashes_df_i$cluster_id
      
    # If doesn't have cluster ID, give ID.
    } else{
      crashes_df$cluster_id[within_time & within_distance] <- crashes_df_i$id
    }
  }
  
  # 3. Create Output from Clusters ---------------------------------------------
  #### Return cluster IDs
  if(cluster_id_only==T){
    return(crashes_df$cluster_id)
    
  #### Return shapefile of clusters  
  } else{
    
    ### Separate clusters into those of size 1 and size > 1
    crashes_df <- crashes_df %>%
      group_by(cluster_id) %>%
      mutate(cluster_id_freq = n()) %>%
      ungroup()
    
    crashes_df_size1 <- crashes_df[crashes_df$cluster_id_freq == 1,]
    crashes_df_sizeg1 <- crashes_df[crashes_df$cluster_id_freq > 1,]
    
    ### For clusters of size > 1, make into shapefile
    crashes_df_sizeg1 <- lapply(unique(crashes_df_sizeg1$cluster_id), function(cluster_id_i){
      cluster_i <- crashes_df[crashes_df$cluster_id %in% cluster_id_i,]
      coordinates(cluster_i) <- ~longitude+latitude
      
      # Create convex hull around points and by small amount. Buffer to account 
      # for cases where locations create a point (all locations are same) or a
      # line (two different points); buffering will make all observations a polygon.
      # Buffer by 1 meter (0.001km)
      cluster_i_polygon <- cluster_i %>% 
        gConvexHull %>%
        gBuffer(width=.001/111.12)
      
      # Add variables to cluster_i_polygon
      cluster_i_polygon$cluster_id <- cluster_id_i
      cluster_i_polygon$crash_time_min <- min(cluster_i$crash_time)
      cluster_i_polygon$crash_time_max <- max(cluster_i$crash_time)
      cluster_i_polygon$N_crash_reports <- nrow(cluster_i)
      
      # Additional vars to keep
      for(var in vars_to_keep){
        cluster_i_polygon[[var]] <- cluster_i[[var]] %>% paste(collapse=" ")
      }
      
      return(cluster_i_polygon)

    }) %>% do.call(what="rbind")
    
    ### For clusters of size 1, make into shapefile
    coordinates(crashes_df_size1) <- ~longitude+latitude
    crashes_df_size1 <- gBuffer(crashes_df_size1, width=.001/111.12, byid=T)
    crashes_df_size1$crash_time_min <- crashes_df_size1$crash_time
    crashes_df_size1$crash_time_max <- crashes_df_size1$crash_time
    crashes_df_size1@data <- crashes_df_size1@data %>%
      dplyr::select(c("crash_time_min", "crash_time_max", "cluster_id", vars_to_keep)) %>%
      dplyr::mutate(N_crash_reports = 1)
    
    ### Append Crashes of size 1 and size > 1
    crashes_df <- rbind(crashes_df_size1, crashes_df_sizeg1)
    
    return(crashes_df)
  }
  
}






