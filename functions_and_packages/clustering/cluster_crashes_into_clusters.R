# Function to CLuster Unique Crashes into Crash Clusters

create_crash_clusters <- function(crashes_df,
                                  lat_var,
                                  lon_var,
                                  distance,
                                  sp_out = T){
  
  # DESCRIPTION:
  # Cluster unique crashes into crash clusters. Grab all crashes within 
  # [near_crash_thresh_meters] meters, not allowing the maximum distance of the
  # cluster to grow beyond [near_anycrash_in_cluster_thresh_meters]. 
  
  # PARAMETERS
  # crashes_df: Crash dataframe
  # lat_var: Name of variable indicating latitude
  # lon_var: Name of variable indicating longitude
  # distance: Distance for clustering
  # sp_out: If T, returns sp object; if F, returns dataframe
  
  ## Common name for lat/lon
  crashes_df$longitude <- crashes_df[[lon_var]]
  crashes_df$latitude <- crashes_df[[lat_var]]
  
  ## Cluster ID
  crashes_dist <- crashes_df[,c("latitude", "longitude")] %>% dist()
  crashes_df$cluster_id <- hclust(crashes_dist, method = "ward.D2") %>%
    cutree(h = 300)
  
  ## Create Convex Hull
  if(sp_out){
    coordinates(crashes_df) <- ~longitude+latitude
    
    # Aggregate
    crashes_df <- gBuffer(crashes_df, byid=T, width=0.000001)
    crashes_df$N_crashes <- 1
    crashes_cluster <- raster::aggregate(crashes_df, by="cluster_id", 
                                         sums=list(list(sum, 'N_crashes')))
    
    # Convex Hull
    crashes_cluster_hull <- gConvexHull(crashes_cluster, byid = T)
    crashes_cluster_hull$id <- 1:length(crashes_cluster_hull)
    crashes_cluster_hull$N_crashes <- crashes_cluster$N_crashes
    
    out <- crashes_cluster_hull
  } else{
    out <- crashes_df %>%
      group_by(cluster_id) %>%
      dplyr::summarise(N_crashes = n()) 
  }
  

  return(out)
}







