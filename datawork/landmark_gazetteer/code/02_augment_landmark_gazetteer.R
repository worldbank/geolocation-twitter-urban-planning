# Augment Gazetteer

# Loop Over Different Subsets Based on Gazetteer Source ------------------------
for(subset in c("all", "google", "osm", "geonames")){
  
  ## Load Data
  landmark_gaz <- readRDS(file.path(landmarkgaz_dir, "data", "gazetteers", "landmark_gazetter_raw.Rds")) 
  
  ## Subset based on source and create suffix to use in naming file
  if(subset != "all"){
    landmark_gaz <- landmark_gaz[landmark_gaz$source %in% subset,]
    subset_suffix <- paste0("_", subset)
  } else{
    subset_suffix <- ""
  }
  
  # Augment --------------------------------------------------------------------
  ## Augment Gazetteer
  landmark_gaz_aug <- augment_gazetteer(landmark_gaz,
                                        crs_distance = NAIROBI_UTM_PROJ,
                                        parallel.add_only_if_name_new = F,
                                        quiet=F)
  
  ## Remove list of specific landmarks
  landmarks_to_rm <- read.csv(file.path(landmarkgaz_dir, "data", "landmarks_to_remove.csv"), 
                              stringsAsFactors = F)
  
  landmark_gaz_aug <- landmark_gaz_aug[!(landmark_gaz_aug$name %in% landmarks_to_rm$name),]
  
  ## Change location of JKIA [put along entrance to JKIA, near mombasa rd, not middle of airport]
  landmark_gaz_aug_df <- landmark_gaz_aug %>% as.data.frame()
  
  landmark_gaz_aug_df$lat[landmark_gaz_aug_df$name == "jkia"] <- -1.344459 
  landmark_gaz_aug_df$lon[landmark_gaz_aug_df$name == "jkia"] <- 36.902554
  
  ## Back to spatial object
  coordinates(landmark_gaz_aug_df) <- ~lon+lat
  crs(landmark_gaz_aug_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Export ---------------------------------------------------------------------
  saveRDS(landmark_gaz_aug_df, file.path(landmarkgaz_dir, "data", "gazetteers",
                                         paste0("landmark_gazetter_aug",subset_suffix,".Rds")))
}

