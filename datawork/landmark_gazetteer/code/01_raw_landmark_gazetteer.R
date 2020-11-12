# Append Files for Raw Gazetteer

# Load Data --------------------------------------------------------------------
googleplaces <- readRDS(file.path(google_places_dir, "data", "google_places.Rds"))
geonames     <- readRDS(file.path(geonames_dir, "data", "processed_data", "geonames_nairobi.Rds"))
osm          <- readRDS(file.path(osm_dir, "data", "processed_data", "landmarks", "osm_landmarks.Rds"))

landmarks_all <- bind_rows(googleplaces, osm, geonames)
landmarks_all <- unique(landmarks_all)

# Remove Some Landmarks --------------------------------------------------------
# Here we remove certain types, like roads that do not represent specific locations

# Remove roads
rm_ends_with <- paste0("\\b", c("rd", "road", "st", "street", "way", "highway", "bypass", "drive", "avenue"), "$") %>% paste(collapse = "|")

landmarks_all <- landmarks_all[!(grepl("route", landmarks_all$type) & grepl(rm_ends_with, landmarks_all$name)),]
landmarks_all <- landmarks_all[!(grepl("geonames", landmarks_all$source) & grepl(rm_ends_with, landmarks_all$name)),]

# Remove these specific ones
landmarks_all <- landmarks_all[!(landmarks_all$name %in% c("junction", "road", "nairobi", "stage",
                                                           "mombasa road", "kenyatta road")),]

# Remove really short ones
landmarks_all <- landmarks_all[nchar(landmarks_all$name) >= 2,]

## Add variables
landmarks_all$general_specific <- NA
landmarks_all$name_original <- landmarks_all$name

# Spatially Define -------------------------------------------------------------
coordinates(landmarks_all) <- ~lon+lat
crs(landmarks_all) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Export -----------------------------------------------------------------------
saveRDS(landmarks_all, file.path(landmarkgaz_dir, "data", "gazetteers", "landmark_gazetter_raw.Rds")) 
write.csv(landmarks_all, file.path(landmarkgaz_dir, "data", "gazetteers", "landmark_gazetter_raw.csv"),
          row.names = F) 

