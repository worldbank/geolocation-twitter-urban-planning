# Append Files for Raw Geonames Gazetteer

# Load Data from Geofabrik -----------------------------------------------------
read_remove_na <- function(dataset){
  df <- readRDS(file.path(file.path(osm_dir, "data", "processed_data", "geofabrik_download_20190317_nairobi", dataset)))
  df <- df[!is.na(df$name),]
  
  df_coordinates <- gCentroid(df, byid=T) %>% 
    coordinates %>%
    as.data.frame %>%
    dplyr::rename(lon = x) %>%
    dplyr::rename(lat = y)
  
  df <- df@data
  df <- cbind(df, df_coordinates)
  
  return(df)
}

osm_landmarks_geofabrik <- lapply(c("gis_osm_buildings_a_free_1_nairobi.Rds",
                          "gis_osm_landuse_a_free_1_nairobi.Rds",
                          "gis_osm_places_a_free_1_nairobi.Rds",
                          "gis_osm_pofw_a_free_1_nairobi.Rds",
                          "gis_osm_pofw_free_1_nairobi.Rds",
                          "gis_osm_pois_a_free_1_nairobi.Rds", 
                          "gis_osm_pois_free_1_nairobi.Rds", 
                          "gis_osm_traffic_a_free_1_nairobi.Rds",
                          "gis_osm_traffic_free_1_nairobi.Rds", 
                          "gis_osm_transport_a_free_1_nairobi.Rds",
                          "gis_osm_transport_free_1_nairobi.Rds"), read_remove_na) %>% bind_rows

osm_landmarks_geofabrik$type <- as.character(osm_landmarks_geofabrik$type)
osm_landmarks_geofabrik$fclass <- as.character(osm_landmarks_geofabrik$fclass)
osm_landmarks_geofabrik$type[is.na(osm_landmarks_geofabrik$type)] <- osm_landmarks_geofabrik$fclass[is.na(osm_landmarks_geofabrik$type)]

# Load Data from Overpass API --------------------------------------------------
osm_buildings <- readRDS(file.path(osm_dir, "data", "processed_data", "overpass_api_extracts", "buildings.Rds"))

# Append -----------------------------------------------------------------------
osm_landmarks <- bind_rows(osm_landmarks_geofabrik, osm_buildings)

# Subset Variables -------------------------------------------------------------
osm_landmarks <- osm_landmarks %>% 
  dplyr::select(lat,lon,name,type) %>%
  mutate(source = "osm")

osm_landmarks$name <- osm_landmarks$name %>% tolower
osm_landmarks <- unique(osm_landmarks)

# Basic Cleaning ---------------------------------------------------------------
osm_landmarks$name <- osm_landmarks$name %>%
  tolower()

osm_landmarks <- osm_landmarks[!is.na(osm_landmarks$name),]

# Export -----------------------------------------------------------------------
saveRDS(osm_landmarks, file.path(osm_dir, "data", "processed_data", "landmarks", "osm_landmarks.Rds"))

