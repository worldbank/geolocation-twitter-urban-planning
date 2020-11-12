# Download OSM Data from Overpass API

# Downlaod buildings from OSM Overpass API. Export as dataframe with name, type
# and coordinates.

# Overpass API: Extraction -----------------------------------------------------
## Extract Buildings
buildings <- opq("nairobi, kenya") %>% 
  add_osm_feature(key = 'building') %>%
  osmdata_sf()

## Create separate sp files for points/polgons
buildings_points <- buildings$osm_points %>% as("Spatial")
buildings_polygons <- buildings$osm_polygons %>% as("Spatial")

# To Dataframe -----------------------------------------------------------------
## Spatial Points to Dataframe
buildings_points_df <- buildings_points %>% 
  as.data.frame %>%
  dplyr::rename(lon = coords.x1) %>%
  dplyr::rename(lat = coords.x2)

## Spatial Polygon to Dataframe
buildings_polygons_df <- buildings_polygons %>%
  gCentroid(byid=T) %>% 
  as.data.frame %>%
  dplyr::rename(lon = x) %>%
  dplyr::rename(lat = y) %>%
  bind_cols(buildings_polygons@data)

## Append
buildings_df <- bind_rows(buildings_points_df,
                          buildings_polygons_df)

# Clean Data -------------------------------------------------------------------
buildings_df <- buildings_df %>%
  filter(!is.na(name)) %>%
  mutate(type = "building") %>%
  dplyr::select(type, name, lat, lon)

# Export -----------------------------------------------------------------------
saveRDS(buildings_df, file.path(osm_dir, "data", "processed_data", "overpass_api_extracts", "buildings.Rds"))
