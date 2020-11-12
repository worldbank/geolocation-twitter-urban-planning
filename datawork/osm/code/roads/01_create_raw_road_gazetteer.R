# Clean Roads Shapefile from OSM

# Load and Clean Data ----------------------------------------------------------
nairobi_roads <- readRDS(file.path(osm_dir, "data", "processed_data", "geofabrik_download_20190317_nairobi", 
                                   "gis_osm_roads_free_1_nairobi.Rds"))

# Remove roads without a name
nairobi_roads <- nairobi_roads[!is.na(nairobi_roads$name),]

# Export -----------------------------------------------------------------------
saveRDS(nairobi_roads, file.path(osm_dir, "data", "processed_data", "roads", "osm_roads.Rds"))


