# Cluster Crashes

# Load Data --------------------------------------------------------------------
crashes_df <- readRDS(file.path(tweets_all_dir, "data", "processed_data", 
                                "tweets_classified_geoparsed_uniquecrashes.Rds"))

## Spatially Define
coordinates(crashes_df) <- ~longitude+latitude
crs(crashes_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Restrict to Nairobi 
kenya <- readRDS(file.path(gadm_dir, "data", "gadm36_KEN_1_sp.rds"))
nairobi <- kenya[kenya$NAME_1 %in% "Nairobi",]

crashes_df <- crashes_df[over(crashes_df, nairobi)$NAME_1 %in% "Nairobi",]

## Project
crashes_df <- spTransform(crashes_df, CRS(NAIROBI_UTM_PROJ))

## Back to df
crashes_df <- crashes_df %>% as.data.frame()

# Cluster Crashes --------------------------------------------------------------
crashes_df_cluster <- create_crash_clusters(crashes_df = crashes_df, 
                                            lat_var = "latitude", 
                                            lon_var = "longitude", 
                                            distance = 300, 
                                            sp_out = F)

# Export -----------------------------------------------------------------------
saveRDS(crashes_df_cluster,
        file.path(tweets_all_dir, "data", "processed_data", 
                                "tweets_classified_geoparsed_uniquecrashes_crashcluster.Rds"))

