# Crop Data to Nairobi and Save as Rds file for faster loading

# Load, Crop, Export -----------------------------------------------------------

# Define Nairobi Shapefile
kenya_adm1 <- readRDS(file.path(gadm_dir, "data", "gadm36_KEN_1_sp.rds"))
nairobi <- kenya_adm1[kenya_adm1$NAME_1 %in% c("Nairobi","Kiambu","Murang'a","Machakos"),]
nairobi$id <- 1
nairobi <- aggregate(nairobi, by="id")
nairobi <- gBuffer(nairobi, width=5/111.12, byid=T)

# List of OSM Shapefiles
files_shp <- list.files(file.path(osm_dir, "data", "raw_data", "geofabrik_download_20190317"),
           full.names=F, pattern="*.shp")
files_shp <- gsub('.{4}$', '', files_shp)

# Loop through OSM shapefiles: (1) load, (2) crop to Nairobi and (3) export as Rds
for(file in files_shp){
  print(file)
  
  shp <- readOGR(dsn=file.path(osm_dir, "data", "raw_data", "geofabrik_download_20190317"), layer=file)
  shp_nairobi <- crop(shp, nairobi) 
  
  saveRDS(shp_nairobi, 
          file.path(osm_dir, "data", "processed_data", "geofabrik_download_20190317_nairobi", 
                    paste0(file,"_nairobi.Rds")))
}


