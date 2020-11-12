# Append Files for Raw Geonames Gazetteer

# Load Data --------------------------------------------------------------------
## Nairobi
ken.adm1 <- readRDS(file.path(gadm_dir, "data", "gadm36_KEN_1_sp.rds"))
ken.adm1 <- ken.adm1[ken.adm1$NAME_1 %in% c("Nairobi","Kiambu","Murang'a","Kajiado","Machakos"),]

## Geonames
geonames.df <- read.csv(file.path(geonames_dir, "data", "raw_data", "KE.txt"), 
                        sep="\t", 
                        header=F, 
                        stringsAsFactors=F)
names(geonames.df) <- c("geonameid",
                        "name",
                        "asciiname",
                        "alternatenames",
                        "latitude",
                        "longitude",
                        "featureclass",
                        "featurecode",
                        "countrycode",
                        "cc2",
                        "admin1code",
                        "admin2code",
                        "admin3code",
                        "admin4code",
                        "population",
                        "elevation",
                        "dem",
                        "timezone",
                        "modificationdate")

# Merge in Types
featureclass.df <- read_excel(file.path(geonames_dir, "data", "raw_data","geonames_features.xlsx"), "featureclass")
featurecode.df <- read_excel(file.path(geonames_dir, "data", "raw_data","geonames_features.xlsx"), "featurecode")
geonames.df <- merge(geonames.df, featureclass.df, by="featureclass")
geonames.df <- merge(geonames.df, featurecode.df, by="featurecode")

# Restrict to Nairobi
geonames.df$lat <- geonames.df$latitude
geonames.df$lon <- geonames.df$longitude
coordinates(geonames.df) <- ~longitude+latitude
crs(geonames.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

geonames.df_OVER_ken.adm1 <- over(geonames.df, ken.adm1)
geonames.df$ISO <- geonames.df_OVER_ken.adm1$NAME_0
geonames.df <- geonames.df[!is.na(geonames.df$ISO),]
geonames.df <- geonames.df@data

# Remove certain types of landmarks --------------------------------------------
geonames.df <- geonames.df[!geonames.df$featureclass %in% "A",]

# Make names match googe var names ---------------------------------------------
geonames.df <- subset(geonames.df, select=c(asciiname, lat, lon, featurecode_description, alternatenames))
geonames.df$asciiname <- geonames.df$asciiname %>% as.character
geonames.df$alternatenames <- geonames.df$alternatenames %>% as.character

# Add dataset of alternate names  ----------------------------------------------
make_alternate_names_df <- function(i, df){
  if(df$alternatenames[i] == ""){
    df.out <- data.frame(lon = integer()) # blank dataframe
    return(df.out)
  } else{
    alt_names <- strsplit(df$alternatenames[i],",")[[1]] %>% as.data.frame
    names(alt_names) <- c("asciiname")
    alt_names$id_temp <- 1
    
    df_alt <- df[i,]
    df_alt <- subset(df_alt, select=-c(asciiname))
    df_alt$id_temp <- 1
    
    df.out <- merge(alt_names, df_alt, by="id_temp")
    df.out <- subset(df.out, select=-c(id_temp))
    return(df.out)
  }
}

geonames_alt_df <- lapply(1:nrow(geonames.df), make_alternate_names_df,geonames.df) %>% dplyr::bind_rows()
geonames.df <- rbind(geonames.df, geonames_alt_df)
geonames.df <- unique(geonames.df)

geonames.df <- subset(geonames.df, select=-c(alternatenames))
names(geonames.df) <- c("landmark","lat","lon","type")

# Basic Cleaning ---------------------------------------------------------------
geonames.df <- geonames.df %>%
  dplyr::mutate(landmark = landmark %>% tolower(),
         source = "geonames") %>%
  dplyr::rename(name = landmark) 

# Export -----------------------------------------------------------------------
saveRDS(geonames.df, file.path(geonames_dir, "data", "processed_data", "geonames_nairobi.Rds"))

