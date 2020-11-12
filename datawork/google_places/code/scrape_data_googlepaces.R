# Create Gazeteer from Google Maps, Using Google Places API

#### Parameters
grid.cell.size <- 50 # meters
api.search.radius <- 50 # meteres
expand.nairobi <- 2 #km
API_KEY <- "API-KEY-HERE"

# Area to Extract Landmarks ----------------------------------------------------
## Nairobi
kenya <- readRDS(file.path(gadm_dir, "data", "gadm36_KEN_1_sp.rds"))
nairobi <- kenya[kenya$NAME_1 %in% "Nairobi",]
nairobi <- gBuffer(nairobi, width=expand.nairobi/111.12) 

## Make Grid
nairobi.grid <- makegrid(nairobi, cellsize=(grid.cell.size/1000)/111.12)
nairobi.grid$val <- 1
coordinates(nairobi.grid) <- ~x1+x2
crs(nairobi.grid) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
nairobi.grid <- nairobi.grid[!is.na(over(nairobi.grid, nairobi)),]

nairobi.grid$lon <- coordinates(nairobi.grid)[,1]
nairobi.grid$lat <- coordinates(nairobi.grid)[,2]
nairobi.grid <- nairobi.grid@data

# Function to Get Dataframe from API Result ------------------------------------
list.to.str <- function(list) paste(list, collapse=";")

api.result.to.df <- function(rd){
  # Function to convert Google Places API result into dataframe
  rd.df <- as.data.frame(rd$results$geometry$location)
  rd.df$northeast.lat <- rd$results$geometry$viewport$northeast$lat
  rd.df$northeast.lng <- rd$results$geometry$viewport$northeast$lng
  rd.df$southwest.lat <- rd$results$geometry$viewport$southwest$lat
  rd.df$southwest.lng <- rd$results$geometry$viewport$southwest$lng
  rd.df$name <- rd$results$name
  rd.df$id <- rd$results$id
  rd.df$place_id <- rd$results$place_id
  rd.df$scope <- rd$results$scope
  rd.df$types <- lapply(rd$results$types, list.to.str) %>% unlist
  rd.df$vicinity <- rd$results$vicinity
  return(rd.df)
}

# Scraping ---------------------------------------------------------------------
plcUrl <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

# Initialize Dataframe
landmark.df.list <- list()

start.time.script <- Sys.time()

for(i in 1:nrow(nairobi.grid)){
  
  # Initial Scrape
  query.url <- paste0(plcUrl,
                      "location=", 
                      nairobi.grid$lat[i], 
                      ",", 
                      nairobi.grid$lon[i], 
                      "&radius=", 
                      api.search.radius,
                      "&key=",
                      key)
  rd <- fromJSON(URLencode(query.url))
  landmark.df.list[[(length(landmark.df.list)+1)]] <- api.result.to.df(rd)
  
  # Loop Through Next Pages
  while(!is.null(rd$next_page_token)){
    
    query.url <- as.character(paste(plcUrl, "pagetoken=",rd$next_page_token,"&key=",key,sep=""))
    rd <- fromJSON(URLencode(query.url))
    landmark.df.list[[(length(landmark.df.list)+1)]] <- api.result.to.df(rd)
    
  }
  
}

# Append -----------------------------------------------------------------------
landmark.df <- landmark.df.list %>% bind_rows()

# Cleanup ----------------------------------------------------------------------
landmark.df <- landmark.df %>%
  dplyr::select(lat, lng, name, types, vicinity) %>%
  dplyr::rename(lon = lng) %>%
  dplyr::rename(type = types) %>%
  unique() %>%
  mutate(source = "google",
         name = name %>% tolower())

# Export -----------------------------------------------------------------------
saveRDS(landmark.df, file.path(google_places_dir, "data", "google_places.Rds"))

