# Heatmap of Crashes

# Load Data --------------------------------------------------------------------
## Crashes - All
crashes_all_df   <- readRDS(file.path(tweets_all_dir, "data", "processed_data", "tweets_classified_geoparsed_uniquecrashes.Rds"))
coordinates(crashes_all_df) <- ~longitude+latitude
crs(crashes_all_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crashes_all_df$N <- 1

## Crashes - Truth
crashes_truth_df   <- readRDS(file.path(tweets_truth_dir, "data", "processed_data", "crash_cluster_analysis", "tweets_truth_uniquecrash.Rds"))

# polygon to point
coords <- crashes_truth_df %>% 
  coordinates() %>% 
  as.data.frame() %>% 
  dplyr::rename(longitude = V1,
                latitude = V2) 
crashes_truth_df <- bind_cols(crashes_truth_df@data, coords)

coordinates(crashes_truth_df) <- ~longitude+latitude
crs(crashes_truth_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crashes_truth_df$N <- 1

## Nairobi
nairobi <- readRDS(file.path(gadm_dir, "data", "gadm36_KEN_1_sp.rds"))
nairobi <- nairobi[nairobi$NAME_1 %in% "Nairobi",]

## OSM
osm <- readRDS(file.path(osm_dir, "data", "processed_data", "geofabrik_download_20190317_nairobi", "gis_osm_roads_free_1_nairobi.Rds"))

# Restrict to Nairobi ----------------------------------------------------------
crashes_all_df   <- crashes_all_df[over(crashes_all_df,     nairobi)$NAME_1 %in% "Nairobi",]
crashes_truth_df <- crashes_truth_df[over(crashes_truth_df, nairobi)$NAME_1 %in% "Nairobi",]

# Basemap ----------------------------------------------------------------------
types_keep <- c("trunk", "motorway", "primary", "secondary", "tertiary") %>% paste(collapse = "|")
osm_main <- osm[grepl(types_keep, osm$fclass),]

osm_main <- osm_main %>% crop(nairobi) 

# KDE --------------------------------------------------------------------------
#### Blank Raster
res <- 0.03
r_all <- raster(ext = extent(nairobi),
                crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                resolution = res/111.12)

r_truth <- raster(ext = extent(nairobi),
                  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  resolution = res/111.12)

#### Rasterize
r_all_values <- rasterize(crashes_all_df, r_all, "N", fun = "sum")
r_all_values[][is.na(r_all_values[])] <- 0

r_truth_values <- rasterize(crashes_truth_df, r_truth, "N", fun = "sum")
r_truth_values[][is.na(r_truth_values[])] <- 0

#### KDE
sigma = 5
n = 281
gm <- gaussian.kernel(sigma=sigma, n=n)

r_all_kde <- raster::focal(r_all_values, w = gm, fun = "sum")
r_all_kde[r_all_kde[] < 0.01] <- NA

r_truth_kde <- raster::focal(r_truth_values, w = gm, fun = "sum")
r_truth_kde[r_truth_kde[] < 0.01] <- NA

## Make DF
r_all_kde_df <- as(r_all_kde, "SpatialPixelsDataFrame") %>% as.data.frame()
colnames(r_all_kde_df) <- c("value", "x", "y") 

r_truth_kde_df <- as(r_truth_kde, "SpatialPixelsDataFrame") %>% as.data.frame()
colnames(r_truth_kde_df) <- c("value", "x", "y") 

# Transform Values -------------------------------------------------------------

#### Log
r_all_kde_df$value_ln <- log(r_all_kde_df$value+1)
r_truth_kde_df$value_ln <- log(r_truth_kde_df$value+1)

large_v <- quantile(r_all_kde_df$value_ln, 0.999)
r_all_kde_df$value_ln[r_all_kde_df$value_ln > large_v]       <- large_v
r_truth_kde_df$value_ln[r_truth_kde_df$value_ln > large_v] <- large_v

## Sigmoid
sig <- function(x) 1 / (0.1 + 1*exp(-x)) - 0.14

r_all_kde_df$value_s <- sig(r_all_kde_df$value_ln) 
r_truth_kde_df$value_s <- sig(r_truth_kde_df$value_ln) 

# Map --------------------------------------------------------------------------
road_color <- "gray60"
p_all <- ggplot() +
  geom_path(data = osm_main[grepl("trunk|motorway", osm_main$fclass),],
            aes(x=long, y=lat, group=group),
            size = .5, color = road_color) +
  geom_path(data = osm_main[grepl("primary", osm_main$fclass),],
            aes(x=long, y=lat, group=group),
            size = .5, color = road_color) +
  geom_path(data = osm_main[grepl("secondary|tertiary", osm_main$fclass),],
            aes(x=long, y=lat, group=group),
            size = .2, color = road_color) +
  #geom_path(data = osm_main[grepl("residential", osm_main$fclass),],
  #          aes(x=long, y=lat, group=group),
  #          size = .05) +
  geom_raster(data = r_all_kde_df, 
              aes(x=x, y=y,
                  fill=value_ln),
              alpha=r_all_kde_df$value_s) +
  scale_fill_gradientn(colors = brewer.pal(7, "Spectral") %>% rev(),
                       limits = c(min(r_all_kde_df$value_ln),
                                  max(r_all_kde_df$value_ln)),
                       breaks = c(min(r_all_kde_df$value_ln),
                                  max(r_all_kde_df$value_ln)),
                       labels = c("Less Crashes      ", "      More Crashes")) +
  labs(title = "Full Data",
       subtitle = "August 2012 - July 2020") +
  theme_void() +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        plot.title = element_text(color = "white",
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(color = "white",
                                     hjust = 0.5,
                                     face = "bold"), 
        legend.position="bottom",
        legend.text = element_text(color = "white",size=12)) +
  bgcolor("black") +
  coord_quickmap(xlim=c(36.694672, 36.986601 - 3/111.12),
                 ylim=c(-1.360817,-1.153733 - 7/111.12)) 

p_truth <- ggplot() +
  geom_path(data = osm_main[grepl("trunk|motorway", osm_main$fclass),],
            aes(x=long, y=lat, group=group),
            size = .5, color = road_color) +
  geom_path(data = osm_main[grepl("primary", osm_main$fclass),],
            aes(x=long, y=lat, group=group),
            size = .5, color = road_color) +
  geom_path(data = osm_main[grepl("secondary|tertiary", osm_main$fclass),],
            aes(x=long, y=lat, group=group),
            size = .2, color = road_color) +
  #geom_path(data = osm_main[grepl("residential", osm_main$fclass),],
  #          aes(x=long, y=lat, group=group),
  #          size = .05) +
  geom_raster(data = r_truth_kde_df, 
              aes(x=x, y=y,
                  fill=value_ln),
              alpha=r_truth_kde_df$value_s) +
  scale_fill_gradientn(colors = brewer.pal(7, "Spectral") %>% rev(),
                       limits = c(min(r_truth_kde_df$value_ln),
                                  max(r_truth_kde_df$value_ln)),
                       breaks = c(min(r_truth_kde_df$value_ln),
                                  max(r_truth_kde_df$value_ln)),
                       labels = c("Less Crashes      ", "      More Crashes")) +
  labs(title = "Truth Data",
       subtitle = "July 2017 - July 2018") +
  theme_void() +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        plot.title = element_text(color = "white",
                                  hjust = 0.5,
                                  face = "bold"), 
        plot.subtitle = element_text(color = "white",
                                     hjust = 0.5,
                                     face = "bold"), 
        legend.position="bottom",
        legend.text = element_text(color = "white",size=12)) +
  bgcolor("black") +
  coord_quickmap(xlim=c(36.694672, 36.986601 - 3/111.12),
                 ylim=c(-1.360817,-1.153733 - 7/111.12 )) 

# side by side
p_final <- ggarrange(p_truth,
                     p_all,
                     ncol = 2,
                     common.legend = T,
                     legend = "bottom") +
  bgcolor("black")
ggsave(p_final, 
       filename = file.path(figures_dir,
                            "figure_3.png"),
       height = 3.5, width = 10.5,
       dpi = 800)

ggsave(p_final, 
       filename = file.path(figures_dir,
                            "figure_3.tiff"),
       height = 3.5, width = 10.5)


