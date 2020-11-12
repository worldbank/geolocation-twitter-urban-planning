# Trends in Tweets and Tweet Map

TWITTER_COLOR <- "dodgerblue3"

# Load and Prep Data -----------------------------------------------------------
### Load data
tweets             <- readRDS(file.path(tweets_all_dir, "data", "processed_data", "tweets_classified_geoparsed.Rds"))
tweets_clustered   <- readRDS(file.path(tweets_all_dir, "data", "processed_data", "tweets_classified_geoparsed_uniquecrashes.Rds"))

### Subset
tweets <- tweets[tweets$crash_tweet_algorithm %in% TRUE,]
tweets <- tweets[tweets$name %in% "Ma3Route",]

### Add Day Variable
tweets$day_nairobitime <- tweets$created_at_nairobitime %>% as.Date() 
tweets_clustered$day_nairobitime <- tweets_clustered$crash_time_min %>% as.Date() 

### Aggregate to Day
tweets_dailysum <- tweets %>%
  as.data.frame %>%
  group_by(day_nairobitime) %>%
  dplyr::summarise(N = n())

tweets_clustered_dailysum <- tweets_clustered %>%
  group_by(day_nairobitime) %>%
  summarise(N = n())

# Trends Figures ---------------------------------------------------------------
fig_tweets_daily <- ggplot(data=tweets_dailysum, aes(x=day_nairobitime, y=N)) +
  geom_col(col = TWITTER_COLOR) +
  theme_minimal() +
  labs(x="", y="", title="Daily Number of Tweets that Report a Crash") +
  theme(axis.title.y = element_text(angle=0, vjust=.5),
        plot.title = element_text(hjust=.5, face = "bold", size=11)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))

fig_crashes_daily <- ggplot(data=tweets_clustered_dailysum, aes(x=day_nairobitime, y=N)) +
  geom_col(col = TWITTER_COLOR) +
  theme_minimal() +
  labs(x="", y="", title="Daily Number of Geolocated Crashes") +
  scale_y_continuous(limits=c(0,max(tweets_dailysum$N))) + 
  theme(axis.title.y = element_text(angle=0, vjust=.5),
        plot.title = element_text(hjust=.5, face = "bold", size=11)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))

# OSM Basemap ------------------------------------------------------------------
## Load Nairobi GADM Area
nairobi <- readRDS(file.path(gadm_dir, "data", "gadm36_KEN_1_sp.rds"))
nairobi <- nairobi[nairobi$NAME_1 %in% "Nairobi",]

## Load Roads 
roads <- readRDS(file.path(osm_dir, "data", "processed_data", "geofabrik_download_20190317_nairobi", "gis_osm_roads_free_1_nairobi.Rds"))

## Restrict by Type
types_keep <- c("trunk", "motorway", "primary", "secondary", "tertiary", "residential", "unclassified") %>% paste(collapse = "|")
roads <- roads[grepl(types_keep, roads$fclass),]

## Restrict to Nairobi
roads <- roads %>% crop(nairobi) 

# Map --------------------------------------------------------------------------
## Extent
ext <- extent(nairobi)
ext@xmin <- 36.69464 # 36.694642 - 0.5
ext@xmax <- 36.986748 #+ 0.5
ext@ymin <- -1.347534
ext@ymax <- -1.200493

## Restrict crashes to extent
tweets_clustered_geo <- tweets_clustered
coordinates(tweets_clustered_geo) <- ~longitude+latitude
crs(tweets_clustered_geo) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

tweets_clustered_geo <- crop(tweets_clustered_geo, ext)
tweets_clustered_geo <- crop(tweets_clustered_geo, nairobi)

tweets_clustered <- as.data.frame(tweets_clustered_geo)

## Restrict road to extent
roads <- crop(roads, ext)

road_color <- "black"
fig_map <- ggplot() +
  geom_path(data = roads[grepl("trunk|motorway", roads$fclass),],
            aes(x=long, y=lat, group=group),
            size = .5, color = road_color) +
  geom_path(data = roads[grepl("primary", roads$fclass),],
            aes(x=long, y=lat, group=group),
            size = .5, color = road_color) +
  geom_path(data = roads[grepl("secondary|tertiary", roads$fclass),],
            aes(x=long, y=lat, group=group),
            size = .2, color = road_color) +
  geom_path(data = roads[grepl("residential", roads$fclass),],
            aes(x=long, y=lat, group=group),
            size = .05) +
  geom_path(data = roads[grepl("unclassified", roads$fclass),],
            aes(x=long, y=lat, group=group),
            size = .05) +
  geom_point(data=tweets_clustered, aes(x=longitude, y=latitude),
             pch = 21,
             size=.6, fill = "cyan", color = TWITTER_COLOR) + 
  labs(title = "Crashes, August 2012 - July 2020") +
  theme_void() +
  theme(plot.title = element_text(hjust=.5, face = "bold", size=11)) +
  coord_quickmap()

# Arrange Figure and Export ----------------------------------------------------
fig_trends <- ggarrange(fig_tweets_daily, fig_crashes_daily, ncol = 2, labels = c("A", "B"))
fig_all <- ggarrange(fig_trends, fig_map, nrow=2, labels = c("", "C"),
                     heights = c(1,1.45))

ggsave(fig_all, filename = file.path(figures_dir, "figure_2.png"), width=8.5, height=7)
ggsave(fig_all, filename = file.path(figures_dir, "figure_2.tiff"), width=8.5, height=7)



