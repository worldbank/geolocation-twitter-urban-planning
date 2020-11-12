# Geoparse Tweets
# Illustrate algorithm

#### Parameters
height = 2.5 
width = 2.5 

height_2 = 1.9
width_2 = 3

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(osm_dir, "data", "processed_data", "roads", "osm_roads_aug.Rds"))
landmarks <- readRDS(file.path(landmarkgaz_dir, "data", "gazetteers", 
                               "landmark_gazetter_aug.Rds")) 

landmarks_df <- landmarks %>% as.data.frame()

# A. All locations -------------------------------------------------------------
exnt <- landmarks[(landmarks$name %in% "kianda") | 
                    (landmarks$name %in% "kianda school") |
                    (landmarks$name %in% "abc place"),] %>% extent() + 3/111.12

ggplot() +
  geom_path(data = roads[roads$name %in% "waiyaki way",], 
            aes(x=long, y=lat, group=group),
            color = "darkorange2") +
  geom_point(aes(x = 36.758419, y =  -1.263912), fill = "seagreen3", color = "black", size=5, pch = 21) +
  geom_point(data=landmarks_df[grepl("kianda", landmarks_df$name),], 
             aes(x=lon, y=lat), color = "black", fill = "purple", pch = 21, size=3) +
  geom_point(data=landmarks_df[(landmarks_df$name %in% "abc place"),], 
             aes(x=lon, y=lat), color = "black", fill = "deepskyblue", pch = 21, size=3) +
  xlim(exnt@xmin, exnt@xmax) + 
  ylim(exnt@ymin, exnt@ymax) + 
  theme_void() +
  coord_quickmap()  +
  ggsave(file.path(figures_dir, "figure_1a.png"), height = height, width = width)

# B. All landmarks with kinda --------------------------------------------------
ggplot() +
  geom_path(data = roads[roads$name %in% "waiyaki way",], 
            aes(x=long, y=lat, group=group),
            color = "darkorange2") +
  geom_point(aes(x = 36.758419, y =  -1.263912), fill = "seagreen3", color = "black", size=5, pch = 21) +
  geom_point(data=landmarks_df[grepl("kianda", landmarks_df$name),], 
             aes(x=lon, y=lat), color = "black", fill = "purple", pch = 21, size=3) +
  xlim(exnt@xmin, exnt@xmax) + 
  ylim(exnt@ymin, exnt@ymax) + 
  theme_void() +
  coord_quickmap()  +
  ggsave(file.path(figures_dir, "figure_1b.png"), height = height, width = width)

## Landmark Examples
landmarks_df$name[grepl("kianda", landmarks_df$name)] %>% unique %>% sort()
landmarks_df$name[grepl("abc place", landmarks_df$name)] %>% unique %>% sort()

# C. Restrict to road ----------------------------------------------------------
## Landmarks near road and new extent
lnd_nr_rd <- landmarks[grepl("kianda", landmarks$name),]

rd_i <- roads[roads$name %in% "waiyaki way",]
rd_i$id <- 1
rd_i <- raster::aggregate(rd_i, by = "id")

lnd_nr_rd$dist_rd <- gDistance(lnd_nr_rd, rd_i, byid=T) %>% as.numeric()
lnd_nr_rd <- lnd_nr_rd[lnd_nr_rd$dist_rd <= 0.5/111.12,]
lnd_nr_rd_df <- as.data.frame(lnd_nr_rd)

exnt <- extent(lnd_nr_rd) + .5/111.12

## Figure
ggplot() +
  geom_path(data = roads[roads$name %in% "waiyaki way",], 
            aes(x=long, y=lat, group=group),
            color = "darkorange2") +
  geom_point(aes(x = 36.758419, y =  -1.263912), fill = "seagreen3", color = "black", size=5, pch = 21) +
  geom_point(data=lnd_nr_rd_df, 
             aes(x=lon, y=lat), color = "black", fill = "purple", pch = 21, size=2) +
  xlim(exnt@xmin, exnt@xmax) + 
  ylim(exnt@ymin, exnt@ymax) + 
  theme_void() +
  coord_quickmap()  +
  ggsave(file.path(figures_dir, "figure_1c.png"), height = height_2, width = width_2)

## Landmark Examples
lnd_nr_rd_df$name[grepl("kianda", lnd_nr_rd_df$name)] %>% unique %>% sort()

# D. Centroid ------------------------------------------------------------------
# Dominant cluster exists

lnd_nr_rd_c <- gCentroid(lnd_nr_rd) 
lnd_nr_rd_c$id <- 1
lnd_nr_rd_c_df <- as.data.frame(lnd_nr_rd_c)

ggplot() +
  geom_path(data = roads[roads$name %in% "waiyaki way",], 
            aes(x=long, y=lat, group=group),
            color = "darkorange2") +
  geom_point(aes(x = 36.758419, y =  -1.263912), fill = "seagreen3", color = "black", size=5, pch = 21) +
  geom_point(data=lnd_nr_rd_c_df, 
             aes(x=x, y=y), color = "black", fill = "purple", pch = 21, size=2) +
  xlim(exnt@xmin, exnt@xmax) + 
  ylim(exnt@ymin, exnt@ymax) + 
  theme_void() +
  coord_quickmap()  +
  ggsave(file.path(figures_dir, "figure_1d.png"), height = height_2, width = width_2)

# E. All Crash Reports ---------------------------------------------------------
lnd_nr_rd_c_df$name <- "kianda"

crashes_all <- bind_rows(
  lnd_nr_rd_c_df,
  data.frame(y = -1.263710, x = 36.759155, name = "alfrati guest house"),
  data.frame(y = -1.263881, x = 36.758963, name = "sky park"),
  data.frame(y = -1.264960, x = 36.758030, name = "kianda school"),
  data.frame(y = -1.263531, x = 36.758889, name = "kabete lab")
)

ggplot() +
  geom_path(data = roads[roads$name %in% "waiyaki way",], 
            aes(x=long, y=lat, group=group),
            color = "darkorange2") +
  geom_point(aes(x = 36.758419, y =  -1.263912), fill = "seagreen3", color = "black", size=5, pch = 21) +
  geom_point(data=crashes_all, 
             aes(x=x, y=y), color = "black", fill = "purple", pch = 21, size=2) +
  xlim(exnt@xmin, exnt@xmax) + 
  ylim(exnt@ymin - .01/111.12, exnt@ymax + .01/111.12) + 
  theme_void() +
  coord_quickmap()  +
  ggsave(file.path(figures_dir, "figure_1e.png"), height = height_2, width = width_2)

# F. Crash ---------------------------------------------------------------------
crashes_all_c <- crashes_all %>%
  mutate(id = 1) %>%
  group_by(id) %>%
  summarise(x = mean(x),
            y = mean(y))

ggplot() +
  geom_path(data = roads[roads$name %in% "waiyaki way",], 
            aes(x=long, y=lat, group=group),
            color = "darkorange2") +
  geom_point(aes(x = 36.758419, y =  -1.263912), fill = "seagreen3", color = "black", size=5, pch = 21) +
  geom_point(data=crashes_all_c, 
             aes(x=x, y=y), color = "black", fill = "purple", pch = 21, size=2) +
  xlim(exnt@xmin, exnt@xmax) + 
  ylim(exnt@ymin - .01/111.12, exnt@ymax + .01/111.12) + 
  theme_void() +
  coord_quickmap() +
  ggsave(file.path(figures_dir, "figure_1f.png"), height = height_2, width = width_2)










