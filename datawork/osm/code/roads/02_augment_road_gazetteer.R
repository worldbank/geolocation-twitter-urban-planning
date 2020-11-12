# Augment Road Network

# Load Raw Road Network --------------------------------------------------------
roads <- readRDS(file.path(osm_dir, "data", "processed_data", "roads", "osm_roads.Rds"))

# Clean Names ------------------------------------------------------------------
roads$name <- roads$name %>% 
  as.character %>% 
  tolower %>%
  str_replace_all("'", "") %>%
  str_squish()

roads$ambiguous_road <- FALSE

# Parallel Road Type Abbreviations ---------------------------------------------
#### Full to abbreviation
roads_includes_road <- roads[grepl("\\broad\\b", roads$name),]
roads_includes_road$name <- gsub("\\broad\\b", "rd", roads_includes_road$name)

roads_includes_avenue <- roads[grepl("\\bavenue\\b", roads$name),]
roads_includes_avenue$name <- gsub("\\bavenue\\b", "ave", roads_includes_avenue$name)

roads_includes_street <- roads[grepl("\\bstreet\\b", roads$name),]
roads_includes_street$name <- gsub("\\bstreet\\b", "st", roads_includes_street$name)

#### Abbreviation to full
roads_includes_rd <- roads[grepl("\\brd\\b", roads$name),]
roads_includes_rd$name <- gsub("\\brd\\b", "road", roads_includes_rd$name)

roads_includes_ave <- roads[grepl("\\bave\\b", roads$name),]
roads_includes_ave$name <- gsub("\\bave\\b", "avenue", roads_includes_ave$name)

roads_includes_st <- roads[grepl("\\bst\\b", roads$name),]
roads_includes_st$name <- gsub("\\bst\\b", "street", roads_includes_st$name)

# Parallel Common Alternate Names ----------------------------------------------
#### Thika Road
roads_thika <- roads[roads$name %in% "thika road",]

roads_thika_parallel_1 <- roads_thika
roads_thika_parallel_1$name <- "thika superhighway"

roads_thika_parallel_2 <- roads_thika
roads_thika_parallel_2$name <- "thika super highway"

roads_thika_parallel_3 <- roads_thika
roads_thika_parallel_3$name <- "thika highway"

roads_thika_parallel_4 <- roads_thika
roads_thika_parallel_4$name <- "superhighway"

#### Mombasa Road
roads_mombasa <- roads[roads$name %in% "mombasa road",]

roads_mombasa_parallel_1 <- roads_mombasa
roads_mombasa_parallel_1$name <- "msa road"

roads_mombasa_parallel_2 <- roads_mombasa
roads_mombasa_parallel_2$name <- "msa rd"

roads_mombasa_parallel_3 <- roads_mombasa
roads_mombasa_parallel_3$name <- "mbs rd"

#### Waiyaki Way
roads_a104 <- roads[roads$name %in% "a104",]

roads_a104_parallel_1 <- roads_a104
roads_a104_parallel_1$name <- "waiyaki way"

# Ambiguous Roads --------------------------------------------------------------
roads_bypass_ambiguous <- roads[grepl("bypass", roads$name),]
roads_bypass_ambiguous$name <- "bypass"
roads_bypass_ambiguous$ambiguous_road <- TRUE

# Append -----------------------------------------------------------------------
roads_aug <- list(roads,
                  roads_includes_road, 
                  roads_includes_avenue, 
                  roads_includes_street,
                  roads_includes_rd,
                  roads_includes_ave,
                  roads_includes_st,
                  roads_thika_parallel_1,
                  roads_thika_parallel_2,
                  roads_thika_parallel_3,
                  roads_thika_parallel_4,
                  roads_mombasa_parallel_1,
                  roads_mombasa_parallel_2,
                  roads_mombasa_parallel_3,
                  roads_a104_parallel_1,
                  roads_bypass_ambiguous) %>% 
  do.call(what="rbind")

# Remove Generic Names ---------------------------------------------------------
generic_names <- c("bridge", "roundabout", "bypass","by pass", "round about", "entrance")
roads_aug <- roads_aug[!((roads_aug$name %in% generic_names) & (roads_aug$ambiguous_road %in% FALSE)),]

# Names must be certain length -------------------------------------------------
roads_aug <- roads_aug[nchar(roads_aug$name) >= 3,]

# Always Remove ----------------------------------------------------------------
roads_aug <- roads_aug[!(roads_aug$name %in% c("road")),]

# Buffer Slightly --------------------------------------------------------------
# Buffer by 10 meters. See lusaka rd and enterprise rd. There's a roundabout at
# the intersection where they actually don't touch.
roads_aug <- gBuffer(roads_aug, width=.01/111.12, byid=T)

# Export -----------------------------------------------------------------------
saveRDS(roads_aug, file.path(osm_dir, "data", "processed_data", "roads", "osm_roads_aug.Rds"))




