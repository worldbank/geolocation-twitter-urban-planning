# Geoparse Tweets
# Table for supplementary information

ROUND <- 3 # number of decimal places to round to

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(tweets_geoparse_dir, "results", "tweet_geoparse_results.Rds"))

# Prep Variables ---------------------------------------------------------------
results_df$order <- NA
results_df$order[results_df$type %in% "lnex"]         <- 1
results_df$order[results_df$type %in% "aug_geonames"] <- 2
results_df$order[results_df$type %in% "aug_google"]   <- 3
results_df$order[results_df$type %in% "aug_osm"]      <- 4
results_df$order[results_df$type %in% "raw"]          <- 5
results_df$order[results_df$type %in% "aug"]          <- 6

results_df <- results_df %>%
  arrange(order)

results_df <- results_df %>%
  mutate(type = case_when(type == "aug" ~ "Aug Gaz",
                          type == "aug_geonames" ~ "Aug Gaz - Geonames",
                          type == "aug_google" ~ "Aug Gaz - Google",
                          type == "aug_osm" ~ "Aug Gaz - OSM",
                          type == "raw" ~ "Raw Gaz",
                          type == "lnex" ~ "LNEx Aug Gaz"))

# LaTeX Variable ---------------------------------------------------------------
results_df$latex <- paste(results_df$type, "&",
                          
                          results_df$anyloc_recall %>% round(ROUND), " & ",
                          results_df$anyloc_precision %>% round(ROUND), " & ",
                          
                          results_df$alg_recall %>% round(ROUND), " & ",
                          results_df$alg_precision %>% round(ROUND), " & ",
                          
                          results_df$clstr_recall %>% round(ROUND), " & ",
                          results_df$clstr_precision %>% round(ROUND), " \\\\ ")

# Table ------------------------------------------------------------------------
sink(file.path(tables_dir, "table_s5.tex"))
cat("\\begin{tabular}{l cc | cc | cc } ")
cat("\\hline ")
cat(" & \\multicolumn{2}{c|}{Any Location Captured by} & \\multicolumn{2}{c|}{Crash Location Determined} & \\multicolumn{2}{c}{Algorithm Cluster} \\\\ ")
cat(" & \\multicolumn{2}{c|}{Algorithm Close to} & \\multicolumn{2}{c|}{by Algorithm Close to}  & \\multicolumn{2}{c}{Contains} \\\\ ") 
cat(" & \\multicolumn{2}{c|}{True Crash Location} & \\multicolumn{2}{c|}{True Crash Location}  & \\multicolumn{2}{c}{True Crash Loction} \\\\  ")
cat("\\hline ")
cat(" & Recall & Precision & Recall & Precision & Recall & Precision \\\\ ")
cat("\\hline ")

for(i in 1:nrow(results_df)){
  
  if(i == 1) cat("\\multicolumn{3}{l|}{\\bf LNEx} & & & & \\\\ ")
  if(i == 2) cat("\\multicolumn{3}{l|}{\\bf Algorithm - by Source} & & & & \\\\ ")
  if(i == 5) cat("\\multicolumn{3}{l|}{\\bf Algorithm - All Sources} & & & & \\\\ ")
  
  cat(" ~~~", results_df$latex[i])
}

cat("\\hline ")
cat("\\end{tabular} ")
sink()

