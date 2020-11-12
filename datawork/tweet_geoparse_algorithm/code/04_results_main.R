# Geoparse Tweets
# Table for main text

ROUND <- 3 # number of decimal places to round to

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(tweets_geoparse_dir, "results", "tweet_geoparse_results.Rds"))

# Table ------------------------------------------------------------------------
sink(file.path(tables_dir, "table_1.tex"))
cat("\\begin{tabular}{l cc | cc} ")
cat("\\hline ")
cat(" & \\multicolumn{2}{c|}{Any Location} & \\multicolumn{2}{c}{Crash Location } \\\\ ")
cat(" & \\multicolumn{2}{c|}{Captured by} & \\multicolumn{2}{c}{Determined by} \\\\ ")
cat(" & \\multicolumn{2}{c|}{Algorithm Close to} & \\multicolumn{2}{c}{Algorithm Close to} \\\\ ") 
cat(" & \\multicolumn{2}{c|}{True Crash Location} & \\multicolumn{2}{c}{True Crash Location} \\\\  ")
cat("\\hline ")
cat(" & Recall & Precision & Recall & Precision \\\\ ")
cat("\\hline ")

cat("LNEx & ")
cat(round(results_df$anyloc_recall[results_df$type %in% "lnex"], ROUND), " & ")
cat(round(results_df$anyloc_precision[results_df$type %in% "lnex"], ROUND), " & ")

cat(round(results_df$alg_recall[results_df$type %in% "lnex"], ROUND), " & ")
cat(round(results_df$alg_precision[results_df$type %in% "lnex"], ROUND), " \\\\ ")

cat("Alg., Raw Gaz & ")
cat(round(results_df$anyloc_recall[results_df$type %in% "raw"], ROUND), " & ")
cat(round(results_df$anyloc_precision[results_df$type %in% "raw"], ROUND), " & ")

cat(round(results_df$alg_recall[results_df$type %in% "raw"], ROUND), " & ")
cat(round(results_df$alg_precision[results_df$type %in% "raw"], ROUND), " \\\\ ")

cat("Alg., Aug Gaz & ")
cat(round(results_df$anyloc_recall[results_df$type %in% "aug"], ROUND), " & ")
cat(round(results_df$anyloc_precision[results_df$type %in% "aug"], ROUND), " & ")

cat(round(results_df$alg_recall[results_df$type %in% "aug"], ROUND), " & ")
cat(round(results_df$alg_precision[results_df$type %in% "aug"], ROUND), " \\\\ ")

cat("Alg., Aug Gaz [Cluster] & ")
cat(" & & ")
cat(round(results_df$clstr_recall[results_df$type %in% "aug"], ROUND), " & ")
cat(round(results_df$clstr_precision[results_df$type %in% "aug"], ROUND), " \\\\ ")

cat("\\hline ")

cat("\\multicolumn{5}{p{11cm}}{`N Crashes' refers to the number of correctly 
    identified crashes. `Raw Gaz' refers to the raw gazetteer (ie, dictionary of 
    landmarks with original names) and `Aug Gaz' refers to the augmented gazetteer. 
    We use our raw gazetteer as an input into LNEX, which implements its own augmentation
    process. For LNEx, the crash location is determined by taking the centroid of all 
    locations captured by the algorithm. Locations are considered close if they are 
    within 500 meters of each other.} ")

cat("\\end{tabular} ")
sink()



