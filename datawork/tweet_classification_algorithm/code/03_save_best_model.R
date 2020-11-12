# Tweet Classification: Classify Tweets as Crash Related or Not
# Save Best Model

# Check which model and parameters performed best. Train and save that model
# for later use.

# Load Results -----------------------------------------------------------------
results_df <- read.csv(file.path(tweets_classif_dir,
                                 "results",
                                 "tweet_classification_results_all_agg.csv"),
                       stringsAsFactors = F)

results_df <- results_df[which.max(results_df$f1_mean),]

# Load Data --------------------------------------------------------------------
truth_data <- readRDS(file.path(tweets_classif_dir, "data", 
                                "tweets_truth_for_classification.Rds"))

dfm <- prep_dfm(truth_data, results_df)

if(results_df$model_type %in% "svm"){
  model <- textmodel_svm(x = dfm, 
                         y = truth_data$accident_truth, 
                         weight = results_df$prior,
                         cost = results_df$svm_cost)
  
} else if(results_df$model_type %in% "nb"){
  model <- textmodel_nb(x = dfm, 
                        y = truth_data$accident_truth, 
                        prior = results_df$prior)
  
} 

# Export -----------------------------------------------------------------------
saveRDS(model, file.path(tweets_classif_dir, "model", "tweet_classification_best_model.Rds"))
saveRDS(results_df, file.path(tweets_classif_dir, "model", "tweet_classification_best_model_parameters.Rds"))

