# Tweet Classification: Classify Tweets as Crash Related or Not
# Clean Results

# Clean grid search results. Append NB and SVM results and aggregate kfolds.

# Load Data --------------------------------------------------------------------
results_nb_df <- read.csv(file.path(tweets_classif_dir, "results", "tweet_classification_results_nb.csv"))
results_svm_df <- read.csv(file.path(tweets_classif_dir, "results", "tweet_classification_results_svm.csv"))

results_df <- bind_rows(results_nb_df, results_svm_df)

# Aggregate --------------------------------------------------------------------
# Aggregate k-folds
results_agg <- results_df %>%
  group_by(model, model_type) %>%
  dplyr::summarise(recall_mean = mean(recall),
                   precision_mean = mean(precision),
                   accuracy_mean = mean(accuracy),
                   f1_mean = mean(f1),
                   
                   recall_min = min(recall),
                   precision_min = min(precision),
                   accuracy_min = min(accuracy),
                   f1_min = min(f1),
                   
                   recall_max = max(recall),
                   precision_max = max(precision),
                   accuracy_max = max(accuracy),
                   f1_max = max(f1))

params <- results_df %>%
  distinct(model, model_type, .keep_all = T) %>%
  dplyr::select(-c(recall, precision, accuracy, f1))

results_agg <- merge(results_agg, params, by = c("model", "model_type"))

# Export -----------------------------------------------------------------------
write.csv(results_agg, file.path(tweets_classif_dir,
                                 "results",
                                 "tweet_classification_results_all_agg.csv"), 
          row.names = F)


