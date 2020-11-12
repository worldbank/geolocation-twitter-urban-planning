# Tweet Classification: Classify Tweets as Crash Related or Not
# Results Table

# Create table of results

ROUND <- 3 # Amount to round numbers

# Load Data --------------------------------------------------------------------
results_df <- read.csv(file.path(tweets_classif_dir,
                                 "results",
                                 "tweet_classification_results_all_agg.csv"),
                       stringsAsFactors = F)

# Table ------------------------------------------------------------------------
## Subset to select parameters
results_sub <- results_df %>%
  filter(prior %in% "uniform") %>%
  filter(trim %in% 0) %>%
  filter(is.na(tfid) | 
           (tfid %in% T) & (model_type %in% "svm") |
           (tfid %in% F) & (model_type %in% "nb")) %>%
  filter(tweet_var %in% "tweet") %>%
  filter(is.na(svm_cost) | svm_cost %in% 0.5) %>%
  arrange(model_type, ngram_max)

results_sub$latex <- paste(results_sub$precision_mean %>% round(ROUND), "&",
                           results_sub$recall_mean %>% round(ROUND), "&",
                           results_sub$f1_mean %>% round(ROUND), "&",
                           results_sub$accuracy_mean %>% round(ROUND), "&",
                           results_sub$ngram_max, " \\\\ ")


sink(file.path(tables_dir, "table_s4.tex"))
cat(" \\begin{tabular}{cccc | c} ")
cat(" \\hline")
cat(" Precision & Recall & F1 & Accuracy & N-Grams \\\\ ")

cat(" \\hline")
cat(" \\multicolumn{4}{c|}{Naive Bayes} &  \\\\ ")
results_sub$latex[results_sub$model_type %in% "nb"] %>% cat()

cat(" \\hline")
cat(" \\multicolumn{4}{c|}{SVM} &  \\\\ ")
results_sub$latex[results_sub$model_type %in% "svm"] %>% cat()

cat(" \\hline")
cat(" \\end{tabular}")
sink()
