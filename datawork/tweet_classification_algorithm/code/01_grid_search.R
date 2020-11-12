# Tweet Classification: Classify Tweets as Crash Related or Not
# Grid Search

# Perform grid search testing algorithms and parameters for predicting
# whether a tweet is crash related or not

#### Parameters 
K_FOLDS <- 4
set.seed(42)

RUN_NB  <- T
RUN_SVM <- T

# Load Data --------------------------------------------------------------------
truth_data <- readRDS(file.path(tweets_classif_dir, 
                                "data",
                                "tweets_truth_for_classification.Rds"))

# Train and Test Sets ----------------------------------------------------------
# Randomly sort truth data and divide into K_FOLDS folds

truth_data <- truth_data[sample(1:nrow(truth_data)),]
k_fold <- rep(1:K_FOLDS, length.out = nrow(truth_data)) 

# Parameters -------------------------------------------------------------------
# Create dataframe of parameters. First create master parameter dataframe, 
# then filter out for relevant parameters for SVM and NB

#### Parameter List
ngram_max <- 1:3
prior <- c("uniform", "docfreq", "termfreq")
trim <- c(0, 0.01, 0.05) 
tweet_var <- c("tweet") 
tfid <- c(T, F)
svm_cost <- c(0.5, 1, 2, 10, 100, 1000)

#### Parameters
all_params <- list(ngram_max = ngram_max,
                   prior = prior,
                   trim = trim,
                   tweet_var = tweet_var,
                   tfid = tfid,
                   svm_cost = svm_cost) %>%
  as.data.frame() %>%
  mutate_all(as.factor) %>%
  complete(ngram_max, prior, trim, tweet_var, tfid, svm_cost) %>%
  mutate_all(as.character) %>%
  mutate_at(c("trim", "svm_cost", "ngram_max"), as.numeric)

## SVM Parameters
svm_params <- all_params[all_params$prior %in% "uniform",]

## NB Parameters
nb_params <- all_params %>%
  filter(svm_cost %in% 1) %>%
  dplyr::select(-svm_cost)

#### Add model number
svm_params$model <- 1:nrow(svm_params)
nb_params$model  <- 1:nrow(nb_params)

# Run Models -------------------------------------------------------------------
if(RUN_NB){
  nb_results <- grid_search_with_dfm(nb_params, "nb")
  
  write.csv(nb_results, file.path(tweets_classif_dir,
                                  "results",
                                  "tweet_classification_results_nb.csv"),
            row.names = F)
}

if(RUN_SVM){
  svm_results <- grid_search_with_dfm(svm_params, "svm")
  
  write.csv(svm_results, file.path(tweets_classif_dir,
                                   "results",
                                   "tweet_classification_results_svm.csv"),
            row.names = F)
}




