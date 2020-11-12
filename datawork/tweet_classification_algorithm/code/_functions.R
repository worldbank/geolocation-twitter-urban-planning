# Tweet Classification: Classify Tweets as Crash Related or Not
# Functions

# Calculate Performance --------------------------------------------------------
calc_performance <- function(pred, truth){
  # Given vectors of predictions and truth data, calculate recall, precision,
  # accuracy and f1 stat
  
  recall <- sum((truth %in% T) & (pred %in% T)) / sum(truth %in% T)
  precision <- sum((truth %in% T) & (pred %in% T)) / sum(pred %in% T)
  accuracy <- mean(truth == pred)
  f1 <- (2 * precision * recall) / (precision + recall)
  
  df_out <- data.frame(recall = recall,
                       precision = precision,
                       accuracy = accuracy,
                       f1 = f1)
  
  return(df_out)
}

# Prep DFM ---------------------------------------------------------------------
prep_dfm <- function(df, params_df){
  # Prep dfm based on parameters
  
  dfm <- tokens(x=df[[params_df$tweet_var]], what="word") %>%
    tokens_ngrams(n=1:params_df$ngram_max, conc=" ") %>%
    dfm(tolower=T) %>%
    dfm_trim(min_docfreq=0.0002, docfreq_type = "prop")  %>% # always take out super few
    dfm_trim(min_docfreq=params_df$trim, docfreq_type = "prop")  %>%
    dfm_trim(max_docfreq=(1-params_df$trim), docfreq_type = "prop") 
  
  if(params_df$tfid %in% "TRUE"){
    dfm <- dfm_tfidf(dfm)
  }
  
  return(dfm)
}

# Grid Search: DFM Based Input -------------------------------------------------
grid_search_with_dfm <- function(params, model_type){
  # Given a dataframe of parameters and model type, performs a grid search
  # on the parameters. Outputs dataframe with results (recall, precision,
  # accuracy and f1), along with associated parameters.
  
  # Loop through parameters - - - - - - - - - - - - - - - - - - - - - - - - - - 
  results_all_df <- lapply(params$model, function(model_i){
    
    print(model_i)
    
    #### Prep DFM
    params_i <- params[model_i,]
    dfm <- prep_dfm(truth_data, params_i)
    
    # Loop through folds - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    results_df <- lapply(1:K_FOLDS, function(fold_i){
      
      if(model_type %in% "svm"){
        model <- textmodel_svm(x=dfm[k_fold != fold_i,], 
                               y=truth_data$accident_truth[k_fold != fold_i], 
                               weight=params_i$prior,
                               cost = params_i$svm_cost)
        
      } else if(model_type %in% "nb"){
        model <- textmodel_nb(x=dfm[k_fold != fold_i,], 
                              y=truth_data$accident_truth[k_fold != fold_i], 
                              prior=params_i$prior)
        
      } 
      
      predictions <- predict(model, newdata = dfm[k_fold == fold_i,], type="class") %>% as.vector()
      truth <- truth_data$accident_truth[k_fold == fold_i]
      
      predictions <- (predictions %in% "TRUE")
      
      df_out <- calc_performance(predictions,
                                 truth)
      df_out$fold <- fold_i
      
      return(df_out)
    }) %>%
      bind_rows()
    
    # Add parameters to results dataframe - - - - - - - - - - - - - - - - - - - -
    for(var in names(params_i)) results_df[[var]] <- params_i[[var]]
    results_df$model_type <- model_type
    
    return(results_df)
  }) %>%
    bind_rows()
  
  return(results_all_df)
}