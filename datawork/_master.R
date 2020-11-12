# Applying machine learning and geolocation techniques to social
# media data (Twitter) to develop a resource for urban planning

# Master R Script

# Organization
# * Filepaths: To make code work, change paths in "Root file path"
# * Parameters: Parameters used throughout scripts
# * Libraries: Load R packages and user defined functions
# * Code: Implements full code for project
# * Code - Figures/tables/stats: Implemnets code just for tables/figures/stats

# FILEPATHS ====================================================================

#### Root file path
github_dir  <- "~/Documents/Github/geolocation-twitter-urban-planning"
dropbox_dir <- "~/Dropbox/World Bank/IEs/Twitter for Urban Planning"

#### Data Paths [Dropbox]
data_dir                <- file.path(dropbox_dir, "data")
tweets_all_dir          <- file.path(data_dir, "tweets_all")
tweets_truth_dir        <- file.path(data_dir, "tweets_truth")
tweets_classif_dir      <- file.path(data_dir, "tweet_classification_algorithm")
tweets_geoparse_dir     <- file.path(data_dir, "tweet_geoparse_algorithm")
landmarkgaz_dir         <- file.path(data_dir, "landmark_gazetteer")
roadgaz_dir             <- file.path(data_dir, "road_gazetteer")
estates_dir             <- file.path(data_dir, "nairobi_estates")
gadm_dir                <- file.path(data_dir, "gadm")
sendy_dir               <- file.path(data_dir, "sendy")
google_places_dir       <- file.path(data_dir, "google_places")
osm_dir                 <- file.path(data_dir, "osm")
geonames_dir            <- file.path(data_dir, "geonames")

#### Datawork Paths [Github]
datawork_dir             <- file.path(github_dir, "datawork")
code_tweets_all_dir      <- file.path(datawork_dir, "tweets_all")
code_tweets_truth_dir    <- file.path(datawork_dir, "tweets_truth")
code_tweets_classif_dir  <- file.path(datawork_dir, "tweet_classification_algorithm")
code_tweets_geoparse_dir <- file.path(datawork_dir, "tweet_geoparse_algorithm")
code_landmarkgaz_dir     <- file.path(datawork_dir, "landmark_gazetteer")
code_roadgaz_dir         <- file.path(datawork_dir, "road_gazetteer")
code_estates_dir         <- file.path(datawork_dir, "nairobi_estates")
code_gadm_dir            <- file.path(datawork_dir, "gadm")
code_sendy_dir           <- file.path(datawork_dir, "sendy")
code_google_places_dir   <- file.path(datawork_dir, "google_places")
code_osm_dir             <- file.path(datawork_dir, "osm")
code_geonames_dir        <- file.path(datawork_dir, "geonames")

## Outputs
outputs_dir <- file.path(github_dir, "outputs")
tables_dir  <- file.path(outputs_dir, "tables")
figures_dir <- file.path(outputs_dir, "figures")


# PARAMETERS ===================================================================
NAIROBI_UTM_PROJ <- "+init=epsg:21037"
IGNORE_TIMEINTENSIVE_SCRIPTS <- T


# LIBRARIES ====================================================================
## Install/Load Package Dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, purrr, ggplot2, ggpubr, raster, rgeos, rgdal, sp,
               spatialEco,  RColorBrewer, readxl, quanteda, quanteda.textmodels,
               osmdata, ClusterR, aricode, clusteval, scales, spacyr)

## Unique Location Extractor
source("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/R/load_ulex.R")

## Clustering Functions
source(file.path(github_dir, "functions_and_packages", "clustering", "cluster_crashes_into_unique_crashes.R"))
source(file.path(github_dir, "functions_and_packages", "clustering", "cluster_crashes_into_clusters.R"))
source(file.path(github_dir, "functions_and_packages", "potentially_crash_related", "potentially_crash_related.R"))


# CODE =========================================================================

## Outline
# 1. Download GADM - used in a number of scripts
# 2. Make landmark and road gazetteers
# 3. Analysis of words that come before landmarks, using truth tweet data
# 4. Tweet classification algorithm
# 5. Tweet geoparse algorithm
# 6. Tweet Cluster Algorithm
# 7. Examine truth data of clustering tweets into crashes
# 8. Apply algorithm on full tweets
# 9. Analysis of landmark types
# 10. Sendy analysis

# 1. Download GADM =============================================================
# Download GADM boundary for Kenya. GADM boundaries used in multiple scripts
source(file.path(code_gadm_dir, "code", "download_gadm.R"))


# 2. Gazetteers ================================================================

# ** 2.1 Prep data from individual sources -------------------------------------
# The gazetteers are made from multiple sources. These scripts clean data
# from individual data sources

# **** 2.1.1 Google Places -----------------------------------------------------
# Scrape landmarks from Google Places. This code requires and API key.
#source(file.path(code_google_places_dir, "code", "scrape_data_googlepaces.R"))


# **** 2.1.2 OSM -----------------------------------------------------
# Downloads and preps landmark data from Open Street Maps

# Crop shapefiles to Narobi and save as .Rds file. May take 1 hour to run
if(IGNORE_TIMEINTENSIVE_SCRIPTS %in% F){
  source(file.path(code_osm_dir, "code", "crop_geofabrik_to_nairobi.R"))
}

# Download builings from overpass API. May take 2+ minutes to run
source(file.path(code_osm_dir, "code", "download_from_overpass_api.R"))

## Create and clean landmark file from OSM
source(file.path(code_osm_dir, "code", "landmarks", "append_landmarks.R"))


# **** 2.1.3 Geonames ----------------------------------------------------------
# Cleans raw geonames data
source(file.path(code_geonames_dir, "code", "clean_geonames.R"))


# ** 2.2 Create landmark gazetteers --------------------------------------------
# Appends data from individual sources to create raw and augmented landmark
# gazetteers

# Append files from different sources into a raw gazetteer
source(file.path(code_landmarkgaz_dir, "code", "01_raw_landmark_gazetteer.R"))

# Augment Gazetteer. May take 30+ minutes to run
if(IGNORE_TIMEINTENSIVE_SCRIPTS %in% F){
  source(file.path(code_landmarkgaz_dir, "code", "02_augment_landmark_gazetteer.R"))
}

# ** 2.3 Create road gazetteers ------------------------------------------------
# Using data from Open Street Maps, creates a clean road gazetteer and an
# augmented road gazetteer

# Clean OSM road data for a raw roads gazetteer
source(file.path(code_osm_dir, "code", "roads", "01_create_raw_road_gazetteer.R"))

# Augment roads gazetteer
source(file.path(code_osm_dir, "code", "roads", "02_augment_road_gazetteer.R"))


# 3. Analysis of words that come before landmark words =========================
# We use the truth tweet data to examine the words that come before the landmark
# word in the tweet that is used to geocode the tweet. This analysis is used to 
# inform the geoparsing algorithm.

word_bf_lndmrk_dir <- file.path(code_tweets_truth_dir, "code", "word_before_landmark_word_analysis")

# Process Data
source(file.path(word_bf_lndmrk_dir, "01_words_before_location_word.R"))
source(file.path(word_bf_lndmrk_dir, "02_make_dataset_word_pairs.R"))

# Determine preposition tiers
source(file.path(word_bf_lndmrk_dir, "03_determine_preposition_tiers.R"))

# [figure_s3.png]
source(file.path(word_bf_lndmrk_dir, "03_top_words_before_landmark_figure.R"))

# [figure_s4.png]
source(file.path(word_bf_lndmrk_dir, "03_word_importance_figure.R"))

# 4. Tweet Classification Algorithm ============================================
# These scripts use the labeled tweet "truth" dataset, that classifies each 
# tweet as accident related or not. Use Naive Bayes and Support Vector Machine.
# Code preps tweet, performs grid search, makes a table of select results
# and saves the best model to be used later on.

# Functions for prepping tweets and for calculating precision and recall
source(file.path(code_tweets_classif_dir, "code", "_functions.R"))

# Cleans tweet names and determines which are potentially accident related
source(file.path(code_tweets_classif_dir, "code", "00_prep_tweets.R"))

# Trains Naive Bayes and SVM models using multiple parameters. Exports dataframe
# of results. Code may take 1+ hours to run
source(file.path(code_tweets_classif_dir, "code", "01_grid_search.R"))

# Cleans results data
source(file.path(code_tweets_classif_dir, "code", "02_clean_results_data.R"))

# Saves best model
source(file.path(code_tweets_classif_dir, "code", "03_save_best_model.R"))

# Results table [table_s4.tex]
source(file.path(code_tweets_classif_dir, "code", "04_results_table.R"))

# 5. Tweet Geoparse Algorithm ==================================================
# Geoparses crash tweets from the truth dataset. Tests algorithm using a number
# of gazetteers and implements geoparsing using LNEx.

# Functions for cleaning results data and calculating results metrics
source(file.path(code_tweets_geoparse_dir, "code", "_functions.R"))

# Clean tweets for geoparsing
source(file.path(code_tweets_geoparse_dir, "code", "01_clean_tweet_data_for_testing.R"))

# Implement geoparsing across multiple gazetteers. This script may take 24+ hours
if(IGNORE_TIMEINTENSIVE_SCRIPTS %in% F){
  source(file.path(code_tweets_geoparse_dir, "code", "02_implement_algorithm.R"))
}

# Implemnt LNEx algorithm for geoparsing. Script may take 24+ hours.
# RUN: /datawork/_master.py
# RUN: /datawork/tweet_geoparse_algorithm/code/02_implement_lnex_algorithm.py

# Append results and calculate precision/recall
source(file.path(code_tweets_geoparse_dir, "code", "03_calc_precision_recall.R"))

# Results tables
source(file.path(code_tweets_geoparse_dir, "code", "04_results_main.R"))
source(file.path(code_tweets_geoparse_dir, "code", "04_results_si.R"))

# Figure to illustrate algorithm
source(file.path(code_tweets_geoparse_dir, "code", "04_figure_illustrate_algorithm.R"))


# 6. Cluster Algorithm =========================================================
# The labeled truth dataset contains information on which tweets report the same
# crashes. We use the information to determine optimal clustering parameters
# for clustering tweets into crashes. We also show summary statistics of the 
# manually labelled clusters.

# Calculate rand and jaccard indices
source(file.path(code_tweets_truth_dir, "code", "uniqe_crash_truth_data_analysis", "cluster_jaccard_rand_calc.R"))

# Figure of rand and jaccard indices [figure_s5.png]
source(file.path(code_tweets_truth_dir, "code", "uniqe_crash_truth_data_analysis", "cluster_jaccard_rand_figure.R"))

# Cluster summary stats [table_s6.tex]
source(file.path(code_tweets_truth_dir, "code", "uniqe_crash_truth_data_analysis", "cluster_sum_stat_table.R"))


# 7. Truth Data Crash Cluster Analysis =========================================
# For the truth dataset, clusters tweets to individual crashes and individual 
# crashes to crash clusters. Computes stats of truth data.

# Cluster Tweets to Crashes
source(file.path(code_tweets_truth_dir, "code", "crash_cluster_analysis", "01_cluster_to_unique_crashes.R"))

# Cluster Crashes to Crash Clusters
source(file.path(code_tweets_truth_dir, "code", "crash_cluster_analysis", "02_create_crash_clusters.R"))

# Stats
source(file.path(code_tweets_truth_dir, "code", "crash_cluster_analysis", "03_stats.R"))


# 8. Apply Algorithm on Tweets =================================================
# Applies tweet classification and geoparse algorithm on tweets and produces
# a figure and map showing trends.

# Classify crashes tweets. May take 10+ minutes
source(file.path(code_tweets_all_dir, "code", "01_classify_crash_tweets.R"))

# Geoparse Tweets. This script may take over 96+ hours. 02a runs algorithm
# on 100 tweets at a time then exports those results. 02b appends them together
# and merges the coordinates with the main tweet dataframe
if(IGNORE_TIMEINTENSIVE_SCRIPTS %in% F){
  source(file.path(code_tweets_all_dir, "code", "02a_geocode_tweets.R"))
}
source(file.path(code_tweets_all_dir, "code", "02b_merge_geocodes_to_tweets.R"))


# Cluster tweets into uniqe crashes
source(file.path(code_tweets_all_dir, "code", "03_cluster_tweets.R"))

# Cluster unique crashes into crash clusters
source(file.path(code_tweets_all_dir, "code", "04_create_crash_clusters.R"))

# Figure: trends and map
source(file.path(code_tweets_all_dir, "code", "05_figure_crash_tweet_trends_map.R"))

# Figure: truth and full data heatmap
source(file.path(code_tweets_all_dir, "code", "05_figure_tweet_crashes_heatmap.R"))

# Figure: trends in all tweets
source(file.path(code_tweets_all_dir, "code", "05_figure_tweet_trends.R"))

# Tweets Stats
source(file.path(code_tweets_all_dir, "code", "05_stats.R"))


# 9. Analysis of landmark types ================================================
# Analysis of landmark types
source(file.path(code_tweets_truth_dir, "code", "landmark_type_analysis", "figure_landmark_types.R"))


# 10. Sendy Analysis ===========================================================
# Stats from Sendy Data
source(file.path(code_sendy_dir, "code", "proportion_crashes_verified.R"))

# CODE - FOR FIGURES/TABLES/STATS ==============================================
# Code for just reproducing the figures, tables and stats for the paper

# 1. Main Text Figures and Tables ----------------------------------------------

# Figure 1
source(file.path(code_tweets_geoparse_dir, "code", "04_figure_illustrate_algorithm.R"))

# Figure 2
source(file.path(code_tweets_all_dir, "code", "04_figure_crash_tweet_trends_map.R"))

# Figure 3 [Takes ~5 minutes]
source(file.path(code_tweets_all_dir, "code", "04_figure_tweet_crashes_heatmap.R"))

# Table 1
source(file.path(code_tweets_geoparse_dir, "code", "04_results_main.R"))

# 2. Supplementary Information Figures and Tables ------------------------------

# Figure S1
source(file.path(code_tweets_all_dir, "code", "04_figure_tweet_trends.R"))

# Figure S2 [Takes 1+ minute]
source(file.path(code_tweets_truth_dir, "code", "landmark_type_analysis", "figure_landmark_types.R"))

# Figure S3
source(file.path(code_tweets_truth_dir, "code", "word_before_landmark_word_analysis", "03_top_words_before_landmark_figure.R"))

# Figure S4
source(file.path(code_tweets_truth_dir, "code", "word_before_landmark_word_analysis", "03_word_importance_figure.R"))

# Figure S5
source(file.path(code_tweets_truth_dir, "code", "uniqe_crash_truth_data_analysis", "cluster_jaccard_rand_figure.R"))

# Table S4
source(file.path(code_tweets_classif_dir, "code", "04_results_table.R"))

# Table S5
source(file.path(code_tweets_geoparse_dir, "code", "04_results_si.R"))

# Table S6
source(file.path(code_tweets_truth_dir, "code", "uniqe_crash_truth_data_analysis", "cluster_sum_stat_table.R"))

# 3. In-Text Stats -------------------------------------------------------------

## All Tweets
source(file.path(code_tweets_all_dir, "code", "04_stats.R"))

## Truth Tweets
source(file.path(code_tweets_truth_dir, "code", "crash_cluster_analysis", "03_stats.R"))

## Sendy Data
source(file.path(code_sendy_dir, "code", "proportion_crashes_verified.R"))

## Determine Preposition Tiers
source(file.path(code_tweets_truth_dir, "code", "word_before_landmark_word_analysis", "03_determine_preposition_tiers.R"))


