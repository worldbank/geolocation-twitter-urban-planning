# -*- coding: utf-8 -*-

# Implement LNEx Algorithm in Truth Tweets

# Setup -----------------------------------------------------------------------
# Setup Project Filepath
code_dir = '~/Documents/Github/geolocation-twitter-urban-planning'
tweets_geoparse_dir = '~/Dropbox/World Bank/IEs/Twitter for Urban Planning/data/tweet_geoparse_algorithm'
landmarkgaz_dir = '~/Dropbox/World Bank/IEs/Twitter for Urban Planning/data/landmark_gazetteer'

# Load Packages
import json, re
from shapely.geometry import MultiPoint
import os
import sys
import pandas as pd
import numpy as np
import re
import pyreadr
import itertools as IT
import collections
import string
#import centerline
import geopandas as gpd
import osmnx as ox
import networkx as nx

# For some reason need to run this multiple times to get working
os.chdir(r'' + code_dir + '/functions_and_packages/LNEx')
sys.path.append(r'' + code_dir + '/functions_and_packages/LNEx')
import LNEx as lnex

os.chdir(r'' + code_dir + '/functions_and_packages/LNEx/LNEx')
sys.path.append(r'' + code_dir + '/functions_and_packages/LNEx/LNEx')
import LNEx as lnex

os.chdir(r'' + code_dir + '/functions_and_packages/LNEx')
sys.path.append(r'' + code_dir + '/functions_and_packages/LNEx')
import LNEx as lnex

import gaz_augmentation_and_filtering as LNEx_aug

# Define Functions ------------------------------------------------------------
# Function for Extracting Coordinates from Tweets. Exports a dataset in "long"
# format, where each row is one location found by LNEx.
def lnex_extract_to_df(x):

    # Run LNEx
    lnex_output = lnex.extract(u'' + tweets_df['tweet'][x])

    if len(lnex_output) > 0:

        # LNEx output to dataframe
        lnex_output_df = pd.DataFrame(lnex_output, columns = ['name1', 'tweet_loc', 'name2', 'geo_info_id'])

        # List of list of keys - flatten to one list
        keys_list = lnex_output_df.geo_info_id.tolist()
        keys = [y for x in keys_list for y in x]

        # Grab locations
        gaz_out = gazetteer[gazetteer['key'].isin(keys)]

        # Add variables from truth data
        gaz_out['tweet'] = tweets_df['tweet'][x]
        gaz_out['latitude_truth'] = tweets_df['latitude_truth'][x]
        gaz_out['longitude_truth'] = tweets_df['longitude_truth'][x]
        gaz_out['status_id_str'] = tweets_df['status_id_str'][x]

    else:
        gaz_out = tweets_df.iloc[[x]]

    return gaz_out

# Function to Convert Pandas Dataframe into Dictionary Needed for LNEx
def pandas_to_geo_locations(df):

    geo_locations_temp = {}

    for name in df.name.unique().tolist():

        keys = gazetteer[gazetteer.name == name].key
        geo_locations_temp.update({u'' + name: keys.tolist()})

    return(geo_locations_temp)

# Load and Prep Tweets --------------------------------------------------------
### Truth Tweets
tweets_df = pd.read_csv(r'' + tweets_geoparse_dir + '/processed_data/tweets_for_geolocation.csv')

### Deal with string encoding issues
tweets_df[u'tweet'] = [re.sub(r'[^\x00-\x7f]', r'', tweet) for tweet in tweets_df[u'tweet']]

### Remove hashtags (causes lnex to fail)
tweets_df[u'tweet'] = [re.sub("#", "", tweet) for tweet in tweets_df[u'tweet']]

# Load and Prep Gazetteers ----------------------------------------------------
### Load
gazetteer = pd.read_csv(r'' + landmarkgaz_dir + '/data/gazetteers/landmark_gazetter_raw.csv', \
    encoding = "ISO-8859-1", engine='python')

### Restrict to key variables
gazetteer = gazetteer[['name', 'lat', 'lon']]
gazetteer['key'] = range(gazetteer.shape[0])

### Initialize
geo_locations = pandas_to_geo_locations(gazetteer)
#geo_info = pandas_to_geo_info(gazetteer)

### Augment
#new_geo_locations, extended_words = LNEx_aug.no_augment(geo_locations)
new_geo_locations = dict()
for x in geo_locations:
    new_geo_locations[x.lower()] = geo_locations[x]
extended_words = LNEx_aug.get_extended_words3(list(new_geo_locations.keys()))

# Initialize and Implement LNEx -----------------------------------------------
#### Initialize
lnex.initialize_using_files(new_geo_locations, extended_words)

#### Implement LNEx
lnex_results_list = map(lnex_extract_to_df, range(len(tweets_df)))
lnex_results_df = pd.concat(lnex_results_list, ignore_index=True)

# Export ----------------------------------------------------------------------
lnex_results_df.to_csv(r'' + tweets_geoparse_dir + '/processed_data/tweet_geoparse_lnex.csv', encoding='utf-8')
