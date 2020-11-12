import json, re
from shapely.geometry import MultiPoint

import sys 

import getpass
import os
if getpass.getuser() == 'WB521633': project_file_path = 'C:/Users/wb521633/Dropbox/World Bank/IEs/CrashMap-Nairobi/'
if getpass.getuser() == 'robmarty': project_file_path = '/Users/robmarty/Dropbox/World Bank/IEs/CrashMap-Nairobi/'

os.chdir(r'' + project_file_path + 'Code/Test Algorithms for Geocoding Tweets/LNEx/LNEx')
sys.path.append(r'' + project_file_path + 'Code/Test Algorithms for Geocoding Tweets/LNEx/LNEx')
import LNEx as lnex

#sys.path.append("LNEx")
#import LNEx as lnex

def read_tweets():
    tweets_file = r'' + project_file_path + 'Code/Test Algorithms for Geocoding Tweets/LNEx/LNEx/' + '_Data/sample_tweets.txt'
    # read tweets from file to list
    with open(tweets_file) as f:
        tweets = f.read().splitlines()
    return tweets

def init_using_elasticindex(bb, cache, augmentType, dataset, capital_word_shape):
    lnex.elasticindex(conn_string='localhost:9200', index_name="photon")

    geo_info = lnex.initialize( bb, augmentType=augmentType,
                                    cache=cache,
                                    dataset_name=dataset,
                                    capital_word_shape=capital_word_shape)
    return geo_info

bbs = { "chennai": [12.74, 80.066986084, 13.2823848224, 80.3464508057],
        "louisiana": [29.4563, -93.3453, 31.4521, -89.5276],
        "houston": [29.4778611958, -95.975189209, 30.1463147381, -94.8889160156]}

dataset = "chennai"

geo_info = init_using_elasticindex(bbs[dataset], cache=False, augmentType="HP", 
                                   dataset=dataset, capital_word_shape=False)


tweet = u'ChennaiFloods A pregnant lady rescued from Mudichur Near tambaram around 1130 this morning by Indian Navy https://t.co/I5ZYe4'

for tweet in read_tweets():
    for output in lnex.extract(tweet):
        print(output[0], output[1], output[2], output[3]["main"])
    print("#"*50)
