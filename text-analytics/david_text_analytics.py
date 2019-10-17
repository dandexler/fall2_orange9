import datetime as dt
import gensim
from matplotlib import pyplot as plt
import nltk
import numpy as np
import os
import pandas as pd
from sklearn import model_selection
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
from sklearn.metrics import adjusted_rand_score

# Expanding pandas column output
pd.set_option('display.max_columns', 5)

os.chdir('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced '
         'Analytics/Fall 2019/Fall 2/Text Analytics/Data')

# Reading in TED Talk data
transcripts = pd.read_csv('transcripts.csv')
ted_main = pd.read_csv('ted_main.csv')

# Checking for missing values
print('Missing values in transcripts?:','\n', transcripts.isnull().sum())
print('Missing values in ted_main?:', '\n', ted_main.isnull().sum())
print()
# Result: speaker_occupation variable in ted_main has 6 missing values

# Deal with missing values

# Merge tables on URL
ted = pd.merge(left=ted_main, right=transcripts, on="url")
# ted.write_csv("ted.csv")

# Change pandas datetime to YYYYMMDD
ted['film_date'] = pd.to_datetime(ted['film_date'], unit='s')
ted['published_date'] = pd.to_datetime(ted['published_date'], unit='s')

# Data splitting, seed=1
np.random.seed(1)
ted['random_number'] = np.random.randn(len(ted))

# 80/20 split of TED talk data, reset indexing
train = ted[ted['random_number'] <= 0.8]
train.index = range(len(train))
test = ted[ted['random_number'] > 0.8]
test.index = range(len(test))

# Creating a documents list, titles of TED talks
documents = [train['title'][i] for i in range(1, len(train['title'])) ]

# Can skip cleaning, no HTML or XML present

# Tokenize the titles
tokenizer = nltk.tokenize.RegexpTokenizer(r'\w+')
token_docs = [tokenizer.tokenize(str(x)) for x in documents]

# Lemmatize the titles
lemma_docs = "placeholder"

# Vectorize the titles, by English stop words
vectorizer = TfidfVectorizer(stop_words='english')
lemma_docs_transf = vectorizer.fit_transform(lemma_docs)