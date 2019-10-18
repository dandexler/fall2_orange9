import datetime as dt
import gensim
from matplotlib import pyplot as plt
import nltk
nltk.download('wordnet')

import numpy as np
import os
import pandas as pd
from sklearn import model_selection
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
from sklearn.metrics import adjusted_rand_score
import spacy

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
documents = [train['title'][i].lower() for i in range(1, len(train['title'])) ]

# Can skip cleaning, no HTML or XML present

# Tokenize the titles
tokenizer = nltk.tokenize.RegexpTokenizer(r'\w+')
token_docs = [tokenizer.tokenize(str(x)) for x in documents]

# Removing stop words
stop_words = nltk.corpus.stopwords.words( 'english' )
new_token_docs = []
for i in range(len(token_docs)):
    stop_rem = [x for x in token_docs[i] if x not in stop_words]
    new_token_docs.append(stop_rem)


# Lemmatize the titles
lemmatizer = nltk.stem.WordNetLemmatizer
lemma_docs = []
for i in range(len(new_token_docs)):
    lemmatized_output = ' '.join([lemmatizer.lemmatize(w, w) for w in new_token_docs[i]])
    lemma_docs.append(lemmatized_output)

# Vectorize the titles, by English stop words
vectorizer = TfidfVectorizer(stop_words='english')
lemma_docs_transf = vectorizer.fit_transform(lemma_docs)





# Alternatively, can use full NLP pipeline using spaCy
# Incomplete!

# Load the large English NLP model
nlp = spacy.load('en_core_web_lg')

# The text we want to examine
text = 'test'

# Parse the text with spaCy. This runs the entire pipeline.
doc = nlp(text)

# 'doc' now contains a parsed version of text. We can use it to do anything we want!
# For example, this will print out all the named entities that were detected:
for entity in doc.ents:
    print(f"{entity.text} ({entity.label_})")