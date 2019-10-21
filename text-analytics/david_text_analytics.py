import nltk
import numpy as np
import os
import pandas as pd
from rake_nltk import Rake
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import string
import sys
import pickle

# Function to remove punctuation from a string and convert to lowercase
def remove_punctuation(text):
    for punctuation in string.punctuation:
        text = text.replace(punctuation, '').lower()
    return text

# Function to combine strings
def combine_string(text):
    text = text.replace(" ", "")
    return text


# Expanding pandas column output
pd.set_option('display.max_columns', 5)

os.chdir('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced '
         'Analytics/Fall 2019/Fall 2/Text Analytics/Data')

# Reading in TED Talk data
transcripts = pd.read_csv('transcripts.csv')
ted_main = pd.read_csv('ted_main.csv')

# Deal with missing values

# Merge tables on URL
ted = pd.merge(left=ted_main, right=transcripts, on="url")
# ted.write_csv("ted.csv")

# Change pandas datetime to YYYYMMDD
ted['film_date'] = pd.to_datetime(ted['film_date'], unit='s')
ted['published_date'] = pd.to_datetime(ted['published_date'], unit='s')

##########################################################
# Task 1: Analyze speaker occupation by year
# Incomplete
##########################################################

# pd.set_option('display.max_rows', ted.shape[0]+1)
# ted['speaker_occupation'].value_counts()
# ted['year'] = [ted['film_date'][i].year for i in range(len(ted['film_date']))]
#
# ted_speaker_dict = {}
# for i in range(len(ted['speaker_occupation'])):
#     if ted['speaker_occupation'][i] not in ted_speaker_dict:
#         ted_speaker_dict[ted['speaker_occupation'][i]] = [ted['year'][i]]
#     else:
#         ted_speaker_dict[ted['speaker_occupation'][i]].append(ted['year'][i])

#############################################################################################
# Task 2: Create recommendation engine based on title, main speaker, description, tags
#############################################################################################

# Filters data frame to key features title, main speaker, description, tags. URL will be used to load next talk
final_ted = ted.filter(['title', 'main_speaker', 'description', 'tags', 'url'])

# Removes name of speaker from description before pre-processing
for i in range(len(final_ted['description'])):
    if final_ted['main_speaker'][i] in final_ted['description'][i]:
        final_ted['description'][i] = final_ted['description'][i].replace(final_ted['main_speaker'][i], "")
# 2372 names replaced, need to look into this. Names not removed designate multiple speakers or groups without speaker names in description

# Cleaning final_ted columns, making all columns lowercase
for column in final_ted:
    if column == 'main_speaker':
        final_ted[column] = final_ted[column].apply(combine_string)
    if column in ['title', 'url', 'tags']:
        continue
    final_ted[column] = final_ted[column].apply(remove_punctuation)

# Distilling TED Talk description down to key words for each talk
final_ted['key_words'] = ""
for index, row in final_ted.iterrows():
    desc = row['description']

    # Uses a NLTK Rake object. English stopwords and punctuation removed.
    rake = Rake()

    # Extracting key words from TED Talk description
    rake.extract_keywords_from_text(desc)

    # Key words and scores for key words
    dict_keys_scores = rake.get_word_degrees()

    # assigning the key words to the new column for the corresponding movie
    row['key_words'] = [row['main_speaker']]+list(dict_keys_scores.keys())

# Removing description column
final_ted.drop(columns=['description'], inplace=True)

# New data frame with keywords, indexed by title. Converts key_words lists to comma-delimited string
keyword_df = final_ted.filter(['key_words'])
keyword_df = keyword_df.set_index(final_ted['title'])

for i in range(len(keyword_df['key_words'])):
    keyword_df['key_words'][i] = ",".join([str(x) for x in keyword_df['key_words'][i]])

# Instantiates CountVectorizer
count = CountVectorizer()
count_matrix = count.fit_transform(keyword_df['key_words'])

# Cosine similarity matrix for TED Talks
cosine_sim = cosine_similarity(count_matrix, count_matrix)


# Generate similar TED Talks based on title input
# Matches movie titles to index
indices = pd.Series(keyword_df.index)


#  Function. Accepts movie title as input, returns list of 10 most similar TED Talks ranked by similarity to title
def recommendations(title, cosine_sim=cosine_sim):
    # Empty list of recommended movies
    recommended_movies = []

    # Extracts index of movie that matches title
    idx = indices[indices == title].index[0]

    # Similarity scores, descending
    score_series = pd.Series(cosine_sim[idx]).sort_values(ascending=False)

    # Top 10 most similar movies
    top_10_indexes = list(score_series.iloc[1:11].index)

    # Appends top 10 movies recommended_movies
    for i in top_10_indexes:
        recommended_movies.append(list(keyword_df.index)[i])

    # Returns final list
    return recommended_movies

# Saving objects to pickle file to speed up loading for application
data = [cosine_sim, keyword_df]
f = open("store.pckl", "wb")
for i in data:
    pickle.dump(i, f)
f.close()
