import numpy as np
import os
import pickle
import pandas as pd

print(os.getcwd())

def loadall(filename):
    with open(filename, "rb") as f:
        while True:
            try:
                yield pickle.load(f)
            except EOFError:
                break


def recommendations(title, cosine_sim=items[0]):
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
        recommended_movies.append(list(items[1].index)[i])

    # Returns final list
    print("Recommended for you, Orange Team 9:")
    return recommended_movies


items = list(loadall("store.pckl"))
indices = pd.Series(items[1].index)
