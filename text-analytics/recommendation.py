import pickle
import pandas as pd
import os

# __file__ will throw error if running from interpreter. Run from bash/cmd.exe
ROOT_PATH = os.path.dirname(os.path.realpath(__file__))

# Full filepath to pickle data. Assumed to be in same directory as recommendation.py
myfile_path = os.path.join(ROOT_PATH, "store.pckl")

# Load all data from pickle data dump. Data assumed to be in same working directory as recommendation.py
def loadall(filename):
    with open(filename, "rb") as f:
        while True:
            try:
                yield pickle.load(f)
            except EOFError:
                break


# Call to function loadall, which loads pickle data
items = list(loadall("store.pckl"))


# Recommendation generator with data pulled in from pickle file
def recommendations(title, cosine_sim=items[0]):
    # Empty list of recommended movies
    recommended_talks = []

    # Extracts index of movie that matches title
    idx = indices[indices == title].index[0]

    # Similarity scores, descending
    score_series = pd.Series(cosine_sim[idx]).sort_values(ascending=False)

    # Top 10 most similar movies
    top_10_indexes = list(score_series.iloc[1:11].index)

    # Appends top 10 movies recommended_movies
    for i in top_10_indexes:
        recommended_talks.append(list(items[1].index)[i])

    # Returns final list
    return recommended_talks

# List index of title names
indices = pd.Series(items[1].index)

# Intended to run from the command line, allows user to continually input title of talk
while True:
    inp = input("Type a TED Talk title name or q to quit: ")
    if inp.lower() == 'q':
        print("Exit.")
        quit()
    else:
        print("\nRecommended for you, Orange Team 9:")
        rec_talks = recommendations(inp)
        for i in rec_talks:
            print(i)
