import pandas as pd
import numpy as np 
import seaborn as sns
import matplotlib.pyplot as plt
from mlxtend.frequent_patterns import apriori
from mlxtend.frequent_patterns import association_rules
import mlxtend as ml
# Uncomment and run the following line if you have trouble loading mlextend like I did
# pip install mlxtend --no-deps

# Read in restaurantData.csv as pandas data frame named 'restaurant'
restaurant = pd.read_csv(r"C:\Users\zachm\Documents\Fall\Module 2\Data Mining\restaurantData.csv")

# Count plot for items in restaurant data
sns.countplot(x = 'order', data = restaurant, order = restaurant['order'].value_counts().iloc[:10].index)
plt.xticks(rotation=90)

# Group restaurant data frame by orderNumber and count of each item ordered
df = restaurant.groupby(['orderNumber','order']).size().reset_index(name='count')
basket = (df.groupby(['orderNumber', 'order'])['count']
          .sum().unstack().reset_index().fillna(0)
          .set_index('orderNumber'))

#The encoding function
def encode_units(x):
    if x <= 0:
        return 0
    if x >= 1:
        return 1
basket_sets = basket.applymap(encode_units)

# Generate frequent item sets with a minimum support of at least 1%
frequent_itemsets = apriori(basket_sets, min_support=0.01, use_colnames=True)
rules = association_rules(frequent_itemsets, metric="lift")
rules.sort_values('confidence', ascending = False, inplace = True)
rules.head(10)

# Output rules set to csv
rules.to_csv(r"C:\Users\zachm\Documents\Fall\Module 2\Data Mining\rules.csv")

# Frequency tables of ordered items
df_freq = pd.crosstab(restaurant['order'], columns='count')
df_freq['freq'] = df_freq['count']/df_freq['count'].sum()

#Note: 
# Cantina Pinot Bianco - 18.06%
# Pork Tenderloin - 18.37%







