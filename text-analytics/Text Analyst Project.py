import os
os.getcwd()
os.chdir("C:/Users/Savannah Hampton/Documents/Master of Science in Analytics/Text Analytics/ted-talks")

import pandas as pd

transcript=pd.read_csv('transcripts.csv')
main=pd.read_csv('ted_main.csv')
pdtranscript=pd.DataFrame(transcript)
pdmain=pd.DataFrame(main)

pd.merge(pdtranscript,pdmain,on="url",how="inner")
newtedtalks=pd.merge(pdtranscript,pdmain,on="url",how="inner")

print(newtedtalks.head)
print(len(newtedtalks))

#export=newtedtalks.to_csv('newtedtalks.csv')

import nltk
nltk.download('punkt')
nltk.download('stopwords')
import re
import string
doc = []

for line in newtedtalks['transcript']:
    doc.append(line)


punc=re.compile( '[%s]' % re.escape( string.punctuation ))
term_vec=[]

for d in doc:
    d=d.lower()
    d=re.sub(r'[\(,\),\.]', '  ',d)
    term_vec.append( nltk.word_tokenize(d))


for vec in term_vec:
    print(vec)
    
stop_words=nltk.corpus.stopwords.words('english')

for i in range(0, len(term_vec)):
    term_list=[]
    
    
    for term in term_vec[i]:
        if term not in stop_words:
            term_list.append(term)
        
    term_vec[i]=term_list


for vec in term_vec:
    print(vec)


porter=nltk.stem.porter.PorterStemmer()

for i in range(0, len(term_vec)):
    for j in range(0, len(term_vec[i])):
        term_vec[i][j]=porter.stem(term_vec[i][j])


for vec in term_vec:
    print(vec)


import gensim
dict=gensim.corpora.Dictionary(term_vec)

corp=[]
for i in range(0, len(term_vec)):
    corp.append(dict.doc2bow(term_vec[i]))

tfidf_model=gensim.models.TfidfModel(corp)

tfidf=[]
for i in range(0, len(corp)):
    tfidf.append(tfidf_model[corp[i]])
    
n=len(dict)
index=gensim.similarities.SparseMatrixSimilarity(tfidf_model[corp],num_features=n)

for i in range(0, len(tfidf)):
    s='Doc ' + str(i+1) + ' TFIDF: '
    
    for j in range(0, len(tfidf[i])):
        s=s+' (' + dict.get(tfidf[i][j][0]) + ','
        s=s+( '%.3f' % tfidf[i][j][1]) + ')'
        
    print(s)
    
for i in range(0, len(corp)):    
    sim = index[ tfidf_model[ corp[ i ] ] ]


from sklearn.cluster import KMeans

import numpy as np
sim=np.array(sim).reshape(-1,1)

from sklearn.preprocessing import MinMaxScaler
import matplotlib.pyplot as plt
mms=MinMaxScaler()
mms.fit(sim)
data_transformed=mms.transform(sim)

Sum_of_squared_distances=[]
K=range(1,15)
for k in K:
    km=KMeans(n_clusters=k)
    km=km.fit(data_transformed)
    Sum_of_squared_distances.append(km.inertia_)

plt.plot(K, Sum_of_squared_distances, 'bx-')
plt.xlabel('k')
plt.ylabel('Sum_of_squared_distances')
plt.title('Elbow Method for Optimal k')
plt.show()

num_clusters=6
km=KMeans(n_clusters=num_clusters,random_state=42).fit(sim)
print(km)

newtedtalks['kmeans_cluster']=km.labels_
print(newtedtalks['kmeans_cluster'])

from collections import Counter
Counter(km.labels_)       
        
tedtalks_clusters=(newtedtalks[['title','kmeans_cluster']].sort_values(by=['kmeans_cluster','title'],ascending=False))
print(tedtalks_clusters)

type(tedtalks_clusters)
tedtalkscsv=pd.DataFrame(tedtalks_clusters)
tedtalkscsv.to_csv('sixclusters.csv')

num_clusters=10
km=KMeans(n_clusters=num_clusters,random_state=42).fit(sim)
print(km)

newtedtalks['kmeans_cluster']=km.labels_
print(newtedtalks['kmeans_cluster'])

from collections import Counter
Counter(km.labels_)       
        
tedtalks_clusters=(newtedtalks[['title','kmeans_cluster']].sort_values(by=['kmeans_cluster','title'],ascending=False))
print(tedtalks_clusters)

type(tedtalks_clusters)
tedtalkscsv=pd.DataFrame(tedtalks_clusters)
tedtalkscsv.to_csv('tenclusters.csv')

sixclusters=pd.read_csv('sixclusters.csv')
print(sixclusters)

isclusterfive=tedtalks_clusters['kmeans_cluster']==5
clusterfive=tedtalks_clusters[isclusterfive]
print(clusterfive)
        
        
full_dict = {}  
for i in range(len(tfidf)):
    full = {i + 1 : {'tf': []}}
    for j in range(0, len(tfidf[i])):
        element = [dict.get(tfidf[i][j][0]), tfidf[i][j][1]]
        full[i+1]['tf'].append(element)
    full_dict.update(full)
print(full_dict)


cluster_dict={}
for i in range(0,7):
    cluster_list=[]
    cl=sixclusters.loc[sixclusters['kmeans_cluster']==i]
    for j in range(len(cl)):
        ex=[cl.loc[cl['title']]]
        cluster_list.append(ex)
        cluster_dict.update(i, cluster_list)


os.chdir("C:/Users/Savannah Hampton/Documents/Master of Science in Analytics/Text Analytics/ted-talks/sentiment_module")

from sentiment_module import sentiment