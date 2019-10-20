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

from sklearn.feature_extraction.text import CountVectorizer

vectorizer=CountVectorizer()
X_tf=vectorizer.fit_transform(newtedtalks['transcript'])
shp=X_tf.toarray().shape
print(f'Documents: {shp[0]}; Unique Terms: {shp[1]}')

#Documents: 2467; Unique Terms: 58795

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

from sklearn.decomposition import LatentDirichletAllocation
lda=LatentDirichletAllocation(n_components=10, learning_method='online')
lda.fit(X_tf)
X_lda=lda.transform(X_tf)
vocab=vectorizer.get_feature_names()
n_top_words=10
concept_term=[]
for i,topic in enumerate(lda.components_):
    print(f'Topic{i+1}: ', end='')
    idx_list=topic.argsort()
    concept_term.append([vocab[i] for i in idx_list])
    print(concept_term[-1])


print(concept_term[0][0:10])
print(concept_term[1][0:10])
print(concept_term[2][0:10])
print(concept_term[3][0:10])
print(concept_term[4][0:10])
print(concept_term[5][0:10])
print(concept_term[6][0:10])
print(concept_term[7][0:10])
print(concept_term[8][0:10])
print(concept_term[9][0:10])

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


def extract_top_terms(term, mat, n):
    term_n=[]
    for i in range(0, len(mat)):
        wt=mat[i]
        term_nm=term[i]
        if wt==0:
            continue
        if term_nm.isdigit():
            try:
                int(term_nm)
                float(term_nm)
                continue
            except:
                pass
        if len(term_n)<n:
            term_n.append([term_nm, wt])
            term_n.sort(key=lambda x:x[1], reverse=True)
        elif wt> term_n[-1][1]:
            term_n[-1]=[term_nm, wt]
            term_n.sort(key=lambda x:x[1],reverse=True)
    return term_n

def extract_clust_docs(cluster):
    doc=[]
    for i in range(0, len(newtedtalks)):
        if newtedtalks[i]!=cluster:
            continue
        elif len(newtedtalks[i])>0:
            doc.append(newtedtalks[i])
        return doc

from sklearn.feature_extraction.text import TfidfVectorizer
tfidf_vectorizer=TfidfVectorizer()
min_ID=min(newtedtalks['kmeans_cluster'])
max_ID=max(newtedtalks['kmeans_cluster'])
term_nm=[]
mat=[]
collection=[]
for cluster in range(min_ID, max_ID + 1):
    collection.append(extract_clust_docs(cluster))
    X_tfidf=tfidf_vectorizer.fit_transform(collection[-1])
    print(f'Cluster {cluster + 1}, ', end='')
    print(f'{X_tfidf.shape[0]} documents, ', end='')
    print(f'{X_tfidf.shape[1]} unique term')
    term_nm.append(tfidf_vectorizer.get_feature_names())
    mat.append(X_tfidf.toarray()[0])
print()
for cluster in range(0, len(mat)):
    print(f'Cluster {cluster + 1}: ', end='')
    top_term=extract_top_terms(term_nm[cluster],mat[cluster],10)
    for i in range(0, len(top_term)):
        print(f'({top_term[i][0]},{top_term[i][1]:.3f}) ', end='')
    print()

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

clusterfive=[]
clusterfour=[]
clusterthree=[]
clustertwo=[]
clusterone=[]
clusterzero=[]

sixclusterspd=pd.DataFrame(sixclusters)

for i in range(0, len(sixclusterspd)):
    if (sixclusterspd['kmeans_cluster'][i]==5):        
        clusterfive.append(sixclusters['document'][i])
    elif (sixclusterspd['kmeans_cluster'][i]==4):
        clusterfour.append(sixclusters['document'][i])
    elif (sixclusterspd['kmeans_cluster'][i]==3):
        clusterthree.append(sixclusters['document'][i])
    elif (sixclusterspd['kmeans_cluster'][i]==2):
        clustertwo.append(sixclusters['document'][i])
    elif (sixclusterspd['kmeans_cluster'][i]==1):
        clusterone.append(sixclusters['document'][i])
    else:
        clusterzero.append(sixclusters['document'][i])

len(clusterfive)
len(clusterfour)
len(clusterthree)
len(clustertwo)
len(clusterone)
len(clusterzero)
    
full_dict = {}  
for i in range(len(tfidf)):
    full = {i + 1 : {'tf': []}}
    for j in range(0, len(tfidf[i])):
        element = [dict.get(tfidf[i][j][0]), tfidf[i][j][1]]
        full[i+1]['tf'].append(element)
    full_dict.update(full)
print(full_dict)

print(full_dict[0]['tf'])

tdfvaluesfive=[]
for i in len(clusterfive):
    tdfvaluesfive.append()


cluster_dict={}
for i in range(0,7):
    cluster_list=[]
    cl=sixclusters.loc[sixclusters['kmeans_cluster']==i]
    for j in range(len(cl)):
        ex=[cl.loc[cl['title']]]
        cluster_list.append(ex)
        cluster_dict.update(i, cluster_list)
        
#document # for the given cluster
#document # for cluster 0


os.chdir("C:/Users/Savannah Hampton/Documents/Master of Science in Analytics/Text Analytics/ted-talks")

from sentiment_module import sentiment

splittedlist=[]
for line in newtedtalks['transcript']:
    splittedlist.append(line.split())

for line in splittedlist:
    sentiment.exist(line)
    sentiment.sentiment(line)

for i in range(0, len(splittedlist)):
    print(sentiment.sentiment(splittedlist[i]))

for i in range(2200, 2467):
    print(sentiment.sentiment(splittedlist[i]))

#highest valence: 8.13: An electrifying acoustic guitar performance
#average valence: 5.32
#highest arousal: 5.32: An electrifying acoustic guitar performance
#average arousal: 4.51

import pandas as pd
sentiment=pd.read_csv('Sentiment Analysis.csv')
print(sentiment)
#comments y, #sentiment on x
