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


pip install numpy
pip install scipy
install gensim
import gensim
dict=gensim.corpora.Dictionary(term_vec)

corp=[]
for i in range(0, len(term_vec)):
    corp.append(dict.doc2bow(term_vec[i]))

tdidf_model=gensim.models.TfidfModel(corp)

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
    print('Doc', (i+1), 'sim: [',) #doesn't create another line
    
    sim=index[tfidf_model]
    for j in range(0, len(sim)):
        print('%.3f' %sim[j],)
    
    print(']')