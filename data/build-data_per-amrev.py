# %%
import os 
import contractions
import re
import regex 
import uuid
import scipy
import pickle 
import json
import numpy as np
import pandas as pd 
from nltk.tokenize import sent_tokenize
from nltk.stem.porter import *
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import PCA

# %% [markdown]
# Steps from Xiao et al. (2018)
# 
# 1) [X] convert uppercase text to lowercase
# 2) [ ] replace common nonword pattern with a unique identifier (e.g., mapping smileys “:-)” to “⟨happy⟩”)
# 3) [X] remove HTML tags and any character that is neither alphanumeric nor a punctuation
# 4) [X] normalize contractions (e.g., transforming “don’t” to “do not”)
# 5) [X] “function words” on the SMART stoplist [60] are removed from the vocabulary
# 6) [X] remaining words are stemmed. 
# 7) [X] each review is considered as a bag, and each sentence in the review is treated as an instance. [33]
# 8) [X] Each instance is represented as a TF-IDF feature vector, and each feature is normalized. 
# 9) [X] Finally, Principal component analysis (PCA) is performed to reduce the data to 200 features.

# %% [markdown]
# Define the output format 
# 
# - Columns: Rating, ReviewID, SentenceID, {either: TF-IDF features or PCA features; depending on whether I do PCA in Python or R}
# - Rows: each row represents a sentence from a review and product combination 
# 
# 

# %% [markdown]
# The Amazon reviews data contains text of reviews based on 6 different product category classes and a score rating from 1 to 5.  The number of reviews depends on the dataset used, but it is very large and so we only want to pull a small portion of them for training and testing.  We select 1000 bags (reviews) for training, apply the steps from Xiao et al. (above) to this subset.  Then we construct the test set by applying the fitted TFIDF and PCA functions to 20,000 randomly sampled test reviews (that weren't used in training).  We repeat these steps 20 times.   

# %%
def cleanhtml(raw_html):
    # see https://stackoverflow.com/questions/9662346/python-code-to-remove-html-tags-from-a-string
    reg = re.compile('<.*?>|&([a-z0-9]+|#[0-9]{1,6}|#x[0-9a-f]{1,6});')
    return re.sub(reg, '', raw_html)

def preprocess(text):
    # Convert uppercase to lowercase 
    text = text.lower() 

    # TODO: Replace common nonword pattern with unique identifier

    # Remove HTML tags, non-alphanumeric, and non-punctuation characters
    text = cleanhtml(text)
    text = regex.sub(r'[^\p{Latin}\p{posix_punct}0-9 ]', u'', text)

    # Normalize contractions
    text = contractions.fix(text)
    
    return text

# Import stop words from the SMART list 
# see: https://rdrr.io/cran/stopwords/man/data_stopwords_smart.html
f = open('data/per-amrev/smart_stopwords.txt',)
stop_words = f.readlines()
stop_words = [line.rstrip("\n") for line in stop_words]
stop_words_stem = [PorterStemmer().stem(w) for w in stop_words]

def my_stem(tokens):
    stemmer = PorterStemmer()
    for t in tokens:
        t = stemmer.stem(t)
        yield t

class CustomVectorizer(TfidfVectorizer):
    def build_tokenizer(self):
       tokenize = super().build_tokenizer()
       return lambda doc: list(my_stem(tokenize(doc)))

# Pull in all review information from .json files 
def load_reviews(data_dir): 
    reviews = []
    ratings = []
    ids = []
    for fname in os.listdir(data_dir):
        # print(fname)
        with open(data_dir + fname) as f:
            data = json.load(f)
            for rev in data['Reviews']:
                if rev['Content'] is not None:
                    reviews.append(rev['Content'])
                    ratings.append(rev['Overall'])
                    ids.append(rev['ReviewID'])
    
    return reviews, ratings, ids 

# need to make the reviews unique based on their id.  
def make_unique(reviews, ratings, ids):
    data = [
        pd.Series(reviews), 
        pd.Series(ratings), 
        pd.Series(ids)
    ]
    temp = pd.concat(data, axis = 1, join = 'inner')
    temp = temp.drop_duplicates()

    return temp[0].tolist(), temp[1].tolist(), temp[2].tolist()

# Tokenize sentences and keep track of review level info
def tokenize_sentences(reviews, ratings, ids):
    sentences = []
    ratings_s = []
    ids_s = []
    for i in range(len(reviews)):
        rev = reviews[i]
        rev = preprocess(rev)
        r = ratings[i]
        id_ = ids[i]
        for sent in sent_tokenize(rev):
            sentences.append(sent)
            ratings_s.append(r)
            ids_s.append(id_)

    return sentences, ratings_s, ids_s

# Randomly select from the reviews, ratings, and ids lists
def randomly_select(n, rng, reviews, ratings, ids):
    ind = [int(i) for i in rng.random(n) * len(reviews)]

    reviews = [reviews[i] for i in ind]
    ratings = [ratings[i] for i in ind]
    ids = [ids[i] for i in ind]
    return reviews, ratings, ids 

def list_pull(n1, n2, list_of_lists):
    a = [l[0:n1] for l in list_of_lists]
    b = [l[n1:n1+n2] for l in list_of_lists]
    return a, b 

# Concat the pca matrix to the ratings and ids 
def my_concat(pca_matrix, ratings, ids):
            data = [
                pd.Series(ratings), 
                pd.Series(ids), 
                pd.DataFrame(data = pca_matrix)
            ]
            out = pd.concat(data, axis = 1, join = 'inner')

            out.columns = ["rating", "review_id"] + [f"pca_{i+1}" for i in range(pca_matrix.shape[1])]
            return out

# %%
datasets = ['cameras', 'laptops', 'mobilephone', 'tablets', 'TVs', 'video_surveillance']
n_rep = 20
n_train = 1000
n_test = 20000
rng = np.random.default_rng(8)

for dataset in datasets:
    print(dataset)
    # data_dir = f'raw/{dataset}/'
    # data_dir = f'/Users/spkent/Documents/_Reference/AmazonReviews/{dataset}/'
    data_dir = f'data/per-amrev/raw/{dataset}/'
    reviews, ratings, ids = load_reviews(data_dir)
    reviews, ratings, ids = make_unique(reviews, ratings, ids)
    
    for i in range(n_rep):
        print(n_train, i)

        # Set up TFIDF and PCA models
        vectorizer = CustomVectorizer(stop_words=stop_words_stem, preprocessor=preprocess, min_df=5)
        pca = PCA(n_components=200)

        # Randomly pull reviews for training
        random_reviews = randomly_select(n_train + n_test, rng, reviews, ratings, ids)
        train_reviews, test_reviews = list_pull(n_train, n_test, random_reviews) 
        
        sentences, ratings_s, ids_s = tokenize_sentences(*train_reviews)
        sentences_test, ratings_s_test, ids_s_test = tokenize_sentences(*test_reviews)

        # Fit TFIDF and PCA models to training data 
        tfidf = vectorizer.fit_transform(sentences)
        tfidf_pca = pca.fit_transform(tfidf.toarray())  
        # print(pd.DataFrame(tfidf.toarray(),columns=vectorizer.get_feature_names()))  

        out = my_concat(tfidf_pca, ratings_s, ids_s)
        out.to_csv(f'data/per-amrev/processed/amrev_pca_train_{dataset}_i={i}_n={n_train}.csv')
        
        # Get testing info and models
        test_info = pd.concat([pd.Series(ratings_s_test), pd.Series(ids_s_test)], axis = 1, join = "inner")
        test_info.columns = ["rating", "review_id"]
        test_info.to_csv(f'data/per-amrev/processed/amrev_rating-info_test_{dataset}_i={i}_n={n_train}.csv')

        tfidf_test = vectorizer.transform(sentences_test)
        scipy.sparse.save_npz(f'data/per-amrev/processed/amrev_tfidf_test_{dataset}_i={i}_n={n_train}.npz', tfidf_test)
        with open(f'data/per-amrev/processed/amrev_pca-model_{dataset}_i={i}_n={n_train}.pkl', 'wb') as fopen:
            pickle.dump(pca, fopen)
        


# %% [markdown]
# ## Resources
# 
# Data download: http://sifaka.cs.uiuc.edu/~wang296/Data/
# 
# Helpful code for NLP in python: 
# - https://betterprogramming.pub/a-friendly-guide-to-nlp-tf-idf-with-python-example-5fcb26286a33
# - https://scikit-learn.org/stable/modules/generated/sklearn.feature_extraction.text.TfidfVectorizer.html#sklearn.feature_extraction.text.TfidfVectorizer
# - https://scikit-learn.org/stable/modules/feature_extraction.html#text-feature-extraction


