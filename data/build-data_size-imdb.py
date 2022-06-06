# %%
import os 
import contractions
import re
import regex 
import uuid
import scipy
import pickle 
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
# The IMDB data is already split into 25,000 training and 25,000 test reviews.  These sets have been carefully crafted to avoid information leakage in the machine learning model, so we would like to preserve this structure.  Thus, the procedure we follow is: 
# 
# 
# 1. Select `n_train` training bags from {30, 60, 90, ..., 300}
# 2. Randomly select `n_train` reviews from the 25,000 in the training set
# 3. Apply the Steps from Xiao et al. (above) to this training subset 
# 4. Construct a test set by applying the fitted TFIDF and PCA functions to the 25,000 test reviews
# 5. Save the train and test sets for use in model fitting
# 
# We repeat these steps for every value of `n_train` and for 20 replications each.
# 
# There are a total of 10 * 20 = 200 combinations of train/test data.  This may cause a memory problem because the test set will be large.  Try it on one and see what the output sizes are.  

# %%
def pull_rating(file_name):
    reg = r"([0-9]+)_([0-9]+).txt"
    subst = "\\g<2>"
    return re.sub(reg, subst, file_name, 0)

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
f = open('data/size-imdb/smart_stopwords.txt',)
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

# Pull in all review information from .txt files 
def load_reviews(data_dir, subdirs): 
    reviews = []
    ratings = []
    ids = []
    for subdir in subdirs:
        for fname in os.listdir(data_dir + subdir):
            # print(fname)
            r = pull_rating(fname)
            ratings.append(r)
            ids.append(str(uuid.uuid4()))

            with open(data_dir + subdir + fname) as f:
                data = f.read()
                reviews.append(data)
    
    return reviews, ratings, ids 

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
data_dir = 'data/size-imdb/raw/aclImdb/'
reviews_train, ratings_train, ids_train = load_reviews(data_dir, ['train/pos/', 'train/neg/'])
reviews_test, ratings_test, ids_test = load_reviews(data_dir, ['test/pos/', 'test/neg/'])

# %%
proc_dir = 'data/size-imdb/processed/'
sentences_test, ratings_s_test, ids_s_test = tokenize_sentences(reviews_test, ratings_test, ids_test)

out = pd.concat([pd.Series(ratings_s_test), pd.Series(ids_s_test)], axis = 1, join = "inner")
out.columns = ["rating", "review_id"]
out.to_csv(proc_dir + f'imdb_rating-info_test.csv')

# %%
n_rep = 10
n_train_list = [30] + [150 * i for i in range(1, 9)]
rng = np.random.default_rng(8)


for n_train in n_train_list: 
    for i in range(n_rep):
        print(n_train, i)

        # Set up TFIDF and PCA models
        vectorizer = CustomVectorizer(stop_words=stop_words_stem, preprocessor=preprocess, min_df=1)
        pca = PCA(n_components=200)

        # Randomly pull reviews for training
        random_reviews = randomly_select(n_train, rng, reviews_train, ratings_train, ids_train)
        sentences, ratings_s, ids_s = tokenize_sentences(*random_reviews)

        # Fit TFIDF and PCA models to training data 
        tfidf = vectorizer.fit_transform(sentences)
        tfidf_pca = pca.fit_transform(tfidf.toarray())  
        # print(pd.DataFrame(tfidf.toarray(),columns=vectorizer.get_feature_names()))  

        out = my_concat(tfidf_pca, ratings_s, ids_s)
        out.to_csv(proc_dir + f'imdb_pca_train_i={i}_n={n_train}.csv')
        
        tfidf_test = vectorizer.transform(sentences_test)
        scipy.sparse.save_npz(proc_dir + f'imdb_tfidf_test_i={i}_n={n_train}.npz', tfidf_test)
        with open(proc_dir + f'imdb_pca-model_i={i}_n={n_train}.pkl', 'wb') as fopen:
            pickle.dump(pca, fopen)
        


# %% [markdown]
# ## Resources
# 
# Data download: https://ai.stanford.edu/~amaas/data/sentiment/
# 
# Helpful code for NLP in python: 
# - https://betterprogramming.pub/a-friendly-guide-to-nlp-tf-idf-with-python-example-5fcb26286a33
# - https://scikit-learn.org/stable/modules/generated/sklearn.feature_extraction.text.TfidfVectorizer.html#sklearn.feature_extraction.text.TfidfVectorizer
# - https://scikit-learn.org/stable/modules/feature_extraction.html#text-feature-extraction


