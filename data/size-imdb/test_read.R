library(reticulate)
use_condaenv("r-reticulate")
np <- import("numpy")
scipy <- import("scipy")



# Load in the saved tfidf features and pca model
data_dir <- "simulations/ordinal-imdb/data/processed"

fname <- file.path(data_dir, "imdb_tfidf_test_i=0_n=300.npz")
tfidf_test <- scipy$sparse$load_npz(fname)

fname <- file.path(data_dir, "imdb_pca-model_i=0_n=300.pkl")
pca <- py_load_object(fname, pickle = "pickle")

pca_features <- pca$transform(as.matrix(tfidf_test))

