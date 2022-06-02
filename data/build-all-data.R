library(here)
library(readr)
library(foreign)

data_dir <- "data"

.warn_file_download <- function(..., name) {
  if (sum(...) != 0) {
    msg <- paste0("Error in downloading files for '", name, "' simulation.")
    warning(msg)
  }
}

## Set up folder structure ----------------------------------------------------#

level_1 <- c("per-amrev", "per-tma", "size-imdb", "size-swd", "size-wq", "wr-car", "wr-era")
level_2 <- c("processed", "raw")

for (x in level_1) {
  dir.create(here(data_dir, x))
  for (y in level_2) {
    dir.create(here(data_dir, x, y))
  }
}

## per-amerev 1.0 ----------------------------------------------------------------#

# Dataset link that I used: http://sifaka.cs.uiuc.edu/~wang296/Data/
# NOTE: seems to be loading slow, unsure why

# TODO: download data to raw folder
# TODO: add information about conda environment needed (used 3.8.8 base)
# system('python data/per-amrev/process-data.py')


## per-tma 2.0 ----------------------------------------------------------------#

# For now, just pull in "tma_stage_imputations_1.0.csv" from old tma simulations
# For later, should run scripts 

## size-imdb 3.0 --------------------------------------------------------------#

raw_dir <- "data/size-imdb/raw"

f1 <- download.file(
  "https://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz", 
  here(raw_dir, "aclImdb_v1.tar.gz"), 
  method = "auto"
)

.warn_file_download(f1, f2, name = "wr-car 6.0")

try({
  system(glue("tar -xzf {raw_dir}/aclImdb_v1.tar.gz"))
  # check directory structure
  system("python data/build-data_size-imdb.py")
})

## wr-car 6.0 -----------------------------------------------------------------#

raw_dir <- "data/wr-car/raw"

f1 <- download.file(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.names", 
  here(raw_dir, "car.names"), 
  method = "auto"
)
f2 <- download.file(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
  here(raw_dir, "car.data"), 
  method = "auto"
)

.warn_file_download(f1, f2, name = "wr-car 6.0")
try({
  cols <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")
  car <- readr::read_csv(here(raw_dir, "car.data"), col_names = cols)
  write_csv(car, here(raw_dir, "car.csv"))
  source(here(data_dir, "build-data_wr-car.R"))  
})
  

## wr-era 7.0 -----------------------------------------------------------------#

raw_dir <- "data/wr-era/raw"

f1 <- download.file(
  "https://www.openml.org/data/download/53564/ERA.arff", 
  here(raw_dir, "era.arff"), 
  method = "auto"
)

.warn_file_download(f1, name = "wr-era 7.0")
try({
  here(raw_dir, "era.arff") %>% 
    foreign::read.arff() %>% 
    write_csv(here(raw_dir, "era.csv"))
  source(here(data_dir, "build-data_wr-era.R"))
})

