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

## per-tma 2.0 ----------------------------------------------------------------#

# For now, just pull in "tma_stage_imputations_1.0.csv" from old tma simulations
# For later, should run scripts 

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

