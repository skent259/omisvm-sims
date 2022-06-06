###----------------------------------------------------------------------------#
#' build-data_size-wq.R
#'  
#' Take the standard ordinal datasets and generate 20 versions of a MIL dataset
#' for each at a given value of the training size
###----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
source(here("data/utils.R"))

## Directory and data information ---------------------------------------------#

name <- "size-wq"
sim <- "5.0"
raw_file <- "winequality-red.csv"
y_inst <- "quality"

out_dir <- glue("data/{name}/processed")
raw_dir <- glue("data/{name}/raw")
raw <- read_delim(here(raw_dir, raw_file), delim = ";") 

if (!dir.exists(here(out_dir))) {
  dir.create(here(out_dir), recursive = TRUE)
}

## Data modification  ---------------------------------------------------------#

proc <- raw %>% 
  mutate(
    quality = as.ordered(quality),
    across(where(is.numeric), ~ scale(.x)[,1])
  ) %>% 
  janitor::clean_names()

## Create MI data -------------------------------------------------------------#

set.seed(8)
make_mil_or_data_sets_from_size(
  proc, 
  y_inst = y_inst,
  n_versions = 20,
  n_bags = seq(30, 300, by = 30),
  n_instances = c(3, 5),
  n_seed_combine = 4, 
  sim_name = glue("{name}-{sim}"),
  out_dir = out_dir
)


rm(raw, proc)
