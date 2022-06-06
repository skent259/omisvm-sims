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

name <- "size-swd"
sim <- "4.0"
raw_file <- "swd.csv"
y_inst <- "Out1"

out_dir <- glue("data/{name}/processed")
raw_dir <- glue("data/{name}/raw")
raw <- read_csv(here(raw_dir, raw_file)) 

if (!dir.exists(here(out_dir))) {
  dir.create(here(out_dir), recursive = TRUE)
}

## Data modification  ---------------------------------------------------------#

proc <- raw %>% 
  mutate(Out1 = as.ordered(Out1))

## Create MI data -------------------------------------------------------------#

set.seed(8)
make_mil_or_data_sets_from_size(
  proc, 
  y_inst = y_inst,
  n_versions = 20,
  n_bags = seq(30, 240, by = 30),
  n_instances = c(3, 5),
  n_seed_combine = 4, 
  sim_name = glue("{name}-{sim}"),
  out_dir = out_dir
)


rm(raw, proc)
