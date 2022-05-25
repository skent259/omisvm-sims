###----------------------------------------------------------------------------#
#' build-data_wr-era.R
#'  
#' Take the standard ordinal datasets and generate 20 versions of a MIL dataset
#' for each at a given value of the witness rate
#' 
#' This generation assumes that the witness rates are constant across each
#' potential label except the lowest one.  
###----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
source(here("data/utils.R"))

## Directory and data information ---------------------------------------------#

name <- "wr-era"
sim <- "7.0"
raw_file <- "ERA.csv"
y_inst <- "out1"

out_dir <- glue("data/{name}/processed")
raw_dir <- glue("data/{name}/raw")
raw <- read_csv(here(raw_dir, raw_file)) 

if (!dir.exists(here(out_dir))) {
  dir.create(here(out_dir), recursive = TRUE)
}

## Data modification  ---------------------------------------------------------#

proc <- raw %>% 
  mutate(across(starts_with("in"), ~ scale(.x)[,1]))

## Create MI data -------------------------------------------------------------#

set.seed(8)
make_mil_or_data_sets_from_wr(
  proc, 
  y_inst = y_inst,
  n_versions = 20,
  witness_rates = c(0.2, 0.4, 0.6, 0.8, 1.0), 
  n_bag_each = 3,
  n_instances = 5,
  sim_name = glue("{name}-{sim}"),
  out_dir = out_dir
)

rm(raw, proc)
