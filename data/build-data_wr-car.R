###----------------------------------------------------------------------------#
#' build-data_wr-car.R
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

name <- "wr-car"
sim <- "6.0"
raw_file <- "php2jDIhh.csv"
y_inst <- "class"

out_dir <- glue("data/{name}/processed")
raw_dir <- glue("data/{name}/raw")
raw <- read_csv(here(raw_dir, raw_file)) 

if (!dir.exists(here(out_dir))) {
  dir.create(here(out_dir), recursive = TRUE)
}

## Data modification  ---------------------------------------------------------#

proc <- raw %>% 
  mutate(class = ordered(class, c("unacc", "acc", "good", "vgood"), 1:4)) 

proc <- bind_cols(
  proc[, "class"], 
  as.data.frame(model.matrix(class ~ 0 + ., data = proc))
)

## Create MI data -------------------------------------------------------------#

set.seed(8)
make_mil_or_data_sets_from_wr(
  proc, 
  y_inst = y_inst,
  n_versions = 20,
  witness_rates = c(0.2, 0.4, 0.6, 0.8, 1.0), 
  n_bag_each = 13,
  n_instances = 5,
  sim_name = glue("{name}-{sim}"),
  out_dir = out_dir
)

rm(raw, proc)
