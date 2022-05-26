##----------------------------------------------------------------------------#
#' Simulation wr-era-7.0.0
#'   Step 1 - Grid-search cross-validation for optimal parameters
#' 
#' See simulation-spreadsheet.xlsx for details 
##----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(microbenchmark)
library(moments)
# devtools::install_github("skent259/mildsvm", ref = "dev-version") 
library(mildsvm) # run on 0.3.1.9011
source(here("sim/utils.R"))
source(here("sim/model-parameters.R"))
# source(here("analysis/utils.R"))

name <- "wr-era"

## Command line arguments -----------------------------------------------------#
#' @argument `sim` the simulation number
#' @argument `i` the process number when using distributed computing
#' @argument `batch_size` the number of models to run in this iteration
#' @argument `output_dir` the directory to save output to
#' @argument `data_dir` the directory where data lives
args = commandArgs(trailingOnly = TRUE)

sim <- args[1] %>% set_default("7.0.0")
i <- as.integer(args[2]) + 1
i <- i %>% set_default(1)
batch_size <- as.integer(args[3])
batch_size <- batch_size %>% set_default(200)
output_dir <- args[4] %>% set_default(glue("output/{name}"))
data_dir <- args[5] %>% set_default(glue("data/{name}/processed"))
# 2475 runs at `batch_size` = 200, for 495,000 total

print(list(sim = sim, i = i, batch_size = batch_size, output_dir = output_dir, data_dir = data_dir))

## Output file ----------------------------------------------------------------#
step <- "1"
output_fname <- glue("sim-{name}-{sim}-{step}-results_i={str_pad(i, 4, pad = 0)}.rds")
output_fname <- here(output_dir, output_fname)
gs_fname <- here(output_dir, glue("gridsearch-spec_{name}.rds"))

if (!dir.exists(here(output_dir))) {
  dir.create(here(output_dir), recursive = TRUE)
}

## Parameters for simulations -------------------------------------------------#

data_param <- list(
  bag_label = "y",
  bag_name = "b",
  inst_label = "out1"
)
n_cols <- 4

model_param <- get_model_param(n_cols, sim = "X.0.0")

data_names <- list.files(here(data_dir), full.names = TRUE)
cv_param <- list(
  nrep = 1,
  nfolds = 10,
  nfolds_gs = 5,
  name = data_names
)

## Set up grid-search specification -------------------------------------------#
set.seed(8)
if (file.exists(gs_fname)) {
  gridsearch_spec <- readRDS(gs_fname)
} else {
  gridsearch_spec <- define_gridsearch_specs(
    y, 
    bags, 
    cv_param, 
    method = "read df k-fold",
    data_param = data_param
  )
  gridsearch_spec <- shuffle_rows(gridsearch_spec)
  saveRDS(gridsearch_spec, gs_fname)
}

gs_spec_this_batch <- 
  gridsearch_spec %>% 
  select(rep, name, fold, gs_fold) %>% 
  expand_grid(model_param) %>% 
  slice(batch_index(i, batch_size)) %>% 
  left_join(gridsearch_spec, by = c("rep", "name", "fold", "gs_fold"))
# gs_spec_this_batch <- gridsearch_spec %>%
#   select(rep, name, fold, gs_fold) %>%
#   expand_grid(model_param) %>%
#   group_by(fun, method) %>% slice_head(n = 1) %>%
#   left_join(gridsearch_spec, by = c("rep", "name", "fold", "gs_fold")) # one of each method

## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- gs_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model(.x, 
                          train = .x$train,
                          test = .x$val,
                          data_param = data_param)) %>%
  bind_cols(gs_spec_this_batch)

## Save output ----------------------------------------------------------------#
print(out)
out <- out %>% select(-test, -train, -val)
saveRDS(out, output_fname)

