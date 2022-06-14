##----------------------------------------------------------------------------#
#' Simulation size-wq-5.0.0
#'   Step 2 - Test set evaluation on optimal parameters
#' 
#' See simulation-spreadsheet.xlsx for details 
##----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(microbenchmark)
library(moments)
# devtools::install_github("skent259/mildsvm", ref = "dev-version") 
library(mildsvm) # run on 0.3.1.9013
source(here("sim/utils.R"))
source(here("sim/model-parameters.R"))

name <- "size-wq"

## Command line arguments -----------------------------------------------------#
#' @argument `sim` the simulation number
#' @argument `i` the process number when using distributed computing
#' @argument `batch_size` the number of models to run in this iteration
#' @argument `output_dir` the directory to save output to
#' @argument `data_dir` the directory where data lives
#' @argument `metric` the metric to optimize over 
args <- commandArgs(trailingOnly = TRUE)

sim <- args[1] %>% set_default("5.0.0")
i <- as.integer(args[2]) + 1
i <- i %>% set_default(1)
batch_size <- as.integer(args[3])
batch_size <- batch_size %>% set_default(2)
output_dir <- args[4] %>% set_default(glue("output/{name}"))
data_dir <- args[5] %>% set_default(glue("data/{name}/processed"))
metric <- args[6] %>% set_default("mae")
# 1000 runs at `batch_size` = 2, for 2000 total

print(list(sim = sim, i = i, batch_size = batch_size, 
           output_dir = output_dir, data_dir = data_dir, metric = metric))

## Output file ----------------------------------------------------------------#
step <- "2"
output_fname <- glue("sim-{name}-{sim}-{step}-results_i={str_pad(i, 4, pad = 0)}-{metric}.rds")
output_fname <- here(output_dir, output_fname)
step1_fname <- glue("omisvm-sims-results-{name}-{sim}-1.rds")
step1_fname <- here(output_dir, step1_fname)
gs_fname <- here(output_dir, glue("gridsearch-spec_{name}.rds"))

if (!dir.exists(here(output_dir))) {
  dir.create(here(output_dir), recursive = TRUE)
}

## Parameters for simulations -------------------------------------------------#

data_param <- list(
  bag_label = "y",
  bag_name = "b",
  inst_label = "quality"
)
n_cols <- 11

model_param <- get_model_param(n_cols, sim = "X.0.0")

data_names <- list.files(here(data_dir), full.names = TRUE)
train_data_names <- data_names[str_detect(data_names, "train")]
test_data_names <- str_replace(train_data_names, "train", "test")

cv_param <- list(
  nrep = 1,
  nfolds = 1,
  nfolds_gs = 5,
  train = train_data_names,
  test = test_data_names
)

## Find optimal parameters from step 1 ----------------------------------------#

gridsearch_spec <- readRDS(step1_fname) %>%
  mutate(across(c(train_name, test_name), ~basename(.x)))
model_vars <- c("train_name", "test_name", "fun_name", "method", "option")
gs_vars <- c("cost", "h", "cost_eta", "sigma")

gridsearch_spec <- 
  gridsearch_spec %>% 
  hoist(control, "sigma", "option", .remove = FALSE) %>% 
  group_by(across(all_of(c(model_vars, gs_vars)))) %>% 
  mutate(mean_metric = mean(.data[[metric]], na.rm = TRUE)) %>% 
  ungroup()

## Set up test-set evaluation specification -----------------------------------#

eval_spec <- 
  gridsearch_spec %>% 
  group_by(across(all_of(model_vars))) %>% 
  mutate(across(time, list(sum = ~sum(.x, na.rm = TRUE)))) %>% 
  slice_min(order_by = mean_metric, n = 1, with_ties = FALSE) %>% 
  ungroup()

eval_spec <- 
  eval_spec %>% 
  select(-mzoe, -mae, -time, -sigma, -option) %>%
  mutate(across(c(train_name, test_name), ~here(data_dir, .x)))
eval_spec_this_batch <- slice(eval_spec, batch_index(i, batch_size))
# eval_spec_this_batch <- eval_spec %>% group_by(fun, method) %>% slice_head(n = 1) 

print(eval_spec)
print(eval_spec_this_batch)

## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- eval_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model(.x, 
                          train = TRUE,
                          test = TRUE,
                          data_param = data_param,
                          train_name = .x$train_name,
                          test_name = .x$test_name,
                          col_select = 1:14)) %>% 
  bind_cols(eval_spec_this_batch)

## Save output ----------------------------------------------------------------#
print(out)
out$metric <- metric
saveRDS(out, output_fname)

