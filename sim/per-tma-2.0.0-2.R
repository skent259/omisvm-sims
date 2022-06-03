##----------------------------------------------------------------------------#
#' Simulation per-tma-2.0.0
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
library(mildsvm) # run on 0.3.1.9011
source(here("sim/utils.R"))
source(here("sim/model-parameters.R"))

name <- "per-tma"

## Command line arguments -----------------------------------------------------#
#' @argument `sim` the simulation number
#' @argument `i` the process number when using distributed computing
#' @argument `batch_size` the number of models to run in this iteration
#' @argument `output_dir` the directory to save output to
#' @argument `data_dir` the directory where data lives
#' @argument `metric` the metric to optimize over 
args = commandArgs(trailingOnly = TRUE)

sim <- args[1] %>% set_default("2.0.0")
i <- as.integer(args[2]) + 1
i <- i %>% set_default(1)
batch_size <- as.integer(args[3])
batch_size <- batch_size %>% set_default(2)
output_dir <- args[4] %>% set_default(glue("output/{name}"))
data_dir <- args[5] %>% set_default(glue("data/{name}/processed"))
metric <- args[6] %>% set_default("mae")
# 250 runs at `batch_size` = 2, for 500 total

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

df <- read_csv(here(data_dir, "tma_stage_imputations_1.0.csv")) %>% 
  mutate(across(grade_differentiation, as.ordered)) %>% 
  janitor::clean_names()

data_param <- list(
  bag_label = "grade_differentiation",
  bag_name = "case_number"
)
n_cols <- ncol(df) - 2

model_param <- get_model_param(n_cols, sim = "X.0.0")

cv_param <- list(
  nrep = 10,
  nfolds = 10,
  nfolds_gs = 5
)

## Find optimal parameters from step 1 ----------------------------------------#

gridsearch_spec <- readRDS(step1_fname)
model_vars <- c("rep", "fold", "fun_name", "method", "option")
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

gs_spec_w_train_cols <- readRDS(gs_fname)

eval_spec <- 
  eval_spec %>% 
  select(-mzoe, -mae, -time, -sigma, -option) %>%
  left_join(gs_spec_w_train_cols, by = c("rep", "fold", "gs_fold"))
eval_spec_this_batch <- slice(eval_spec, batch_index(i, batch_size))
# eval_spec_this_batch <- eval_spec %>% group_by(fun, method) %>% slice_head(n = 1) 

print(eval_spec)
print(eval_spec_this_batch)

## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- eval_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model(.x, 
                          train = c(.x$train, .x$val),
                          test = .x$test, 
                          df = df, 
                          data_param = data_param)) %>%
  bind_cols(eval_spec_this_batch)

## Save output ----------------------------------------------------------------#
print(out)
out <- out %>% select(-test, -train, -val)
out$metric <- metric
saveRDS(out, output_fname)

