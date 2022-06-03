library(glue)
library(here)
library(dplyr)

#' Combine RDS files
#' @param dir Directory that files are located in.
#' @param pattern File pattern to look for.
combine_files <- function(dir, pattern) {
    files <- list.files(path = dir, pattern = pattern, full.names = TRUE)
    res <- lapply(files, FUN = readRDS)
    res <- dplyr::bind_rows(res)
    return(res)
}

#' Set defaults for interactive session 
set_default <- function(.x, val) { 
  if(is.na(.x)) val else .x 
}

## Command line arguments -----------------------------------------------------#
args = commandArgs(trailingOnly = TRUE)
print(args)

result_dir <- args[1] %>% set_default("output/6.0")
sim <- args[2] %>% set_default("wr-car-6.0.0")
step <- args[3] %>% set_default(1)
output_dir <- args[4] %>% set_default(result_dir)

## Combine files and save -----------------------------------------------------#
sim_files_pattern <- glue("sim-{sim}-{step}-results_")
outfile_name <- glue("omisvm-sims-results-{sim}-{step}.rds")
outfile_name <- here(output_dir, outfile_name)

out <- combine_files(result_dir, sim_files_pattern)
saveRDS(out, outfile_name)
