##-----------------------------------------------------------------------------#
#' Generate figures for witness rate sims:
#'   wr-car 6.0.0
#'   wr-era 7.0.0
#' 
#' Make all figures that are useful for analysis 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(scales)
library(ggbeeswarm)
library(mildsvm)
library(knitr)
library(kableExtra)
library(patchwork)
source(here("analysis/utils.R"))

res_dir <- "output"
fig_dir <- "fig"

##-----------------------------------------------------------------------------#
## per-tma 2.0.0 ---------------------------------------------------------------
##-----------------------------------------------------------------------------#

metrics <- c("mae", "mzoe") %>% set_names()

## per-tma 2.0.0 --------------------------------------------------------------#
results <- read_results(name = "per-tma", sim = "2.0.0", step = 2)
results_mod <- process_data_tma(results)
p2 <- imap(metrics, ~plot_data_tma(results_mod, .x))


p2.1 <- p2$mzoe / p2$mae 
print(p2.1)

ggsave(here(fig_dir, "per-tma_both-vs-methods.pdf"), p2.1, width = 8, height = 3)


