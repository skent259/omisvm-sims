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

metrics <- c("mae", "mzoe") %>% set_names()

## per-amrev 1.0.0 ------------------------------------------------------------#
results <- read_results(name = "per-amrev", sim = "1.0.0", step = 2)
results_mod <- process_data_amrev(results)
p1 <- imap(metrics, ~plot_data_amrev(results_mod, .x))

p_amrev <- p1$mzoe / p1$mae
print(p_amrev)
ggsave(here(fig_dir, "per-amrev_both-vs-methods.pdf"), p_amrev, width = 8, height = 5.5, dpi = 800)


## per-tma 2.0.0 --------------------------------------------------------------#
results <- read_results(name = "per-tma", sim = "2.0.0", step = 2)
results_mod <- process_data_tma(results)
p2 <- imap(metrics, ~plot_data_tma(results_mod, .x))

p_tma <- p2$mzoe / p2$mae 
print(p_tma)

saveRDS(results_mod, here(res_dir, "per-tma_results-mod.rds"))
ggsave(here(fig_dir, "per-tma_both-vs-methods.pdf"), p_tma, width = 8, height = 3, dpi = 800)


