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

## wr-car 6.0.0 ---------------------------------------------------------------#
results <- read_results(name = "wr-car", sim = "6.0.0", step = 2)
results_mod <- process_data_wr(results)
p6 <- imap(metrics, ~plot_data_wr(results_mod, .x))
# walk(p6, print)

## wr-era 7.0.0 ---------------------------------------------------------------#
results <- read_results(name = "wr-era", sim = "7.0.0", step = 2)
results_mod <- process_data_wr(results)
p7 <- imap(metrics, ~plot_data_wr(results_mod, .x))
# walk(p7, print)

## Combine for single plot ----------------------------------------------------#
p_wr <- (((p6$mzoe + ggtitle("A. CAR data set")) / p6$mae) | 
          ((p7$mzoe  + ggtitle("B. ERA data set")) / p7$mae)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

print(p_wr)
ggsave(here(fig_dir, "wr-car-era_both-vs-methods.pdf"), p_wr, width = 8, height = 6, dpi = 800)


