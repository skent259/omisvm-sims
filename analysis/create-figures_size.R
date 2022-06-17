##-----------------------------------------------------------------------------#
#' Generate figures for size sims:
#'   size-imdb 3.0.0
#'   size-swd 4.0.0
#'   size-wq 5.0.0
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

## size-imdb 3.0.0 ------------------------------------------------------------#
# TODO: 

## size-swd 4.0.0 -------------------------------------------------------------#
results <- read_results(name = "size-swd", sim = "4.0.0", step = 2)
results_mod <- process_data_size(results)
p4 <- imap(metrics, ~plot_data_size(results_mod, .x))


## size-wq 5.0.0 --------------------------------------------------------------#
results <- read_results(name = "size-wq", sim = "5.0.0", step = 2)
results_mod <- process_data_size(results)
p5 <- imap(metrics, ~plot_data_size(results_mod, .x))


## Combine for single plot ----------------------------------------------------#
p_size <- (((p5$mzoe + ggtitle("A. Wine-quality data set")) / p5$mae) | 
          ((p4$mzoe  + ggtitle("B. SWD data set")) / p4$mae)) +
  plot_layout(guides = "collect") & 
  scale_x_continuous(breaks = seq(30, 300, by = 60)) &
  theme(legend.position = "bottom") 

print(p_size)
ggsave(here(fig_dir, "size_swd-wq_both-vs-methods.pdf"), p_size, width = 8, height = 6)


