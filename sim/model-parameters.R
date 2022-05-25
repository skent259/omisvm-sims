##-----------------------------------------------------------------------------#
#' Model parameters
#' 
#' Define parameters that are used in each simulation.  Want to avoid copying
#' them in every file. 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(mildsvm)

#' Model parameters
#'
#' Used for all simulations
#'
#' @param n_cols The number of columns in a data frame, used to define the
#'   `sigma` for radial kernel
#' @return A tibble with columns `fun_name`, `fun`, `cost`, `h`, `method`,
#'   `control`, and `cost_eta` representing functions and arguments to use 
get_model_param <- function(n_cols, sim = "X.0.0") {
  if (sim == "X.0.0") {
    .cost <- c(0.1, 10, 1000)
    .h <- c(0.1, 10, 1000)
    .cost_eta <- c(0.1, 10, 1000)
    .sigma <- (1/n_cols) * 2 ^ c(-1, 0, 1)
    
    dplyr::bind_rows(
      # OMI-SVM (proposed)
      tidyr::expand_grid(
        fun_name = "omisvm",
        fun = list(mildsvm::omisvm), 
        cost = .cost,
        h = .h, 
        method = c("qp-heuristic"), 
        control = purrr::transpose(tidyr::expand_grid(
          kernel = "radial",
          sigma = .sigma, 
          time_limit = 600
        ))
      ),
      # SI-SVOREXC
      tidyr::expand_grid(
        fun_name = "svor_exc",
        fun = list(mildsvm::svor_exc),
        cost = .cost,
        method = c("smo"), 
        control = purrr::transpose(tidyr::expand_grid(
          kernel = "radial",
          sigma = .sigma
        ))
      ),
      # MI-SVM (OVA)
      tidyr::expand_grid(
        fun_name = "misvm_orova",
        fun = list(mildsvm::misvm_orova),
        cost = .cost,
        method = c("qp-heuristic"), 
        control = purrr::transpose(tidyr::expand_grid(
          kernel = "radial",
          sigma = .sigma, 
          time_limit = 600
        ))
      ),
      # MIOR (as written, corrected)
      tidyr::expand_grid(
        fun_name = "mior",
        fun = list(mildsvm::mior), 
        cost = .cost,
        cost_eta = .cost_eta, 
        method = c("qp-heuristic"), 
        control = purrr::transpose(tidyr::expand_grid(
          kernel = "radial",
          sigma = .sigma, 
          time_limit = 600,
          option = c("corrected", "xiao")
        ))
      ),
    )
  }
}
