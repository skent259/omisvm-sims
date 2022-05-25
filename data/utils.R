#' Safer version of sample
.resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Generate semi-synthetic MIL data from witness rate
#'
#' @param data A data.frame to generate from
#' @param y_inst A character that indicates which column of `data` has instance labels
#' @param n_bag_each The number of bags in each level of `y_inst`
#' @param n_instances The number of instances in each bag
#' @param wr The fraction of instances where the instance label should match the
#'   bag label (except for the lowest label level).
#'   
#' @return A data.frame
generate_mil_or_data_from_wr <- function(data, y_inst, wr, n_bag_each, n_instances) {
  y_inst <- data[[y_inst]]
  n_lev <- length(unique(y_inst))
  y_inst_i <- y_inst
  all_rows <- all_rows_i <- seq_len(nrow(data))
  
  n_rep_inst <- round(n_bag_each * wr * n_instances)
  n_nonrep_inst <- round(n_bag_each * (1-wr) * n_instances)
  
  rows <- c()
  bags <- c()
  max_bag <- 0
  
  for (i in n_lev:1) {
    if (i > 1) {
      rep_inst <- .resample(all_rows_i[y_inst_i == i], size = n_rep_inst)
      nonrep_inst <- .resample(all_rows_i[y_inst_i < i], size = n_nonrep_inst)
      
      rows_i <- c(rep_inst, nonrep_inst)
      bags_i <- c(rep(max_bag + 1:n_bag_each, each = round(n_instances * wr)),
                  rep(max_bag + 1:n_bag_each, each = round(n_instances * (1-wr))))
    } else {
      rows_i <- .resample(all_rows_i[y_inst_i == i], size = round(n_rep_inst + n_nonrep_inst))
      bags_i <- rep(max_bag + 1:n_bag_each, each = n_instances)
    }
    
    rows <- c(rows, rows_i)
    bags <- c(bags, bags_i)
    max_bag <- max(bags)
    
    all_rows_i <- all_rows[-rows]
    y_inst_i <- y_inst[-rows]
  }
  
  dplyr::bind_cols(
    y = rep(n_lev:1, each = n_bag_each * n_instances),
    b = bags,
    data[rows, ]
  ) 
}

#' Make a series of MIL data sets
#' 
#' @param df A data frame to pass to `generate_mil_or_data()`. 
#' @param n_versions An integer for the number of versions to create
#' @param witness_rates A vector of witness rates to use
#' @param sim_name The simulation name, used in file name
#' @param out_dir The directory to save to
#' @param ... Arguments passed to `generate_mil_or_data()`. 
#' @param verbose A logical, whether to print information
#' @return Nothing, data will be saved to out_dir
make_mil_or_data_sets_from_wr <- function(df, n_versions, witness_rates, sim_name, out_dir, ..., verbose = TRUE) {
  seeds <- ceiling(runif(n_versions, 0, 2^30))
  
  if (verbose) cat("Starting to process CAR data set... ")
  for (wr in witness_rates) {
    for (j in 1:n_versions) {
      tryCatch({
        if (verbose) cat("wr: ", wr, "; i: ", j, "\n")
        set.seed(seeds[j])
        
        out <- generate_mil_or_data_from_wr(df, wr = wr, ...)
        out_file <- glue::glue("{sim_name}_wr={wr}_i={j}.csv")
        readr::write_csv(out, here::here(out_dir, out_file))
        
      }, error = function(e) print(e))
    }
  }
  if (verbose) cat("Done")
}

