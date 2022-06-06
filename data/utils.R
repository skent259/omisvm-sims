##----------------------------------------------------------------------------#
#' Data utility functions for omisvm-sims
##----------------------------------------------------------------------------#

#' Safer version of sample
.resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Generate semi-synthetic MIL data from size
#'
#' First, randomly combine data into bags according to `n_instances`, allowing
#' for extra instances to fit into a last bag.  Second, split the data into
#' training and testing according to `n_bag_train`, leaving the rest for testing
#'
#' @param data A data.frame to generate from
#' @param y_inst A character that indicates which column of `data` has instance labels
#' @param n_bag_train The number of bags in total for training data
#' @param n_instances The number of instances in each bag
#' @param seed_combine A integer number seed for grouping instances to bags
#' @param seed_split A integer number seed for splitting data into train/test
#'   
#' @return A list with two data.frames: 'train' and 'test
generate_mil_or_data_from_size <- function(
    data, 
    y_inst, 
    n_bag_train, 
    n_instances,
    seed_combine = NULL, 
    seed_split = NULL) {
  # Generate bags from the data
  if (!is.null(seed_combine)) {
    set.seed(seed_combine)
  }
  n_bag_total <- ceiling(nrow(data) / n_instances)
  if (n_bag_total < n_bag_train) {
    stop(glue::glue(
      "The number of training bags {n_bag_train} must be larger than ",
      "the number of possible bags {n_bag_total}."
    ))
  }
  all_rows <- .resample(1:nrow(data))
  
  df <- data[all_rows, ]
  col_ind <- which(colnames(df) == y_inst)
  y_inst <- df[[y_inst]]
  # y_inst <- y_inst[all_rows]
  bag_id <- rep(1:n_bag_total, each = n_instances)[1:length(all_rows)]
  y <- mildsvm::classify_bags(y_inst, bag_id, condense = FALSE)
  
  # Split data into train and testing 
  if (!is.null(seed_split)) {
    set.seed(seed_split)
    if (is.null(seed_combine)) {
      warning(glue::glue(
        "If you specify `seed_combine`, you should also specify `seed_split`, ", 
        "otherwise the seed split will be the same every time."
      ))
    }
  }
  bag_id_train <- .resample(unique(bag_id), n_bag_train)
  bag_id_test <- setdiff(unique(bag_id), bag_id_train)
  
  
  df <- bind_cols(
    y = y,
    b = bag_id,
    df[, col_ind],
    df[, -col_ind]
  ) 
  
  return(list(
    "train" = df[which(df$b %in% bag_id_train), ],
    "test" = df[which(df$b %in% bag_id_test), ]
  ))
}

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
generate_mil_or_data_from_wr <- function(
    data, 
    y_inst, 
    wr, 
    n_bag_each, 
    n_instances) {
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
#' Creates a train and test data set for the 
#' `n_versions * length(n_bags) * length(n_instance)` combinations in the data
#'
#' @param df A data frame to pass to `generate_mil_or_data()`.
#' @param n_versions An integer for the number of versions to create
#' @param n_bags A vector of sizes to use for number of bags
#' @param n_instances A vector of sizes to use for number of instances
#' @param n_seed_combine An integer for the number of different ways to group
#'   data into bags. Should be a multiple of `n_versions`. 
#' @param sim_name The simulation name, used in file name
#' @param out_dir The directory to save to
#' @param ... Arguments passed to `generate_mil_or_data()`.
#' @param verbose A logical, whether to print information
#' 
#' @return Nothing, data will be saved to out_dir
make_mil_or_data_sets_from_size <- function(
    df, 
    n_versions, 
    n_bags, 
    n_instances, 
    n_seed_combine, 
    sim_name,
    out_dir,
    ..., 
    verbose = TRUE) {
  
  seeds_combine <- ceiling(runif(n_seed_combine, 0, 2^30))
  seeds_split <- ceiling(runif(n_versions / n_seed_combine, 0, 2^30))
  
  if (verbose) cat("Starting to process data set... ", "\n")
  for (n_bag in n_bags) {
    Sys.sleep(1)
    for (n_inst in n_instances) {
      for (j in 1:n_versions) {
        tryCatch({
          if (verbose)  cat(glue("n_bag: {n_bag}; n_inst: {n_inst}; i: {j}"), "\n")
          ind_c <- ((j-1) %% n_seed_combine) + 1
          ind_s <- ((j-1) %/% n_seed_combine) + 1
          
          out <- generate_mil_or_data_from_size(
            df, 
            n_bag_train = n_bag, 
            n_instances = n_inst, 
            seed_combine = seeds_combine[ind_c],
            seed_split = seeds_split[ind_s],
            ...
          )
          
          out_file <- glue::glue("{sim_name}_train_nbag={n_bag}_ninst={n_inst}_i={j}.csv")
          readr::write_csv(out[["train"]], here(out_dir, out_file))
          out_file <- str_replace(out_file, "train", "test")
          readr::write_csv(out[["test"]], here(out_dir, out_file))
          
        }, error = function(e) print(e))
      }
    }
  }

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
make_mil_or_data_sets_from_wr <- function(
    df, 
    n_versions, 
    witness_rates, 
    sim_name, 
    out_dir, 
    ..., 
    verbose = TRUE) {
  seeds <- ceiling(runif(n_versions, 0, 2^30))
  
  if (verbose) cat("Starting to process data set... ")
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
