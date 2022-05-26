#' Set defaults for interactive session 
set_default <- function(.x, val) { 
  if(is.na(.x)) val else .x 
}

#' Shuffle rows of a data frame
#' @param df A data.frame object
#' @return A data.frame 
shuffle_rows <- function(df) {
  df[sample.int(nrow(df)), ]  
}

#' Get the indices of the `i`th batch of size `batch_size`
#' 
#' @param i The index number.
#' @param batch_size The size of all batches.
#' @return A vector of indices.  
batch_index <- function(i, batch_size) {
  start <- (i-1) * batch_size + 1
  end <- i * batch_size
  return(start:end)
}

#' Cross-validation for MIL data
#' 
#' For a MIL data set, we want to ensure that any cross validation is done at
#' the bag level.  This function passes the MIL data set info to
#' `rsample::vfold_cv()` in a way that ensures this constraint holds.
#' Stratification on the label `y` is done when possible.  The folds are
#' returned at the instance level, so that there is fold information for each
#' row of the data set.
#' 
#' @param y The bag label from a MIL data set.
#' @param bags The bag ids from a MIL data set.
#' @param n_fold The number of folds for cross-validation (default 5).  
#' 
#' @return A list with cross-validation info: `n_fold`, the number of folds
#'   used; `fold_id`: a vector indicating which fold each row belongs to.
select_cv_folds <- function(y, bags, n_fold = 5) {
  info <- data.frame(y = y, bags = bags)
  info_bag_layer <- unique(info)
  
  if (min(table(info_bag_layer$y)) < n_fold) {
    # Not enough for stratification on y
    vfold <- rsample::vfold_cv(info_bag_layer, v = n_fold)
  } else {
    vfold <- rsample::vfold_cv(info_bag_layer, v = n_fold, strata = "y")
  }
  
  ids <- imap(vfold$splits, function(.x, .y) {
    out <- setdiff(1:nrow(info_bag_layer), .x$in_id)
    names(out) <- rep(.y, length(out))
    out
  })
  ids <- sort(unlist(ids))
  info_bag_layer$bag_id <- as.numeric(names(ids))
  
  tmp <- info %>%
    dplyr::left_join(info_bag_layer, by = c("bags", "y"))
  
  fold_id <- tmp$bag_id
  return(list(n_fold = n_fold, fold_id = fold_id))
}

#' Specification of grid-search parameters
#' 
#' @param y The column name for the outcome. 
#' @param bags The column name for the bag label. 
#' @param cv_param A list of parameters that define the cross-validation,
#'   including `nfolds_gs`, `train`, and `test`.  `nfolds_gs` gives the number
#'   of cross-validation folds, while `train` and `test` denote the data set
#'   number in this case.
#' @param method The method to use for grid-search (default: 'train-test').  
#' @param ... Additional parameters for this special case, including:
#' - `data_fun` the function used to generate the data set
#' - `data_param` A data.frame where each row indicates the arguments passed to
#' `data_fun`.  
#' 
#' @return A data.frame where each row provides the information needed to perform 
#' a training and evaluation for the grid-search.  This includes columns of 
#' - `train_name` the data set number, pulled from `cv_param$train`
#' - `test_name` the data set number, pulled from `cv_param$test`
#' - `gs_fold` the fold number for each data set
#' - `train` the data set rows used for training
#' - `val` the data set rows used for validation
#' - `seed` the seed number set before data set generation and CV fold
#'   selection. (using `set.seed()` before both operations). 
define_gridsearch_specs <- function(
    y, 
    bags, 
    cv_param, 
    model_param = NULL, 
    method = c("train-test", "repeated k-fold", "read df k-fold"), 
    ...) {
  method = match.arg(method, c("train-test", "repeated k-fold", "read df k-fold"))
  dots <- list(...)
  
  if (method == "train-test") {
    out <- with(cv_param, 
                expand_grid(nesting(train_name = train, test_name = test),
                            gs_fold = 1:nfolds_gs))
    
    rep_seeds <- ceiling(runif(length(cv_param$train), 0, 2^30))
    for (ds in 1:length(cv_param$train)) {
      
      set.seed(rep_seeds[ds])
      args <- transpose(dots$data_param)[[ds]]
      train_ds <- do.call(dots$data_fun, args)
      
      y_ <- train_ds[[y]]
      bags_ <- train_ds[[bags]]
      
      set.seed(rep_seeds[ds])
      order <- sample(1:length(y_))
      gs_folds <- select_cv_folds(y_[order], bags_[order], n_fold = cv_param$nfolds_gs)
      
      for (gs_fold in 1:cv_param$nfolds_gs) {
        train <- gs_folds$fold_id != gs_fold
        val <- gs_folds$fold_id == gs_fold
        
        i <- which(out$train_name == cv_param$train[ds] & 
                     out$test_name == cv_param$test[ds] &
                     out$gs_fold == gs_fold)
        out[[i, "train"]] <- list(order[train])
        out[[i, "val"]] <- list(order[val])
        out[[i, "seed"]] <- rep_seeds[ds]
      }
    }
  } else if (method == "repeated k-fold") {
    out <- with(cv_param, 
                expand_grid(rep = 1:nrep, fold = 1:nfolds, gs_fold = 1:nfolds_gs))
    
    # split the dataset into test, train, and validation for each row of `out`
    rep_seeds <- ceiling(runif(cv_param$nrep, 0, 2^30))
    for (rep in 1:cv_param$nrep) {
      set.seed(rep_seeds[rep])
      order <- sample(1:length(y))
      
      folds <- select_cv_folds(y[order], bags[order], n_fold = cv_param$nfolds)
      
      for (fold in 1:cv_param$nfolds) {
        trainval <- folds$fold_id != fold
        test <- which(folds$fold_id == fold)
        
        gs_folds <- select_cv_folds(y[order][trainval], bags[order][trainval], n_fold = cv_param$nfolds_gs)
        
        for (gs_fold in 1:cv_param$nfolds_gs) {
          train <- which(trainval)[gs_folds$fold_id != gs_fold]
          val <- which(trainval)[gs_folds$fold_id == gs_fold]
          
          i <- which(out$rep == rep & out$fold == fold & out$gs_fold == gs_fold)
          out[[i, "test"]] <- list(order[test])
          out[[i, "train"]] <- list(order[train])
          out[[i, "val"]] <- list(order[val])
        }
      }
    }
  } else if (method == "read df k-fold") {
    out <- with(cv_param, 
                expand_grid(rep = 1:nrep, name = name, fold = 1:nfolds, gs_fold = 1:nfolds_gs))
    
    # split the dataset into test, train, and validation for each row of `out`
    rep_seeds <- ceiling(runif(cv_param$nrep, 0, 2^30))
    rep_seeds2 <- ceiling(runif(length(cv_param$name), 0, 2^30))
    
    for (rep in 1:cv_param$nrep) {
      for (ds in 1:length(cv_param$name)) {
        set.seed(rep_seeds[rep] + rep_seeds2[ds])
        
        train_ds <- read_csv(cv_param$name[ds])
        y_ <- train_ds[[dots$data_param$bag_label]]
        bags_ <- train_ds[[dots$data_param$bag_name]]
        
        order <- sample(seq_along(y_))
        folds <- select_cv_folds(y_[order], bags_[order], n_fold = cv_param$nfolds)
        
        for (fold in 1:cv_param$nfolds) {
          trainval <- folds$fold_id != fold
          test <- which(folds$fold_id == fold)
          
          gs_folds <- select_cv_folds(y_[order][trainval], bags_[order][trainval], n_fold = cv_param$nfolds_gs)
          
          for (gs_fold in 1:cv_param$nfolds_gs) {
            train <- which(trainval)[gs_folds$fold_id != gs_fold]
            val <- which(trainval)[gs_folds$fold_id == gs_fold]
            
            i <- which(out$rep == rep & 
                         out$name == cv_param$name[ds] & 
                         out$fold == fold & 
                         out$gs_fold == gs_fold)
            out[[i, "test"]] <- list(order[test])
            out[[i, "train"]] <- list(order[train])
            out[[i, "val"]] <- list(order[val])
          }
        }
      }
    }
  } 
  return(out)
}

#' Fit and evaluate a given model
#'
#' This function is designed to run both steps during a cross-validation or
#' train/test procedure depending on the arguments used.  It works by passing in
#' the full data set and pulling the train/test versions from arguments. 
#'
#' @param row A row of the grid-search specification. This should include both
#'   grid-search parameters and the model parameters.
#' @param df A data.frame to train on.
#' @param train Indices of the data to train the model on.  Alternatively, can
#'   pass `TRUE` to use the entire training data.
#' @param test  Indices of the data to validate/test the model on.
#'   Alternatively, can pass `TRUE` to use the entire testing data.
#' @param data_param A list with arguments specifying the `bag_label`,
#'   `bag_name`, and `inst_label` of `df`
#' @param verbose A logical for whether to output useful information
#'   
#' @return A data.frame with performance metrics such as 
#' - `auc` The area under ROC based on the bag-level predictions
#' - `time` The time taken for fitting and prediction
#' - `mipgap` The reported gap from the MIP procedure in Gurobi, if applicable
evaluate_model <- function(row, df, train, test, data_param, verbose = TRUE) {
  if (verbose) {
    cat("Function:", row$fun_name, ", ", 
        "Method:", row$method, "\n")
  }
  
  if (missing(df)) {
    df <- read_csv(row$name) 
  }
  df <- mildsvm::as_mi_df(df, data_param$bag_label, data_param$bag_name, data_param$inst_label)
  train_df <- df[train, , drop = FALSE]
  test_df <- df[test, , drop = FALSE]
  tryCatch({
    benchmark <- microbenchmark({
      
      arg_names <- switch(
        row$fun_name,
        "omisvm" = c("cost", "h", "method", "control"),
        "svor_exc" = c("cost", "method", "control"),
        "misvm_orova" = c("cost", "method", "control"),
        "mior" = c("cost", "cost_eta", "method", "control")
      )
      args <- row[arg_names]
      args$x <- train_df
      fit <- do.call(row$fun, args)
      
      pred <- predict(fit, new_data = test_df, type = "class", layer = "bag")
      
    }, times = 1)
    
    y_true_bag <- classify_bags(test_df$bag_label, test_df$bag_name)
    y_true_bag <- as.numeric(as.character(y_true_bag))
    y_pred_bag <- classify_bags(as.ordered(pred$.pred_class), test_df$bag_name)
    y_pred_bag <- as.numeric(as.character(y_pred_bag))
    
    return(tibble(
      mzoe = mean(y_true_bag != y_pred_bag),
      mae = mean(abs(y_true_bag - y_pred_bag)), 
      time = benchmark$time / 1e9
    ))
  },
  error = function(e) {
    cat("ERROR :",conditionMessage(e), "\n")
    return(tibble(mzoe = NA, mae = NA, time = NA))
  })
}
