#' Read in results file 
#' @param name A string denoting the simulation name, i.e. "wr-car"
#' @param sim A string denoting the simulation number, i.e. "6.0.0".
read_results <- function(name, sim, results_dir = "output", step = 2) {
  fname <- here(results_dir, name, glue("omisvm-sims-results-{name}-{sim}-{step}.rds"))
  read_rds(fname) %>% 
    dplyr::mutate(sim = sim)
}

#' Pull data from specialized strings
#' Assumption is that core components of the string are separated by '_' and 
#' within these strings, they look like 'x=y'. This function pulls out 'y'
#' @param .x A string (or path) to pull from
#' @param i An integer for which component to pull
#' @param n_under An integer for the number of underscores in `.x`
extract_from_string <- function(.x, i = 1, n_under = 2) {
  stopifnot(length(.x) == 1)
  s1 <- tools::file_path_sans_ext(basename(.x))
  s2 <- str_split_fixed(s1, "_", n_under + 1)[, i]
  str_split_fixed(s2, "=", 2)[2]
}

#' Create methods df for plotting
methods_to_show <- function() {
  tibble::tribble(
    ~method, ~short_name, ~color,
    "MIOR (corrected)", "MIOR (corrected)", "#1b9e77",
    "MIOR (xiao)", "MIOR (as written)", "#d95f02",
    "OMISVM", "MISVM ORDINAL (proposed)", "#7570b3",
    "MISVM_OROVA", "MISVM OVA", "#e7298a",
    "SVOR_EXC", "SVOR EXC", "#66a61e"
  )
}

#' Average metric variables after grouping 
average_metric_over <- function(df, grp_vars, metric_vars = c("mae", "mzoe")) {
  df %>%
    group_by(across(all_of(grp_vars))) %>%
    summarize(
      across(all_of(metric_vars), ~mean(.x, na.rm = TRUE)),
      time = sum(time, na.rm = TRUE) + sum(time_sum, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(mean_metric = case_when(
      metric == "mae" ~ mae,
      metric == "mzoe" ~ mzoe
    ))
}

process_data_tma <- function(result) {
  res1 <- result %>% 
    hoist(control, "option", "sigma", .remove = FALSE) %>% 
    mutate(
      method_name = glue(
        "{str_to_upper(fun_name)}",
        "{ifelse(is.na(option), '', glue(' ({option})'))}",
      )
    ) %>% 
    select(method_name, sim, everything()) 
  
  grp_vars <- c("method_name", "fun", "method", "option", "rep", "metric")
  average_metric_over(res1, grp_vars)
}

process_data_size_imdb <- function(result, metric) {
  res1 <- result %>% 
    hoist(control, "option") %>%
    mutate(
      method_name = glue(
        "{str_to_upper(fun_name)}",
        "{ifelse(is.na(option), '', glue(' ({option})'))}",
      ),
      version = map_chr(train_name, ~extract_from_string(.x, i = 4, n_under = 4)),
      nbag = map_chr(train_name, ~extract_from_string(.x, i = 5, n_under = 4))
    ) %>%
    select(method_name, sim, everything()) 
  
  grp_vars <- c("method_name", "fun", "method", "option", 
                "nbag", "version", "metric")
  average_metric_over(res1, grp_vars)
}

process_data_size <- function(result) {
  res1 <- result %>%
    hoist(control, "option", "sigma", .remove = FALSE) %>%
    mutate(
      method_name = glue(
        "{str_to_upper(fun_name)}",
        "{ifelse(is.na(option), '', glue(' ({option})'))}",
      ),
      nbag = map_chr(train_name, ~extract_from_string(.x, i = 3, n_under = 4)),
      ninst = map_chr(train_name, ~extract_from_string(.x, i = 4, n_under = 4)),
      version = map_chr(train_name, ~extract_from_string(.x, i = 5, n_under = 4))
    ) %>%
    select(method_name, sim, everything()) 
  
  grp_vars <- c("method_name", "fun", "method", "option", 
                "nbag", "ninst", "version", "metric")
  average_metric_over(res1, grp_vars)
}

process_data_wr <- function(result) {
  res1 <- result %>% 
    hoist(control, "option", "sigma", .remove = FALSE) %>% 
    mutate(
      method_name = glue(
        "{str_to_upper(fun_name)}",
        "{ifelse(is.na(option), '', glue(' ({option})'))}",
      ),
      wr = map_chr(name, ~extract_from_string(.x, i = 2, n_under = 3)),
      version = map_chr(name, ~extract_from_string(.x, i = 3, n_under = 3)),
    ) %>% 
    select(method_name, sim, everything()) 
  
  grp_vars <- c("method_name", "fun", "method", "option", "rep",
                "name", "wr", "version", "metric")
  average_metric_over(res1, grp_vars)
}


plot_data_tma <- function(df, .metric) {
  df %>% 
    filter(metric == .metric) %>% 
    ggplot(aes(x = mean_metric, 
               y = method_name, 
               color = method_name)) +
    geom_vline(xintercept = 0, color = "grey70") +   
    ggbeeswarm::geom_beeswarm(aes(color = method_name, 
                                  group = method_name),
                              alpha = 0.6, 
                              groupOnX = FALSE) +
    stat_summary(geom = "errorbar",
                 color = "grey20", 
                 fun.min = ~mean(.x, na.rm = TRUE),
                 fun.max = ~mean(.x, na.rm = TRUE)) +
    scale_y_discrete(limits = methods_to_show()$method,
                     labels = methods_to_show()$short_name) +
    scale_color_manual(limits = methods_to_show()$method,
                       labels = methods_to_show()$short_name,
                       values = methods_to_show()$color) +
    theme_light() +
    theme(legend.position = "none") +
    labs(x = str_to_upper(.metric),
         y = NULL)
}

plot_data_size_imdb <- function(result, .metric) {
  result %>% 
    filter(metric == .metric) %>% 
    ggplot(aes(x = as.numeric(nbag), y = mean_metric, color = method_name)) +
    # geom_hline(yintercept = 0, color = "grey70") +
    geom_quasirandom(width = 5, alpha = 0.3) +
    stat_summary(geom = "line", fun = mean, size = 1.2) +
    scale_x_continuous(breaks = c(30, 150*(1:8))) + 
    scale_color_manual(name = NULL,
                       limits = rev(methods_to_show()$method),
                       labels = rev(methods_to_show()$short_name),
                       values = rev(methods_to_show()$color)) +
    theme_light() + 
    theme(legend.position = "top") +
    labs(x = "Number of training bags",
         y = str_to_upper(.metric))
}

plot_data_size <- function(df, .metric) {
  df %>% 
    filter(metric == .metric) %>% 
    mutate(`N Inst` = as.numeric(ninst)) %>% 
    ggplot(aes(x = as.numeric(nbag), y = mean_metric, color = method_name)) +
    geom_hline(yintercept = 0, color = "grey70") +
    ggbeeswarm::geom_quasirandom(width = 5, alpha = 0.1) +
    stat_summary(geom = "line", fun = mean, size = 1.2) +
    scale_x_continuous(breaks = seq(30, 300, by = 30)) + 
    scale_color_manual(name = NULL,
                       limits = rev(methods_to_show()$method),
                       labels = rev(methods_to_show()$short_name),
                       values = rev(methods_to_show()$color)) +
    facet_wrap(~`N Inst`, scales = "free_x", labeller = "label_both") + 
    theme_light() + 
    theme(legend.position = "top") +
    labs(x = "Number of training bags",
         y = str_to_upper(.metric))
}

plot_data_wr <- function(df, .metric) {
  df %>% 
    filter(metric == .metric) %>% 
    ggplot(aes(x = as.numeric(wr), 
               y = mean_metric, 
               color = method_name)) +
    geom_hline(yintercept = 0, color = "grey70") +   
    ggbeeswarm::geom_quasirandom(width = 0.01, alpha = 0.3) +
    stat_summary(geom = "line", fun = mean, size = 1.2) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_manual(name = NULL,
                       limits = methods_to_show()$method,
                       labels = methods_to_show()$short_name,
                       values = methods_to_show()$color) +
    theme_minimal() +
    theme(legend.position = "top") +
    labs(x = "Witness Rate",
         y = str_to_upper(.metric))
}

#' Plot AUC vs Method with facets
#' 
#' @param data A data.frame with key columns `auc`, `method_name`, where each row
#' corresponds to a simulation result.
#' @param methods A data.frame with key columns `method`, `short_name`, and
#'   `color`, where `method` matches `data$method_name`, `short_name` gives the
#'   display names, and `color` indicates which color to use for the points.
#' @param facets A formula to pass to `ggplot2::facet_grid()`, using variables
#'   from `data`. The default facets by three sample size measures, `ninst`,
#'   `nsample`, and `nbag`.
#'   
#' @return A ggplot object. 
create_results_plot <- function(data, methods, facets = ninst + nsample ~ nbag,
                                alpha = 0.5) {
  data %>% 
    ggplot(aes(x = auc, y = method_name, color = method_name)) +
    ggbeeswarm::geom_quasirandom(
      aes(color = method_name, group = method_name),
      groupOnX = FALSE, 
      alpha = alpha
    ) +
    geom_vline(xintercept = 0.5, color = "grey40") +
    geom_errorbarh(stat = "summary",
                   fun.min = mean, fun.max = mean, 
                   color = "grey10") + 
    facet_grid(facets, 
               labeller = label_both) + 
    scale_y_discrete(limits = methods$method,
                     labels = methods$short_name) +
    scale_color_manual(limits = methods$method,
                       labels = methods$short_name,
                       values = methods$color) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = "Comparing replications of CV procedure across methods", 
         x = "AUROC",
         y = NULL)
}

#' Plot critical differences
#' 
#' @param rank_df A data.frame with two columns `method_name`, which gives the
#'   method and `mean.rank` which is the average rank across a number of data
#'   sets or runs.
#' @param methods A data.frame with key columns `method`, `short_name`, and
#'   `color`, where `method` matches `data$method_name`, `short_name` gives the
#'   display names, and `color` indicates which color to use for the points.
#'   
#' @return A ggplot object
my_cd_plot <- function(rank_df, methods) {
  # Set up data.frame for plot
  obj <- list(
    data = rank_df %>% 
      mutate(
        rank = rank(mean.rank),
        right = ifelse(rank < (n() / 2 + 1), 0, 1), 
        xend = ifelse(rank < (n() / 2 + 1), 0, max(rank)+1),
        yend = ifelse(rank < (n() / 2 + 1), rank - 0.5, max(rank) - rank + 0.5)
      ) %>% 
      left_join(methods, by = c("method_name" = "method")) %>% 
      rename(learner.id = short_name)
  )
  
  
  p <- ggplot(obj$data) +
    geom_segment(y = 0, x = 0, xend = max(obj$data$xend), yend = 0) +
    # geom_point(aes_string("mean.rank", 0, colour = "learner.id"), 
    #            size = 3) +
    geom_segment(aes_string("mean.rank", 0, xend = "mean.rank", 
                            yend = "yend", color = "learner.id"),
                 arrow = arrow(ends = "first", length = unit(0.1, "inches")),
                 lineend = "round", linejoin = "mitre", 
                 size = 1) +
    geom_segment(aes_string("mean.rank", "yend", xend = "xend", 
                            yend = "yend", color = "learner.id"), size = 1) +
    geom_text(aes_string("xend", "yend", label = "learner.id", 
                         hjust = "right"), color = "black", vjust = -1) +
    xlab("Average Rank") +
    scale_y_continuous(expand = expansion(mult = 0, add = c(0, 0.4))) +
    scale_x_continuous(breaks = c(0:max(obj$data$xend))) + 
    scale_color_manual(limits = methods$short_name,
                       labels = methods$short_name,
                       values = methods$color,
                       guide = FALSE) +
    theme_minimal() + 
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.ticks.x = element_line(), 
          legend.position = "none", 
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          # axis.line = element_line(size = 1), 
          axis.line.y = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.background = element_blank())
  
  if (!is.null(obj$cd.info)) {
    cd.x = obj$cd.info$x
    cd.y = obj$cd.info$y
    cd = obj$cd.info$cd
    if (obj$cd.info$test == "bd") {
      if (!is.null(baseline)) {
        assertChoice(baseline, as.character(obj$data$learner.id))
        cd.x = obj$data$mean.rank[obj$data$learner.id == 
                                    baseline]
      }
      p = p + annotate("segment", x = cd.x + cd, xend = cd.x - 
                         cd, y = cd.y, yend = cd.y, alpha = 0.5, color = "darkgrey", 
                       size = 2)
      p = p + annotate("segment", x = cd.x + cd, xend = cd.x + 
                         cd, y = cd.y - 0.05, yend = cd.y + 0.05, color = "darkgrey", 
                       size = 1)
      p = p + annotate("segment", x = cd.x - cd, xend = cd.x - 
                         cd, y = cd.y - 0.05, yend = cd.y + 0.05, color = "darkgrey", 
                       size = 1)
      p = p + annotate("point", x = cd.x, y = cd.y, alpha = 0.5)
      p = p + annotate("text", label = stri_paste("Critical Difference =", 
                                                  round(cd, 2), sep = " "), x = cd.x, y = cd.y + 0.05)
    }
    else {
      nemenyi.data = obj$cd.info$nemenyi.data
      if (!(nrow(nemenyi.data) == 0L)) {
        p <- p + 
          geom_segment(aes_string("xstart", "y", xend = "xend", 
                                  yend = "y"), data = nemenyi.data, size = 1.5, color = "dimgrey", 
                       alpha = 0.9) +
          annotate(
            "text", 
            label = paste0("Critical Difference = ", round(cd, 2)),
            y = max(obj$data$yend) + 0.5 + 0.15, 
            x = mean(obj$data$mean.rank)
          ) +
          # annotate(
          #   "segment",
          #   x = mean(obj$data$mean.rank) - 0.5 * cd,
          #   xend = mean(obj$data$mean.rank) + 0.5 * cd,
          #   y = max(obj$data$yend) + 0.5 + 0.40,
          #   yend = max(obj$data$yend) + 0.5 + 0.40,
          #   size = 1L
          # ) +
          annotate(
            "errorbar", 
            xmin = mean(obj$data$mean.rank) - 0.5 * cd,
            xmax = mean(obj$data$mean.rank) + 0.5 * cd,
            y = max(obj$data$yend) + 0.5 + 0.40,
            # yend = max(obj$data$yend) + 0.5 + 0.40,
            width = 0.1, 
            size = 1L
          )
      }
      else {
        message("No connecting bars to plot!")
      }
    }
  }
  return(p)
}