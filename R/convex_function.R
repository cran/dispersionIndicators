
#' @title Function to check if hull_data_list is a valid list of data frames
#'
#' @param hull_data_list List of data frames representing convex hulls.
#' @param name Name of the hull_data_list for error messages.
#'
#' @return None. The function raises an error if the checks fail.
hull_data_list_check <- function(hull_data_list, name) {
  if (
    !is.list(hull_data_list) ||
    length(hull_data_list) == 0 ||
    length(unique(names(hull_data_list))) != length(hull_data_list) ||
    "" %in% names(hull_data_list) ||
    !all(sapply(hull_data_list, is.data.frame)) ||
    !all(sapply(
      hull_data_list,
      function(df) all(c("order", "value") %in% colnames(df))
    )) ||
    !all(sapply(
      hull_data_list,
      function(df)
        length(unique(df$order)) == (length(df$order) - 1)
          &&
        all(df[1,c("order", "value")] == df[nrow(df), c("order", "value")])
    ))
  ) {
      stop(
        paste0("'", name, "' must be a non-empty list of named elements
        (data.frame), each containing, 'order' and 'value' columns.
        Each 'order' column must have unique values, except for the first
        and last rows"),
        hull_data_list
      )
  }
}

#' @title Function to check if a single variable data frame is valid
#'
#' @param df Data frame containing 'batch', 'order', and 'value' columns.
#' @param name Name of the data frame for error messages.
#'
#' @return None. The function raises an error if the checks fail.
single_variable_df_check <- function(df, name) {
  if (!is.data.frame(df)) {
    stop(paste0("'", name, "' must be a data frame."))
  }
  missing_columns <- setdiff(c("batch", "order", "value"), colnames(df))
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "'", name, "' is missing columns: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}

#' @title Calculate Convex Hulls for one variable
#'
#' @param data Data frame containing the 'batch', 'order' and variable
#' 'value' columns.
#' @param var_name Name of the variable to calculate convex hull for.
#' @param impute_method One of "mean" or "median".
#'
#' @return A list of dataframes of convex hull.
#' @importFrom grDevices chull
#' @export
calculate_convex_hull <- function(
  data, var_name, impute_method = c("mean", "median")
) {
  # Precondition checks
  impute_method <- match.arg(impute_method)
  single_variable_df_check(data, "data")
  # Function logic
  hull_data_list <- list()
  for (b in unique(data$batch)) {
    batch_data <- data[data$batch == b, ]
    if (nrow(batch_data) >= 3 && sum(!is.na(batch_data$value)) >= 3) {
      batch_data$value[is.na(batch_data$value)] <- switch(impute_method,
        mean = mean(batch_data$value, na.rm = TRUE),
        median = median(batch_data$value, na.rm = TRUE)
      )
      idx <- chull(batch_data$order, batch_data$value)
      hull_data_list[[b]] <- batch_data[c(idx, idx[1]), ]
    } else {
      warning(
        "Batch '", b, "' of variable '", var_name,
        "' has less than 3 valid points. Skipping convex hull calculation."
      )
    }
  }
  return(hull_data_list)
}

#' @title Compute the shoelace core for convex hulls of a single variable
#'
#' @param hull_data_list named list of data frames of convex hulls, for each batch.
#'
#' @return named list of dataframes of convex hull concatenated with a column of shoelace core values, for each batch.
compute_shoelace_core <- function(hull_data_list) {
  res <- hull_data_list
  for (b in names(res)) {
    h <- hull_data_list[[b]]
    x <- h$order
    y <- h$value
    res[[b]][["shoelace"]] <- x * c(y[-1], y[1]) - c(x[-1], x[1]) * y
  }
  return(res)
}

#' @title Calculate the intra batch dispersion indicator on convex hulls of a single
#' variable
#'
#' @param hull_data_shoelace_list named list of convex hulls data frames with an additional column of shoelace core
#' values, for each batch.
#'
#' @return value of intra batch dispersion.
compute_intra_batch_dispersion <- function(hull_data_shoelace_list) {
  intraB_area <- NULL
  nb_pt_total <- 0
  for (b in names(hull_data_shoelace_list)) {
    shoelace_core <- hull_data_shoelace_list[[b]]$shoelace
    intraB_area <-  append(
      intraB_area,
      (length(shoelace_core)-1) * 0.5 * abs(sum(
        shoelace_core
      ))
    )
    nb_pt_total <- nb_pt_total + length(shoelace_core) - 1
  }
  return(median(intraB_area/nb_pt_total, na.rm = TRUE))
}

#' @title Calculate the inter batch dispersion indicator on convex hulls of a single
#' variable
#'
#' @param hull_data_shoelace_list named list of convex hulls data frames with an additional column of shoelace core
#'
#' @return value of inter batch dispersion.
#' @importFrom grDevices chull
compute_inter_batch_dispersion <- function(hull_data_shoelace_list) {
  if (length(names(hull_data_shoelace_list)) > 1) {
    batch_centroids <- NULL
    ref_batch <- names(hull_data_shoelace_list)[1]
    for (b in names(hull_data_shoelace_list)) {
      shoelace_core <- hull_data_shoelace_list[[b]]$shoelace
      x <- hull_data_shoelace_list[[b]]$order
      y <- hull_data_shoelace_list[[b]]$value
      area <- 0.5 * abs(sum(
          shoelace_core
      ))
      x_center <- abs((1/(6*area))*sum((x + c(x[-1], x[1])) * shoelace_core))
      y_center <- abs((1/(6*area))*sum((y + c(y[-1], y[1])) * shoelace_core))
      batch_centroids <- rbind(
        batch_centroids,
        data.frame(x = x_center, y = y_center, row.names=b)
      )
    }
    distances <- as.matrix(dist(batch_centroids))
    interB <- median(distances[ref_batch, -which(row.names(distances) == ref_batch)])
  } else {
    interB <- 0
  }
  return(interB)
}

#' @title Calculate the intra/inter batch dispersion ratio indicator on convex hulls
#' of a single variable.
#'
#' @param intraB_disp value of intra batch dispersion indicator.
#' @param interB_disp value of inter batch dispersion indicator.
#'
#' @return value of intra/inter batch dispersion ratio.
compute_ratio <- function(intraB_disp, interB_disp) {
  if (interB_disp == 0) {
      warning("Inter batch dispersion is zero, returning NA for ratio.")
      return(NA)
  }else{
    ratio <- intraB_disp / interB_disp
  }
  return(ratio)
}

#' @title Calculate the intra/inter batch dispersion indicators and their ratio on
#' convex hulls of a single variable.
#'
#' @param hull_data_list list of data frames of convex hulls.
#' @param var_name name of the variable.
#' @return A data frame with the indicators values.
#' @export
calculate_convex_indicators <- function(
  hull_data_list, var_name
) {
  # Precondition checks
  hull_data_list_check(hull_data_list, "hull_data_list")
  hull_data_shoelace_list <- compute_shoelace_core(hull_data_list)
  intraB_disp <- compute_intra_batch_dispersion(hull_data_shoelace_list)
  interB_disp <- compute_inter_batch_dispersion(hull_data_shoelace_list)
  indicators <- data.frame(
    Variable = var_name,
    IntraB = round(intraB_disp, 6),
    InterB = round(interB_disp, 6),
    Ratio = round(compute_ratio(intraB_disp, interB_disp), 6)
  )
  return(indicators)
}

#' @title Plot the convex hulls of a single variable.
#'
#' @param data Data frame containing the batch, order and variable
#' value columns.
#' @param hull_data_list List of data frames of convex hulls.
#' @param var_name Name of the variable.
#' @param show_points Boolean indicating whether to show points.
#' @param label_prefix Prefix for the plot title.
#' @param indicators Data frame with the indicators values.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon labs theme_minimal
#' .data
#' @export
plot_convex_hull <- function(
  data, hull_data_list, var_name, show_points, label_prefix, indicators
) {
  # Precondition checks
  hull_data_list_check(hull_data_list, "hull_data_list")
  single_variable_df_check(data, "data")
  if (!is.logical(show_points)) {
    stop("'show_points' must be a logical value (TRUE or FALSE).")
  }
  if (
    !is.data.frame(indicators) ||
    !all(
      c("IntraB", "InterB", "Ratio") %in% colnames(indicators)
    )
  ) {
    stop("'indicators' must be a data frame,
     with columns 'IntraB', 'InterB', and 'Ratio'.")
  }
  # Function logic
  plot <- ggplot(
      data[data$batch %in% names(hull_data_list), ],
      aes(x = .data$order, y = .data$value ,color = .data$batch)
    ) +
    geom_point(size = ifelse(show_points, 2, 0)) +
    geom_polygon(
      data = do.call(rbind, hull_data_list),
      aes(
        x = .data$order,
        y = .data$value,
        group = .data$batch,
        fill = .data$batch
      ),
      alpha = 0.2, color = NA,
      inherit.aes = FALSE
    ) +
    labs(
      title = paste0(label_prefix, "Convex Hull - ", var_name),
      subtitle = paste0(
        "IntraB = ", round(indicators$IntraB, 2),
        ", InterB = ", round(indicators$InterB, 2),
        ", Ratio = ", round(indicators$Ratio, 2)
      ),
      x = "Injection Order",
      y = paste("Intensity -", var_name),
    ) +
    theme_minimal(base_family = "sans")

  return(plot)
}

#' @title Plot all convex hulls for each variable in a PDF file.
#'
#' @param target_file_path Path to the output PDF file.
#' @param convex_analysis_res Result of the convex analysis containing data,
#'  convex hulls and indicators.
#' @param show_points Boolean indicating whether to show points in the plot.
#' @param mode Mode of the analysis, either "global" or "batchwise".
#' @return None. The function saves the plots to a PDF file.
#' @importFrom grDevices cairo_pdf dev.off
#' @export
plot_all_convex_hulls <- function(
  target_file_path,
  convex_analysis_res,
  show_points,
  mode = c("global", "batchwise")
) {
  # Precondition checks
  mode <- match.arg(mode)
  if (
    !is.list(convex_analysis_res) ||
    !all(
      c("data", "convex_hulls", "indicators") %in% names(convex_analysis_res)
    )
  ) {
    stop("'convex_analysis_res' must be a list,
     resulting from convex_analysis_of_variables function call,
     containing 'data', 'convex_hulls', and 'indicators' named variables")
  }
  # Function logic
  cairo_pdf(target_file_path, width = 7, height = 5, family = "sans")
  for (var in names(convex_analysis_res$data)) {
    plot <- plot_convex_hull(
      data = convex_analysis_res$data[[var]],
      hull_data_list = convex_analysis_res$convex_hulls[[var]],
      var_name = var,
      show_points = show_points,
      label_prefix = paste0(
        toupper(substr(mode,1,1)),
        substr(mode,2,nchar(mode)),
        " - "
      ),
      indicators = convex_analysis_res$indicators[
        convex_analysis_res$indicators$Variable == var,
      ]
    )
    print(plot)
  }
  dev.off()
}

#' @title Analyze a set of variables using convex hulls.
#'
#' @param data Data frame containing the data of multiple variable on multiple
#' ordered and potentially batched sample.
#' @param variable_columns Character vector of variable column names to analyse.
#' @param batch_col Name of the column containing batch information.
#' @param sample_order_col Name of the column containing the sample time order.
#' @param impute_if_needed Method for imputing missing values, either "mean" or
#' "median".
#' @param mode Analysis mode, either "global" or "batchwise"
#' @return A list containing the following elements:
#'   - data: List of data frames for each variable.
#'   - indicators: Data frame with convex hull indicators for each variable.
#'   - convex_hulls: List of data frames of convex hulls for each varaible.
#' @export
#' @examples
#' # Example usage on toy metabolomics data:
#' data <- data.frame(
#'   batch = rep(c("A","B","C"), each = 10),
#'   injectionOrder = rep(1:30, times = 1),
#'   metabolite1 = rnorm(30, mean = 100, sd = 10),
#'   metabolite2 = rnorm(30, mean = 200, sd = 20)
#' )
#' result <- convex_analysis_of_variables(
#'   data = data,
#'   variable_columns = c("metabolite1", "metabolite2"),
#'   batch_col = "batch",
#'   sample_order_col = "injectionOrder",
#'   impute_if_needed = "median",
#'   mode = "global"
#' )
#' plot_all_convex_hulls(
#'   target_file_path = file.path(tempdir(), "convex_hulls.pdf"),
#'   convex_analysis_res = result,
#'   show_points = TRUE,
#'   mode = "global"
#' )
convex_analysis_of_variables <- function(
  data,
  variable_columns,
  batch_col = "batch",
  sample_order_col = "order",
  impute_if_needed = c("median","mean"),
  mode = c("global", "batchwise")
) {
  # Precondition checks
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  mode <- match.arg(mode)
  impute_if_needed <- match.arg(impute_if_needed)
  if (
    !all(variable_columns %in% colnames(data))
  ) stop("One or more variables to analyse are not in the data file.")
  if (
    !batch_col %in% colnames(data)
  ) stop("Column ", batch_col, " could not be found.")
  if (
    !sample_order_col %in% colnames(data)
  ) stop("Column ", sample_order_col, " could not be found.")
  if (
    (length(unique(data[[sample_order_col]])) != nrow(data))
      ||
     (!all(is.integer(data[[sample_order_col]])))
  ) stop(
    "Column ", sample_order_col, " must contain non-duplicate integer values."
  )
  # Function logic
  if (mode == "batchwise") {
    order_col_use <- paste(sample_order_col, "WithinBatch")
    data[[order_col_use]] <- ave(
      data[[sample_order_col]],
      data[[batch_col]],
      FUN = function(x) seq_along(x)
    )
  } else {
    order_col_use <- sample_order_col
  }
  res_indicators <- list()
  res_data <- list()
  res_convex_hulls <- list()
  for (met in variable_columns) {
    d <- data[, c(batch_col, order_col_use, met)]
    colnames(d) <- c('batch', 'order', "value")
    d <- d[!is.na(d$order), ]
    convex_hulls <- calculate_convex_hull(
      data = d,
      var_name = met,
      impute_method = impute_if_needed
    )
    indicators <- calculate_convex_indicators(
      hull_data_list = convex_hulls,
      var_name = met
    )
    res_indicators[[met]] <- indicators
    res_data[[met]] <- d
    res_convex_hulls[[met]] <- convex_hulls
  }
  res_indicators <- do.call(rbind, res_indicators)

  return(list(
    data=res_data, indicators=res_indicators, convex_hulls=res_convex_hulls
  ))
}
