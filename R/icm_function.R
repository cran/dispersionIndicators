#' @title Computes Integrated Covariance Mahalanobis (ICM) distances of all
#' individuals in PCA-reduced space, against their global barycenter reference.
#' @param pc_data PCA-reduced data frame.
#' @param batch_col Name of the column representing batch or group.
#' @return A data frame with Mahalanobis distances for each individual against
#' the global barycenter.
#' @import stats
#' @import corpcor
compute_individual_global <- function(pc_data, batch_col) {
  pc_numeric <- as.data.frame(
    lapply(pc_data[, -which(names(pc_data) == batch_col)], as.numeric)
  )
  center <- apply(
    pc_numeric,
    2,
    median
  )
  cov_matrix <- tryCatch({
    cov_mat <- stats::cov(pc_numeric)
    if (det(cov_mat) <= .Machine$double.eps) stop("Singular matrix")
    cov_mat
  }, error = function(e) corpcor::cov.shrink(pc_numeric, verbose = FALSE))
  mah <- stats::mahalanobis(pc_numeric, center = center, cov = cov_matrix)
  dist_all <- data.frame(
    ID = seq_len(nrow(pc_numeric)),
    Group = if (!is.null(batch_col)) pc_data[[batch_col]] else "Global",
    Mahalanobis_Distance = mah,
    Reference = "Global"
  )
  return(dist_all)
}

#' @title Computes Integrated Covariance Mahalanobis (ICM) distances of all
#' individuals in PCA-reduced space, against their batch-wise barycenter
#' reference.
#'
#' @param pc_data PCA-reduced data frame.
#' @param batch_col Name of the column representing batch or group.
#' @return A data frame with Mahalanobis distances for each individual against
#' their batch barycenter.
#' @import stats
#' @import corpcor
compute_individual_batch <- function(pc_data, batch_col) {
  batch_list <- split(pc_data, pc_data[[batch_col]])
  dist_all <- data.frame()
  for (b in names(batch_list)) {
    subset <- batch_list[[b]]
    subset <- subset[, -which(names(subset) == batch_col)]
    center <- apply(subset, 2, median)
    cov_matrix <- tryCatch({
      cov_mat <- stats::cov(subset)
      if (det(cov_mat) <= .Machine$double.eps) stop("Singular matrix")
      cov_mat
    }, error = function(e) corpcor::cov.shrink(subset, verbose = FALSE))

    mah <- stats::mahalanobis(subset, center = center, cov = cov_matrix)
    dist_df_b <- data.frame(ID = seq_len(nrow(subset)), Group = b,
                            Mahalanobis_Distance = mah, Reference = "Batch")
    dist_all <- rbind(dist_all, dist_df_b)
  }
  return(dist_all)
}

#' @title Computes Integrated Covariance Mahalanobis (ICM) distances for
#' individuals, in PCA-reduced space, against either global or batch-wise
#' references.
#'
#' @param pc_data PCA-reduced data frame.
#' @param ref Reference type: "global" for global barycenter, "batch" for
#' batch-wise barycenters.
#' @param batch_col Name of the column representing batch or group.
#' @return A data frame with Mahalanobis distances for each individual against
#' the specified reference.
compute_individual <- function(pc_data, ref = c("global", "batch"), batch_col) {
  ref <- match.arg(ref)
  dist_all <- switch(
    ref,
    global = compute_individual_global(pc_data, batch_col),
    batch = compute_individual_batch(pc_data, batch_col)
  )
  return(dist_all)
}

#' @title Computes Integrated Covariance Mahalanobis (ICM) mean distances
#' within each batch in PCA-reduced space, using median and mean for center
#' references.
#'
#' @param pc_data PCA-reduced data frame.
#' @param batch_col Name of the column representing batch or group.
#' @return A data frame with Mahalanobis distances mean for each batch.
#' @import stats
#' @import corpcor
compute_intra <- function(pc_data, batch_col) {
  batch_list <- split(pc_data, pc_data[[batch_col]])
  summary_df <- data.frame()
  intra_values <- c()
  for (b in names(batch_list)) {
    subset <- batch_list[[b]]
    subset <- subset[, -which(names(subset) == batch_col)]
    center <- apply(subset, 2, median)
    cov_matrix <- tryCatch({
      cov_mat <- stats::cov(subset)
      if (det(cov_mat) <= .Machine$double.eps) stop("Singular matrix")
      cov_mat
    }, error = function(e) corpcor::cov.shrink(subset, verbose = FALSE))
    mah <- mean(
      stats::mahalanobis(subset, center = center, cov = cov_matrix)
    )
    intra_values <- c(intra_values, mah)
    summary_df <- rbind(summary_df, data.frame(Batch = b, Intra_ICM = mah))
  }
  global_median <- median(intra_values)
  global_mean <- mean(intra_values)
  summary_df <- rbind(
    summary_df,
    data.frame(Batch = "Global_Median", Intra_ICM = global_median),
    data.frame(Batch = "Global_Mean", Intra_ICM = global_mean)
  )
  return(summary_df)
}

#' @title Computes Integrated Covariance Mahalanobis (ICM) distances between
#' batches barycenters in PCA-reduced space, using a reference bacth and either
#' mean or median for center references.
#'
#' @param pc_data PCA-reduced data frame.
#' @param batch_col Name of the column representing batch or group.
#' @param ref_batch Name of the reference batch for distance computation.
#' @param center_method Method for centering: "mean" or "median".
#' @return A data frame with Mahalanobis distances for each batch against the
#' reference.
#' @import stats
#' @import corpcor
compute_inter <- function(
  pc_data,
  batch_col,
  ref_batch,
  center_method=c("mean", "median")
) {
  center_method <- match.arg(center_method)
  bary_list <- by(pc_data, pc_data[[batch_col]], function(df) {
    if (center_method == "mean")
      colMeans(df[, which(names(df)!=batch_col)])
    else
      apply(df[, which(names(df)!=batch_col)], 2, median)
  })
  bary_df <- do.call(rbind, bary_list)
  rownames(bary_df) <- names(bary_list)
  cov_matrix <- tryCatch({
      cov_mat <- stats::cov(bary_df)
      if (det(cov_mat) <= .Machine$double.eps) stop("Singular matrix")
      cov_mat
    },
    error = function(e) corpcor::cov.shrink(bary_df, verbose = FALSE)
  )
  ref <- bary_df[ref_batch, ]
  mah <- stats::mahalanobis(bary_df, center = ref, cov = cov_matrix)
  dist_df <- data.frame(
    Batch = rownames(bary_df),
    Mahalanobis_Distance = mah,
    Reference = rownames(bary_df) == ref_batch
  )
  dist_df <- rbind(
    dist_df,
    data.frame(
      Batch = "Global_Median",
      Mahalanobis_Distance = median(mah),
      Reference = NA
    ),
    data.frame(
      Batch = "Global_Mean",
      Mahalanobis_Distance = mean(mah),
      Reference = NA
    )
  )
  return(dist_df)
}

#' @title Compute ICM (Integrated Covariance Mahalanobis) Distances
#'
#' @description This function computes Mahalanobis distances in PCA-reduced space, with
#' options for individual, intra-group, and inter-group comparisons. It supports
#' batch-wise analysis and shrinkage covariance estimation for robustness.
#'
#' @param data A data.frame containing numeric variables and optionally a
#' batch/group column.
#' @param batch_col Name of the column representing batch or group (optional).
#' @param mode Mode of computation: "individual", "intra", "inter", or "all".
#' @param variance_threshold Threshold for cumulative variance to retain in
#' PCA (default: 0.95).
#' @param center_method_individual Method for centering in "individual" mode:
#' "global" or "batch" (default: "global").
#' @param center_method_inter Method for centering in "inter" mode: "mean" or
#' "median" (default: "mean").
#' @param ref_batch Reference batch name to compute inter-batch distances
#' (default: first batch).
#'
#' @return A list containing data.frames of computed distances depending on the
#' selected mode(s).
#' @import stats
#' @export
#' 
#' @examples
#' data <- data.frame(matrix(rnorm(100*5), ncol = 5))
#' data$Batch <- rep(c("A", "B", "C", "D"), each = 25)
#' result <- compute_icm_distances(
#'  data,
#'  batch_col = "Batch",
#'  mode = "all",
#'  center_method_individual = "batch",
#'  center_method_inter = "mean"
#' )
#' print(result)
compute_icm_distances <- function(
  data,
  batch_col = NULL,
  mode = c("individual", "intra", "inter", "all"),
  variance_threshold = 0.95,
  center_method_individual = c("global", "batch"),
  center_method_inter = c("mean", "median"),
  ref_batch = NULL
) {
  mode <- match.arg(mode)
  center_method_individual <- match.arg(center_method_individual)
  center_method_inter <- match.arg(center_method_inter)
  # Preprocessing (imputation + PCA)
  numeric_cols <- sapply(data, is.numeric)
  data[, numeric_cols] <- lapply(data[, numeric_cols], function(x) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    return(x)
  })
  pca_result <- stats::prcomp(data[, numeric_cols], scale. = TRUE)
  cum_var <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
  n_comp <- which(cum_var >= variance_threshold)[1]
  pca_data <- as.data.frame(pca_result$x[, 1:n_comp, drop = FALSE])
  if (!is.null(batch_col)) pca_data[[batch_col]] <- data[[batch_col]]
  # Compute distances
  distances <- list()
  if (mode %in% c("individual", "all")) {
    distances$individual <- compute_individual(
      pca_data,
      center_method_individual,
      batch_col = batch_col
    )
  }
  if (mode %in% c("intra", "all")) {
    distances$intra <- compute_intra(pca_data, batch_col = batch_col)
  }
  if (mode %in% c("inter", "all")) {
    if (is.null(ref_batch)) {
      batch_names <- unique(pca_data[[batch_col]])
      ref_batch <- batch_names[1]
    }
    distances$inter <- compute_inter(
      pca_data,
      batch_col,
      ref_batch,
      center_method_inter
    )
  }
  return(distances)
}

#' @title Save ICM Distances to CSV Files
#'
#' @param distances A list containing data.frames of distances (result from
#' `compute_icm_distances`)
#' @param folder_path Path to the folder where files will be saved.
#' @param prefix Prefix for the output file names.
#' @return None. Saves files to folder_path.
#' @import utils
#' @export
save_icm_distances_csv <- function(
  distances, folder_path, prefix="ICM"
){
  for (k in intersect(c("individual", "intra", "inter"), names(distances))) {
    dfile <- file.path(
      folder_path,
      paste(
        paste(prefix, k, "distances", format(Sys.Date(), "%Y%m%d"), sep = "_"),
        "csv",
        sep = "."
      )
    )
    utils::write.csv(distances[[k]], dfile, row.names = FALSE)
    message("File ", k, " saved : ", dfile)
  }
}