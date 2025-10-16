# Créer un jeu de données de test
set.seed(123)
data <- data.frame(matrix(rnorm(400 * 5), ncol = 5))
data$Batch <- rep(c("A", "B", "C", "D"), each = 100)
pc_data <- data.frame(
  PC1 = rnorm(500),
  PC2 = rnorm(500),
  Batch = rep(c("A", "B", "C", "D", "E"), each = 100)
)
# Tests pour compute_individual_global
test_that("compute_individual_global works correctly", {
  result <- compute_individual_global(pc_data, "Batch")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(pc_data))
  expect_true(all(
    colnames(result) == c("ID", "Group", "Mahalanobis_Distance", "Reference")
  ))
})

# Tests pour compute_individual_batch
test_that("compute_individual_batch works correctly", {
  result <- compute_individual_batch(pc_data, "Batch")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(pc_data))
  expect_true(all(
    colnames(result) == c("ID", "Group", "Mahalanobis_Distance", "Reference")
  ))
})

# Tests pour compute_individual
test_that("compute_individual works correctly", {
  result_global <- compute_individual(
    pc_data, ref = "global", batch_col = "Batch"
  )
  result_batch <- compute_individual(
    pc_data, ref = "batch", batch_col = "Batch"
  )
  expect_true(is.data.frame(result_global))
  expect_true(is.data.frame(result_batch))
  expect_equal(nrow(result_global), nrow(pc_data))
  expect_equal(nrow(result_batch), nrow(pc_data))
})

# Tests pour compute_intra
test_that("compute_intra works correctly", {
  result <- compute_intra(pc_data, "Batch")
  expect_true(is.data.frame(result))
  expect_true(all(colnames(result) == c("Batch", "Intra_ICM")))
})

# Tests pour compute_inter
test_that("compute_inter works correctly", {
  result <- compute_inter(
    pc_data, "Batch", ref_batch = "A", center_method = "mean"
  )
  expect_true(is.data.frame(result))
  expect_true(
    all(colnames(result) == c("Batch", "Mahalanobis_Distance", "Reference"))
  )
})

# Tests pour compute_icm_distances
test_that("compute_icm_distances works correctly", {
  result <- compute_icm_distances(
    data,
    batch_col = "Batch",
    mode = "all",
    center_method_individual = "batch",
    center_method_inter = "mean"
  )
  expect_type(result, "list")
  expect_true(all(c("individual", "intra", "inter") %in% names(result)))
  expect_true(is.data.frame(result$individual))
  expect_true(is.data.frame(result$intra))
  expect_true(is.data.frame(result$inter))
})

# Tests pour save_icm_distances_csv
test_that("save_icm_distances_csv works correctly", {
  distances <- list(
    individual = data.frame(
      ID = 1:10,
      Group = rep(c("A", "B"), each = 5),
      Mahalanobis_Distance = rnorm(10),
      Reference = "Global"
    ),
    intra = data.frame(
      Batch = c("A", "B", "Global_Median", "Global_Mean"),
      Intra_ICM = c(
        rnorm(2), median(rnorm(10)), mean(rnorm(10))
      )
    ),
    inter = data.frame(
      Batch = c("A", "B", "Global_Median", "Global_Mean"),
      Mahalanobis_Distance = c(
        rnorm(2),
        median(rnorm(10)),
        mean(rnorm(10))
      ),
      Reference = c(TRUE, FALSE, NA, NA))
  )
  tmp_dir <- tempdir()
  expect_message(
    save_icm_distances_csv(distances, tmp_dir, prefix = "Test"),
    "File individual saved"
  )
  expect_message(
    save_icm_distances_csv(distances, tmp_dir, prefix = "Test"),
    "File intra saved"
  )
  expect_message(
    save_icm_distances_csv(distances, tmp_dir, prefix = "Test"),
    "File inter saved"
  )
  csv_files <- list.files(
    path = tmp_dir,
    pattern = "^Test_(intra|inter|individual)_distances_\\d{8}\\.csv$",
    full.names = TRUE
  )
  expect_true(length(csv_files) == 3)
})

# remove csv file created by tests
csv_files <- list.files(
  path = ".",
  pattern = "*.csv$",
  full.names = TRUE
)
file.remove(csv_files)