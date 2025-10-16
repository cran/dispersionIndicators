tmp_dir <- tempdir()

test_that(
  "hull_data_list_check works with valid input",
  {
    data <- list(
      A=data.frame(order=c(1,2,3,4,1), value=c(5,6,7,8,5), dummy=rep(NA, 5)),
      B=data.frame(order=c(1,2,3,4,1), value=c(5,6,7,8,5), dummy=rep(NA, 5))
    )
    expect_null(hull_data_list_check(data, "eat"))
  }
)

test_that(
  "hull_data_list_check fails with unnamed list",
  {
    data <- list(
      data.frame(order=c(1,2,3,4,1), value=c(5,6,7,8,5), dummy=rep(NA, 5)),
      data.frame(order=c(1,2,3,4,1), value=c(5,6,7,8,5), dummy=rep(NA, 5))
    )
    expect_error(hull_data_list_check(data, "the"), "the")
  }
)

test_that(
  "hull_data_list_check fails without order column",
  {
    data <- list(
      A=data.frame(value=c(5,6,7,8,5), dummy=rep(NA, 5)),
      B=data.frame(value=c(5,6,7,8,5), dummy=rep(NA, 5))
    )
    expect_error(hull_data_list_check(data, "rich"), "rich")
  }
)

test_that(
  "hull_data_list_check fails without value column",
  {
    data <- list(
      A=data.frame(value=c(5,6,7,8,5), dummy=rep(NA, 5)),
      B=data.frame(value=c(5,6,7,8,5), dummy=rep(NA, 5))
    )
    expect_error(hull_data_list_check(data, "seize"), "seize")
  }
)

test_that(
  "hull_data_list_check fails with non-unique values",
  {
    data <- list(
      A=data.frame(order=c(1, 1, 3, 4), value=c(5, 5, 7, 8), dummy=rep(NA, 4)),
      B=data.frame(
        order=c(1, 2, 3, 4, 1), value=c(5, 6, 7, 8, 5), dummy=rep(NA, 5)
      )
    )
    expect_error(hull_data_list_check(data, "means"), "means")
  }

)

test_that("single_variable_check works with valid input", {
  data <- data.frame(
    batch=c("A", "A", "B", "B"),
    order=1:4,
    value=5:8,
    dummy=rep(NA, 4)
  )
  expect_null(single_variable_df_check(data, "production"))
})

test_that("single_variable_check fails without batch column", {
  data <- data.frame(
    order=1:4,
    value=5:8,
    dummy=rep(NA, 4)
  )
  expect_error(single_variable_df_check(data, "alice"), "alice")
})

test_that("single_variable_check fails without order column", {
  data <- data.frame(
    batch=c("A", "A", "B", "B"),
    value=5:8,
    dummy=rep(NA, 4)
  )
  expect_error(single_variable_df_check(data, "alice"), "alice")
})

test_that("single_variable_check fails without value column", {
  data <- data.frame(
    batch=c("A", "A", "B", "B"),
    order=1:4,
    dummy=rep(NA, 4)
  )
  expect_error(single_variable_df_check(data, "alice"), "alice")
})

test_that(
  "compute_intra_batch_dispersion works with valid input",
  {
    data <- list(
      A=data.frame(order=1:4, value=c(1,2,1,0), dummy=rep(NA, 4)),
      B=data.frame(order=5:8, value=c(1,4,3,2), dummy=rep(NA, 4))
    )
    result <- compute_intra_batch_dispersion(compute_shoelace_core(data))
    expect_true(is.numeric(result))
    expect_equal(result, 1.5)
  }
)

test_that(
  "compute_inter_batch_dispersion works with valid input",
  {
    data <- list(
      A=data.frame(order=1:4, value=c(1,2,1,0), dummy=rep(NA, 4)),
      B=data.frame(order=5:8, value=c(1,4,3,2), dummy=rep(NA, 4))
    )
    result <- compute_inter_batch_dispersion(compute_shoelace_core(data))
    expect_true(is.numeric(result))
    expect_true(round(result,5)==4.21637)
    data <- list(
      A=data.frame(order=1:4, value=c(1,2,1,0), dummy=rep(NA, 4)),
      B=data.frame(order=1:4, value=c(1,4,3,2), dummy=rep(NA, 4))
    )
    result <- compute_inter_batch_dispersion(compute_shoelace_core(data))
    expect_true(is.numeric(result))
    expect_true(round(result,5)==1.33333)
  }
)

test_that(
  "compute_ratio works with valid input",
  {
    result <- compute_ratio(6,7)
    expect_true(is.numeric(result))
    expect_true(result==6/7)
  }
)

test_that(
  "compute_ratio warns with zero denominator",
  {
    suppressWarnings(
      result <- compute_ratio(6,0)
    )
    expect_warning(compute_ratio(6,0))
    expect_true(is.na(result))
  }
)

test_that(
  "calculate_convex_indicators works with valid input",
  {
    data <- list(
      A=data.frame(order=c(1,2,3,4,1), value=c(1,2,1,0,1), dummy=rep(NA, 5)),
      B=data.frame(order=c(5,6,7,8,5), value=c(1,4,3,2,1), dummy=rep(NA, 5))
    )
    result <- calculate_convex_indicators(data, "marx")
    expect_true(is.data.frame(result))
    expect_true(
      all(c("Variable", "InterB", "IntraB", "Ratio") %in% names(result))
    )
    expect_true(nrow(result) == 1)
    expect_true(result$Variable == "marx")
  }
)



test_that(
  "calculate_convex_hull works with valid input",
  {
    data <- data.frame(
      batch=c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
      order=1:10,
      value=c(1,2,1,0,1,4,3,2,5,6),
      dummy=rep(NA, 10)
    )
    res <- calculate_convex_hull(data,"blah", "mean")
    exp <- list(
      A=data.frame(
        batch=rep("A",5),
        order=c(4,1,2,5,4),
        value=c(0,1,2,1,0),
        dummy=rep(NA, 5),
        row.names = c('4','1','2','5','4.1')
      ),
      B=data.frame(
        batch=rep("B",4),
        order=c(8,6,10,8),
        value=c(2,4,6,2),
        dummy=rep(NA, 4),
        row.names = c('8','6','10','8.1')
      )
    )
    expect_null(hull_data_list_check(res, ""))
    expect_true(all(sapply(res, function (df) nrow(df) >= 3)))
    expect_true(all(names(res) == c("A", "B")))
    expect_true(all(res$A$order == exp$A$order))
    expect_true(all(res$B$order == exp$B$order))
    expect_true(all(res$A$value == exp$A$value))
    expect_true(all(res$B$value == exp$B$value))
    expect_true(all(is.na(res$A$dummy)))
    expect_true(all(is.na(res$B$dummy)))
  }
)

test_that(
  "calculate_convex_hull works with imputation",
  {
    data <- data.frame(
      batch=c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
      order=1:10,
      value=c(1,2,NA,4,NA,4,3,2,5,6),
      dummy=rep(NA, 10)
    )
    exp <- list(
      Amean=data.frame(
        batch=rep("A",4),
        order=c(5,1,4,5),
        value=c(7/3,1,4,7/3),
        dummy=rep(NA, 4),
        row.names = c('1','2','4','1.1')
      ),
      Amedian=data.frame(
        batch=rep("A",4),
        order=c(5,1,4,5),
        value=c(2,1,4,2),
        dummy=rep(NA, 4),
        row.names = c('1','2','4','1.1')
      ),
      B=data.frame(
        batch=rep("B",4),
        order=c(8,6,10,8),
        value=c(2,4,6,2),
        dummy=rep(NA, 4),
        row.names = c('8','6','10','8.1')
      )
    )
    res_mean <- calculate_convex_hull(data, "blah", "mean")
    res_median <- calculate_convex_hull(data, "blah", "median")
    expect_null(hull_data_list_check(res_median, ""))
    expect_null(hull_data_list_check(res_mean, ""))
    expect_true(all(sapply(res_mean, function (df) nrow(df) >= 3)))
    expect_equal(names(res_mean), c("A", "B"))
    expect_equal(res_mean$A$order, exp$Amean$order)
    expect_equal(res_median$A$order, exp$Amedian$order)
    expect_equal(res_mean$B$order, exp$B$order)
    expect_equal(res_mean$A$value, exp$Amean$value)
    expect_equal(res_median$A$value, exp$Amedian$value)
    expect_equal(res_mean$B$value, exp$B$value)
    expect_true(all(is.na(res_mean$A$dummy)))
    expect_true(all(is.na(res_mean$B$dummy)))
  }
)

test_that(
  "calculate_convex_hull warns with unvalid batch",
  {
    data <- data.frame(
      batch=c("A", "A", "A", "A", "A", "B", "B"),
      order=1:7,
      value=c(1,NA,NA,NA,NA, 2, 3),
      dummy=rep(NA, 7)
    )
    ws <- capture_warnings(
      calculate_convex_hull(data, "trend", "mean")
    )
    expect_true(length(ws) == 2)
  }
)


test_that(
  "convex_analysis_of_variables works with valid input",
  {
    set.seed(101917)
    data <- data.frame(
      batch = rep(c("A", "B", "C"), each = 10),
      injectionOrder = rep(1:30, times = 1),
      metabolite1 = round(rnorm(30, mean = 100, sd = 10),3),
      metabolite2 = round(rnorm(30, mean = 200, sd = 20), 3)
    )
    result <- convex_analysis_of_variables(
      data = data,
      variable_columns = c("metabolite1", "metabolite2"),
      batch_col = "batch",
      sample_order_col = "injectionOrder",
      mode = "global"
    )
    expect_true(is.list(result))
    expect_true(all(names(result) == c("data", "indicators", "convex_hulls")))
    expect_true(is.list(result$data))
    expect_true(is.list(result$convex_hulls))
    expect_true(is.data.frame(result$indicators))
    expect_true(is.data.frame(result$data$metabolite1))
    expect_true(is.data.frame(result$data$metabolite2))
    expect_true(is.list(result$convex_hulls$metabolite1))
    expect_true(is.list(result$convex_hulls$metabolite2))
    expect_true(is.data.frame(result$convex_hulls$metabolite1$A))
    expect_true(is.data.frame(result$convex_hulls$metabolite1$B))
    expect_true(is.data.frame(result$convex_hulls$metabolite2$A))
    expect_true(is.data.frame(result$convex_hulls$metabolite2$B))
  }
)

test_that(
  "convex_analysis_of_variables fails with unvalid input",
  {
    set.seed(101917)
    data <- data.frame(
      batch = rep(c("A", "B", "C"), each = 10),
      injectionOrder = rep(1:30, times = 1),
      metabolite1 = round(rnorm(30, mean = 100, sd = 10),3),
      metabolite2 = round(rnorm(30, mean = 200, sd = 20), 3)
    )
    data_list <- list(
      batch = rep(c("A", "B", "C"), each = 10),
      injectionOrder = rep(1:30, times = 1),
      metabolite1 = round(rnorm(30, mean = 100, sd = 10),3),
      metabolite2 = round(rnorm(30, mean = 200, sd = 20), 3)
    )
    data_duplicate_order <- data.frame(
      batch = rep(c("A", "B", "C"), each = 10),
      injectionOrder = rep(1:10, times = 3),
      metabolite1 = round(rnorm(30, mean = 100, sd = 10),3),
      metabolite2 = round(rnorm(30, mean = 200, sd = 20), 3)
    )
    expect_error(
      convex_analysis_of_variables(
        data = data_list,
        variable_columns = c("metabolite1", "metabolite2"),
        batch_col = "batch",
        sample_order_col = "injectionOrder",
        mode = "global"
      )
    )
    expect_error(
      convex_analysis_of_variables(
        data = data_list,
        variable_columns = c("metabolite1", "metabolite2"),
        batch_col = "batch",
        sample_order_col = "injectionOrder",
        mode = "bobby"
      )
    )
    expect_error(
      convex_analysis_of_variables(
        data = data_list,
        variable_columns = c("metabolite1", "metabolite2"),
        batch_col = "batch",
        sample_order_col = "ordre",
        mode = "global"
      )
    )
    expect_error(
      convex_analysis_of_variables(
        data = data_list,
        variable_columns = c("metabolite1", "metabolite2"),
        batch_col = "group",
        sample_order_col = "injectionOrder",
        mode = "global"
      )
    )
    expect_error(
      convex_analysis_of_variables(
        data = data_list,
        variable_columns = c("metaboliteA", "metaboliteB"),
        batch_col = "batch",
        sample_order_col = "injectionOrder",
        mode = "global"
      )
    )
  }
)

test_that("plot_convex_hull works with valid input",
  {
    library(ggplot2)
    data <- data.frame(
      batch = rep(c("A", "B", "C"), each = 10),
      injectionOrder = rep(1:30, times = 1),
      metabolite1 = round(rnorm(30, mean = 100, sd = 10),3),
      metabolite2 = round(rnorm(30, mean = 200, sd = 20), 3)
    )
    result <- convex_analysis_of_variables(
      data = data,
      variable_columns = c("metabolite1", "metabolite2"),
      batch_col = "batch",
      sample_order_col = "injectionOrder",
      mode = "global"
    )
    plot_result <- plot_convex_hull(
      result$data$metabolite1,
      result$convex_hulls$metabolite1,
      var_name = "metabolite1",
      show_points = TRUE,
      label_prefix = "prefix",
      indicators = result$indicators[
        result$indicators$Variable == "metabolite1",
      ]
    )
    expect_true(is_ggplot(plot_result))
  }
)

test_that("plot_convex_hull fails with wrong input",
  {
    set.seed(101917)
    data <- data.frame(
      batch = rep(c("A", "B", "C"), each = 10),
      injectionOrder = rep(1:30, times = 1),
      metabolite1 = round(rnorm(30, mean = 100, sd = 10),3),
      metabolite2 = round(rnorm(30, mean = 200, sd = 20), 3)
    )
    result <- convex_analysis_of_variables(
      data = data,
      variable_columns = c("metabolite1", "metabolite2"),
      batch_col = "batch",
      sample_order_col = "injectionOrder",
      mode = "global"
    )
    expect_error(plot_result <- plot_convex_hull(
      result$data$metabolite1,
      result$convex_hulls$metabolite1,
      var_name = "metabolite1",
      show_points = "TRUE",
      label_prefix = "prefix",
      indicators = result$indicators[
        result$indicators$Variable == "metabolite1",
      ]
    ))
    indicators <- result$indicators[
      result$indicators$Variable == "metabolite1",
    ]
    colnames(indicators) <- c("Variable", "interb", "intrab", "ratio")
    expect_error(plot_result <- plot_convex_hull(
      result$data$metabolite1,
      result$convex_hulls$metabolite1,
      var_name = "metabolite1",
      show_points = TRUE,
      label_prefix = "prefix",
      indicators = indicators
    ))
  }
)

test_that("plot_all_convex_hulls works",
  {
    library(pdftools)
    library(cli)
    set.seed(101917)
    data <- data.frame(
      batch = rep(c("A", "B", "C"), each = 10),
      injectionOrder = rep(1:30, times = 1),
      metabolite1 = round(rnorm(30, mean = 100, sd = 10),3),
      metabolite2 = round(rnorm(30, mean = 200, sd = 20), 3)
    )
    result_batch <- convex_analysis_of_variables(
      data = data,
      variable_columns = c("metabolite1", "metabolite2"),
      batch_col = "batch",
      sample_order_col = "injectionOrder",
      mode = "batchwise"
    )
    result_global <- convex_analysis_of_variables(
      data = data,
      variable_columns = c("metabolite1", "metabolite2"),
      batch_col = "batch",
      sample_order_col = "injectionOrder",
      mode = "global"
    )

    plot_all_convex_hulls(
      file.path(tmp_dir, "plot_all_test_batchwise.pdf"),
      result_batch,
      TRUE,
      "batchwise"
    )
    plot_all_convex_hulls(
      file.path(tmp_dir, "plot_all_test_global.pdf"),
      result_global,
      TRUE,
      "global"
    )
    bitmap_batch <- pdf_render_page(file.path(tmp_dir,"plot_all_test_batchwise.pdf"))
    bitmap_global <- pdf_render_page(file.path(tmp_dir,"plot_all_test_global.pdf"))
    switch(.Platform$OS.type,
      "windows" = {
        expect_equal(hash_raw_md5(bitmap_batch), "61fc229d56539ee16663167c9d5fc3f7")
        expect_equal(hash_raw_md5(bitmap_global), "e90fd69b670212d8170dbfdcd77e36c1")
      },
      "unix" = {
        expect_equal(hash_raw_md5(bitmap_batch), "2a1d8576a0ea9af63d0fa996b2194542")
        expect_equal(hash_raw_md5(bitmap_global), "a51b98fb0ff70313f01658453907afa0")
      }
    )

  }
)

# remove pdf file created by tests
pdf_files <- list.files(
   path = tmp_dir,
   pattern = "*.pdf$",
   full.names = TRUE
)
file.remove(pdf_files)