# R package `dispersionIndicators`
## Convex hull plot, convex and ICM dispersion indicators

### 1. Description
This package provides function for analysing the dispersion of values from a 
batched and ordered dataset.

It can generate convex hull plots for each batch of a given dataset, along with
computing convex and ICM (Integrated Covariance Mahalanobis) dispersion
indicators.

The convex hull of a set of multidimensional points is the smallest convex
polygon that encloses all its points.

The convex indicators are based on convex hulls while  ICM dispersion indicators
are rather based on mahalanobis distance; All these provide insights into the
distribution of the data points and their dispersion among and between batches.

These indicators where developped in the context of metabolomics data during the 
thesis of Elfried Salanon [[1]](#1) that ended in 2025 (soon to be published). The convex
indicators are extensively described in [[2]](#2) while the ICM indicators are
detailed in [[3]](#3).
The main functions in this package are:
- `compute_icm_distances(...)`: Computes ICM 
    dispersion indicators.
- `convex_analysis_of_variables(...)`: Performs a comprehensive analysis by
    computing convex hull and all convex indicators for each
    variable (columns) in the dataset.
- `plot_all_convex_hulls(...)`: Generates convex hull plots
    for each batch in the dataset.

### 2. Prerequisites
R (version 4.1 or higher) is required to run this package.

The package requires the following R packages to be installed:
```R
install.packages("ggplot2")
install.packages("stats")
install.packages("corpcor")
install.packages("utils")
````
### 3. Installation
You can install the package from CRAN using the following command:
```R
install.packages("dispersionIndicators")
```

### 4. Usage
Load the package:
```R
library(dispersionIndicators)
```
Prepare your data:
```R
data <- data.frame(
batch = rep(c("A","B","C"), each = 10),
injectionOrder = rep(1:30, times = 1),
metabolite1 = rnorm(30, mean = 100, sd = 10),
metabolite2 = rnorm(30, mean = 200, sd = 20)
)
```
Run the analysis:
```
result <- convex_analysis_of_variables(
  data = data,
  variable_columns = c("metabolite1", "metabolite2"),
  batch_col = "batch",
  sample_order_col = "injectionOrder",
  impute_if_needed = "median",
  mode = "global"
)
```
Generate and save the convex hull plots:
```
plot_all_convex_hulls(
  target_file_path = "../convex_hulls.pdf",
  convex_analysis_res = result,
  show_points = TRUE,
  mode = "global"
)
```
Compute ICM dispersion indicators:
```
result <- compute_icm_distances(
  data,
  batch_col = "batch",
  mode = "all",
  center_method_individual = "batch",
  center_method_inter = "mean"
)
```

## References
<a id="1">[1]</a> **Salanon, Elfried M.B. (2025)**.
*Reproducibility and multi-source integration  in metabolomics for the
identification of common phenotypes.*

<a id="2">[2]</a> **Salanon, Elfried M.B. & al. (2024)**.
*An alternative for the robust assessment of the repeatability and
reproducibility of analytical measurements using bivariate dispersion.*
Chemometrics and Intelligent Laboratory Systems, 250, 105148.

<a id="3">[3]</a> **Salanon, Elfried M.B. & al. (2025)**.
*A benchmarking workflow for assessing the reliability of batch correction methods*

