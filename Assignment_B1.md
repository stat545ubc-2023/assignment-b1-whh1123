Assignment_B1
================
Han Wang
2023-11-01

``` r
library(datateachr) 
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## Exercise 1: Make a Function (25 points) & Exercise 2: Document the Function

In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.

In the same code chunk where you made your function, document the
function using roxygen2 tags.

In my previous MDA, I analyzed the cancer_sample including grouping by
diagnosis and then summarizing various statistics for columns like
radius_mean and symmetry_mean. This is a repeated action and thus can be
encapsulated in a function below.

``` r
#' @title Summarize the statistics of a given variable grouped by another variable
#' @details This function computes summary statistics for a specified column grouped by another column.
#' @param data A dataframe containing the data
#' @param group_var The variable to group by (as a symbol)
#' @param stat_var The variable on which to compute the statistics (as a symbol)
#'
#' @return A dataframe with min, max, mean, median, standard deviation, and count for the specified variable, grouped by the given grouping variable.
summarize_stats <- function(data, group_var, stat_var){
  if (nrow(data) == 0) {
    return(data.frame())
  }
  data %>%
    group_by({{group_var}}) %>%
    summarise(
      min=min({{stat_var}}, na.rm = TRUE),
      max=max({{stat_var}}, na.rm = TRUE),
      mean=mean({{stat_var}}, na.rm = TRUE),
      median=median({{stat_var}}, na.rm = TRUE),
      sd=sd({{stat_var}}, na.rm = TRUE),
      n=n()
    )
}
```

## Exercise 3: Include examples

``` r
# Example 1: Computing statistics for radius_mean grouped by diagnosis
# This example demonstrates how to use the function to compute the summary statistics
# for the `radius_mean` column, grouping the data by the `diagnosis` column. 
# The resulting dataframe will contain the minimum, maximum, average, median, 
# standard deviation and count of `radius_mean` values for each unique `diagnosis`.
summarized_radius_mean <- summarize_stats(cancer_sample, diagnosis, radius_mean)
print(summarized_radius_mean)
```

    ## # A tibble: 2 × 7
    ##   diagnosis   min   max  mean median    sd     n
    ##   <chr>     <dbl> <dbl> <dbl>  <dbl> <dbl> <int>
    ## 1 B          6.98  17.8  12.1   12.2  1.78   357
    ## 2 M         11.0   28.1  17.5   17.3  3.20   212

``` r
# Example 2: Computing statistics for symmetry_mean grouped by diagnosis
# This example demonstrates the use of the function to compute 
# summary statistics for the `symmetry_mean` column, again grouping by `diagnosis`.
# The results will show the summarized statistics for `symmetry_mean` based on each 
# unique value in the `diagnosis` column.
summarized_symmetry_mean <- summarize_stats(cancer_sample, diagnosis, symmetry_mean)
print(summarized_symmetry_mean)
```

    ## # A tibble: 2 × 7
    ##   diagnosis   min   max  mean median     sd     n
    ##   <chr>     <dbl> <dbl> <dbl>  <dbl>  <dbl> <int>
    ## 1 B         0.106 0.274 0.174  0.171 0.0248   357
    ## 2 M         0.131 0.304 0.193  0.190 0.0276   212

``` r
# Example 3: Computing statistics for texture_mean grouped by diagnosis
# This example computes the summary statistics for the `texture_mean` column.
# The cancer_sample is grouped by `diagnosis` to see how the texture_mean values differ 
# across different diagnoses.
summarized_texture_mean <- summarize_stats(cancer_sample, diagnosis, texture_mean)
print(summarized_texture_mean)
```

    ## # A tibble: 2 × 7
    ##   diagnosis   min   max  mean median    sd     n
    ##   <chr>     <dbl> <dbl> <dbl>  <dbl> <dbl> <int>
    ## 1 B          9.71  33.8  17.9   17.4  4.00   357
    ## 2 M         10.4   39.3  21.6   21.5  3.78   212

``` r
# Example 4: Computing statistics for compactness_mean grouped by diagnosis
# This example computes the summary statistics for the `compactness_mean` column,
# grouping the data by the `diagnosis` column. It allows us to analyze how the 
# compactness mean values are distributed based on the diagnosis of the cancer.
summarized_compactness_mean <- summarize_stats(cancer_sample, diagnosis, compactness_mean)
print(summarized_compactness_mean)
```

    ## # A tibble: 2 × 7
    ##   diagnosis    min   max   mean median     sd     n
    ##   <chr>      <dbl> <dbl>  <dbl>  <dbl>  <dbl> <int>
    ## 1 B         0.0194 0.224 0.0801 0.0753 0.0337   357
    ## 2 M         0.0460 0.345 0.145  0.132  0.0540   212

``` r
# Example 5: Computing statistics for smoothness_mean grouped by diagnosis
# For the `smoothness_mean` column, it is grouped by `diagnosis`, giving 
# an insight into the distribution of smoothness mean values across different cancer diagnoses.
summarized_smoothness_mean <- summarize_stats(cancer_sample, diagnosis, smoothness_mean)
print(summarized_smoothness_mean)
```

    ## # A tibble: 2 × 7
    ##   diagnosis    min   max   mean median     sd     n
    ##   <chr>      <dbl> <dbl>  <dbl>  <dbl>  <dbl> <int>
    ## 1 B         0.0526 0.163 0.0925 0.0908 0.0134   357
    ## 2 M         0.0737 0.145 0.103  0.102  0.0126   212

## Exercise 4: Test the Function

### Setup

``` r
library(dplyr)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

### Generating a Testing Dataset

This dataset has 4 types of variables: 1. A grouping variable group with
three levels: a, b, and c. 2. A numeric variable value with some NAs. 3.
An integer variable value_int. 4. A character variable value_char.

``` r
# Create a sample dataset
set.seed(123) # for reproducibility

test_data <- data.frame(
  group = sample(letters[1:3], 100, replace = TRUE),
  value = rnorm(100),
  value_int = sample(1:10, 100, replace = TRUE),
  value_char = sample(letters[4:6], 100, replace = TRUE)
)

# Introduce some NA's
test_data[sample(1:100, 10), "value"] <- NA
```

### Testing

``` r
# Test the function
test_that("Test summarize_stats function", {

  # 1. Vector with no NA's
  result_1 <- summarize_stats(test_data, group, value_int)
  expect_true(!any(is.na(result_1$mean)))

  # 2. Vector that has NA's
  result_2 <- summarize_stats(test_data, group, value)
  expect_true(any(is.na(test_data$value)))
  expect_true(!any(is.na(result_2$mean)))

  # 3. Vector of a different type
  # Note: Summarizing a character vector will throw an error in this context.
  # So, we are expecting an error in this case.
  expect_error(summarize_stats(test_data, group, value_char))
  expect_error(summarize_stats(test_data, group, cur_date))

  # 4. Vector of length 0
  empty_data <- test_data[0, ]
  result_4 <- summarize_stats(empty_data, group, value)
  expect_equal(nrow(result_4), 0)
})
```

    ## Test passed 🎉
