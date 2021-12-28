set.seed(123)

data_df <- head(NearRepeat::chicago_be, 1000)
data_tb <- tibble::as_tibble(data_df)
data_sf <- sf::st_as_sf(data_tb, coords = c("X", "Y"), crs = 26971)

# Since the simulations take some time, store the result of the function with
# arguments that should not produce any errors, warnings or messages
result <- tidy_near_repeat(
  data = data_sf,
  sds = c(0, 1000, 10000),
  tds = c(0, 7, 14, 21, 28)
)

# Create object that mimics the expected result from the above function call
# This code was created using `dput(result)`
expected_result <- structure(
  list(
    distance = structure(
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      .Label = c("[0,1000)", "[1000,10000)"),
      class = c("ordered", "factor")
    ),
    time = structure(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
      .Label = c("[0,7)", "[7,14)", "[14,21)", "[21,28)"),
      class = c("ordered", "factor")
    ),
    observed = c(1318, 52038, 1019, 42573, 646, 28259, 332, 13680),
    knox_ratio = c(
      1.04351320121289, 0.996668407281271, 0.994673803437845, 1.00443787988413,
      0.944299341108921, 1.00004697228051, 1.0009778357236, 1.00036709523724
    ),
    knox_ratio_median = c(
      1.04354711005542, 0.996743794054551, 0.994146341463415, 1.00417492216247,
      0.944444444444444, 1.0002123668283, 1.00302114803625, 1.0010244402166
    ),
    pvalues = c(0.046, 0.767, 0.59, 0.248, 0.955, 0.488, 0.483, 0.482)
  ),
  row.names = c(NA, -8L),
  class = c("tbl_df", "tbl", "data.frame")
)



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("function produces an error if data is not a data frame, tibble or SF object containing points", {
  expect_error(tidy_near_repeat(data = data_df$X))
  expect_error(tidy_near_repeat(data = sf::st_cast(data_sf, "LINESTRING")))
})

test_that("function produces an error if x and y are NULL but data is not an SF object", {
  expect_error(tidy_near_repeat(data = data_df))
})

test_that("function produces an error if only one of x and y is specified", {
  expect_error(tidy_near_repeat(data_df, x = X))
  expect_error(tidy_near_repeat(data_df, y = Y))
})

test_that("function produces an error if x, y or time columns are not found in data", {
  expect_error(tidy_near_repeat(data_df, x = does_not_exist, y = does_not_exist, time = date))
  expect_error(tidy_near_repeat(data_df, x = does_not_exist, y = Y, time = date))
  expect_error(tidy_near_repeat(data_df, x = X, y = does_not_exist))
  expect_error(tidy_near_repeat(data_df, x = X, y = Y, time = does_not_exist))
})

data_df_no_time <- data_df
data_df_no_time$date <- as.character(data_df_no_time$date)
test_that("function produces an error if time argument is NULL and there is no date column present", {
  expect_error(tidy_near_repeat(data_df_no_time))
})

test_that("function produces error if specified time column does not contain dates", {
  expect_error(tidy_near_repeat(data_df_no_time, time = date))
})

test_that("function produces an error if time argument is NULL and multiple columns are dates", {
  data_df_two_dates <- data_df
  data_df_two_dates$date2 <- data_df_two_dates$date
  expect_error(tidy_near_repeat(data_df_two_dates))
})

test_that("function produces an error if the sds or tds arguments are not numeric or NULL", {
  expect_error(tidy_near_repeat(data_df, x = X, y = Y, time = date, sds = LETTERS[1:3]))
  expect_error(tidy_near_repeat(data_df, x = X, y = Y, time = date, tds = LETTERS[1:3]))
})

test_that("function produces an error if the sds or tds arguments are numeric vectors with fewer than two unique values", {
  expect_error(tidy_near_repeat(data_df, sds = c(1, 1)))
  expect_error(tidy_near_repeat(data_df, tds = c(1, 1)))
})


## Warnings ----

test_that("function produces a warning if SF co-ordinates are specified using lon/lat", {
  expect_warning(tidy_near_repeat(sf::st_transform(head(data_sf, n = 500), 4326), sds = c(0, 100), tds = c(0, 7, 28)))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces a tibble", {
  expect_s3_class(result, "tbl_df")
})

test_that("output tibble has the required column names", {
  expect_equal(
    names(result),
    c("distance", "time", "observed", "knox_ratio", "knox_ratio_median", "pvalues")
  )
})

test_that("columns in output have the required types", {
  expect_s3_class(result$distance, "ordered")
  expect_s3_class(result$time, "ordered")
  expect_type(result$observed, "double")
  expect_type(result$knox_ratio, "double")
  expect_type(result$knox_ratio_median, "double")
  expect_type(result$pvalues, "double")
})

test_that("there are the correct number of rows in the output", {
  # nrows = (length(sds) - 1) * (length(tds) - 1) = (3 - 1) * (5 - 1) = 8 with
  # the argument values specified at the top of this test file
  expect_equal(nrow(result), 8)
})

test_that("output is as expected", {
  expect_equal(result, expected_result)
})



## Warnings ----

test_that("function produces a warning if there are significant near repeats at the maximum distance specified", {
  # This example sets a very small maximum distance, while also specifying
  # values of `tds` to avoid a zero-count message because of the low number of
  # rows of data
  expect_warning(tidy_near_repeat(head(data_sf, n = 500), sds = c(0, 100), tds = c(0, 7, 28)))
})

test_that("function produces a warning if there are significant near repeats at the maximum time period specified", {
  # This example sets a very small maximum time period
  expect_warning(tidy_near_repeat(head(data_sf, n = 500), tds = c(0, 1)))
})


## Messages ----

test_that("function produces a message if some Knox ratios could not be calculated because of zero event counts", {
  # In this example there few enough rows that some combinations of distance and
  # time have zero counts and the corresponding Knox ratios will be NaN and a
  # message should be produced
  expect_message(tidy_near_repeat(head(data_sf, n = 100)))
})
