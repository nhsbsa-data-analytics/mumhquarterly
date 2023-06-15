test_that("month_pred_fun returns a data frame", {

  lmObject <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

  month <- 3
  iris_month <- iris[1:5, ]

  iris_month$YEAR_MONTH <- 3

  result <- month_pred_fun(month = month, data = iris_month, model = lmObject)

  expect_s3_class(result, "data.frame")

})

test_that("month_pred_fun returns a data frame with the correct column names", {

  lmObject <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

  month <- 3
  iris_month <- iris[1:5, ]

  iris_month$YEAR_MONTH <- 3

  result <- month_pred_fun(month = month, data = iris_month, model = lmObject)

  expected_col_names <- c("YEAR_MONTH", "mean_fit", "var", "PIlwr", "PIupr", "PIlwr99", "PIupr99")

  actual_col_names <- colnames(result)

  expect_identical(actual_col_names, expected_col_names)

})

test_that("month_pred_fun returns a dataframe correct number of collumns", {

  lmObject <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

  month <- 3
  iris_month <- iris[1:5, ]

  iris_month$YEAR_MONTH <- 3

  result_col_number <- month_pred_fun(month = month, data = iris_month, model = lmObject) |>
    ncol()

  expect_equal(result_col_number, 7)

})
