test_that("fast_agg_pred throws an error if lmObject is not a valid 'lm' object", {

  # Generate sample data
  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 2*x1 - 3*x2 + rnorm(n)
  data <- data.frame(x1 = x1, x2 = x2, y = y)

  # Fit a linear regression model
  lmObject <- lm(y ~ x1 + x2, data = data)

  # Test case: Invalid lmObject argument
  iris_lm <- lm(Sepal.Length ~ Sepal.Width, data = iris)  # Fit a different linear regression model

  # Expect an error to be thrown when calling fast_agg_pred with an invalid lmObject
  expect_error(fast_agg_pred(w = c(0.5, 0.5), lmObject = iris_lm, newdata = data))

})

test_that("fast_agg_pred returns a list ", {

  # Generate sample data
  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 2*x1 - 3*x2 + rnorm(n)
  data <- data.frame(x1 = x1, x2 = x2, y = y)

  # Fit a linear regression model
  lmObject <- lm(y ~ x1 + x2, data = data)

  # Test case: Check the output structure and dimensions
  newdata <- data.frame(x1 = c(0.5, -1), x2 = c(-2, 1))
  result <- fast_agg_pred(w = c(0.5, 0.5), lmObject = lmObject, newdata = newdata)

  # Expect the result to be a list
  expect_type(result, "list")

})

test_that("fast_agg_pred returns a list with the expected number of elements", {

  # Generate sample data
  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 2*x1 - 3*x2 + rnorm(n)
  data <- data.frame(x1 = x1, x2 = x2, y = y)

  # Fit a linear regression model
  lmObject <- lm(y ~ x1 + x2, data = data)

  # Test case: Check the output structure and dimensions
  newdata <- data.frame(x1 = c(0.5, -1), x2 = c(-2, 1))
  result <- fast_agg_pred(w = c(0.5, 0.5), lmObject = lmObject, newdata = newdata)


  # Expect the result to have the expected number of elements
  expect_equal(length(result), 4)

})
