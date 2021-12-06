test_that("%!in% subsets correctly", {
  x <- c(1:10)
  y <- c(5:9)
  z <- as.integer(c(1:4,10))

  expect_identical(x[x %!in% y], z)
})

test_that("%!in% subsets in the same way as negation", {
  x <- c(1:10)
  y <- c(5:9)

  expect_identical(x[x %!in% y], subset(x, !(x %in% y)))
})
