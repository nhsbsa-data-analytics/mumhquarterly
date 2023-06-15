test_that("data frame retured ", {

  df20_train <- openxlsx::read.xlsx(file = "C:\\Users\\ASEMP\\R\\pchcR\\R\\mumhquarterly\\df20_train.xlsx" )

  test_lm_data <- covid_lm_with_months(df20_train, "0401")

  expect_s3_class(test_lm_data,"lm")

})

test_that("data frame returned when lm is subject to broom::glance", {

  df20_train <- openxlsx::read.xlsx(file = "C:\\Users\\ASEMP\\R\\pchcR\\R\\mumhquarterly\\df20_train.xlsx" )

  test_lm_data <- covid_lm_with_months(df20_train, "0401")

  function_testing_data <- broom::glance(test_lm_data)

  expect_s3_class(function_testing_data,"data.frame")

})


