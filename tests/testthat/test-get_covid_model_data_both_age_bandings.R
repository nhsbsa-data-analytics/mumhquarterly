test_that("function outputs a dataframe", {

  covid_model_testing_raw_data <- openxlsx::read.xlsx("C:/Users/ASEMP/Desktop/file8ec2cf5843.xlsx", sheet = 1)

  Covid_model_testing_data_processed <- get_covid_model_data_both_age_bandings(covid_model_testing_raw_data)

  expect_true(is.data.frame(Covid_model_testing_data_processed))
})


test_that("function outputs has expected value in first cell of first column", {

  covid_model_testing_raw_data <- openxlsx::read.xlsx("C:/Users/ASEMP/Desktop/file8ec2cf5843.xlsx", sheet = 1)

  Covid_model_testing_data_processed <- get_covid_model_data_both_age_bandings(covid_model_testing_raw_data)

  expect_equal(Covid_model_testing_data_processed[1,1],201504)
})


test_that("Correct number of columns in outputs", {

  covid_model_testing_raw_data <- openxlsx::read.xlsx("C:/Users/ASEMP/Desktop/file8ec2cf5843.xlsx", sheet = 1)

  Covid_model_testing_data_processed <- get_covid_model_data_both_age_bandings(covid_model_testing_raw_data)

  expect_equal(ncol(Covid_model_testing_data_processed), 13)
})
