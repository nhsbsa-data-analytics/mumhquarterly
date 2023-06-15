test_that("function outputs a dataframe", {

  covid_model_testing_raw_data <- openxlsx::read.xlsx("C:/Users/ASEMP/Desktop/file8ec2cf5843.xlsx", sheet = 1)

  Covid_model_testing_data_processed <- agebanded_covid_model_data(covid_model_testing_raw_data,
                                              five_year = TRUE)

  expect_true(is.data.frame(Covid_model_testing_data_processed))
})


test_that("function outputs has expected value in first cell of first column", {

  covid_model_testing_raw_data <- openxlsx::read.xlsx("C:/Users/ASEMP/Desktop/file8ec2cf5843.xlsx", sheet = 1)

  Covid_model_testing_data_processed <- agebanded_covid_model_data(covid_model_testing_raw_data,
                                                                   five_year = TRUE)

  covid_data_first_column_name <- Covid_model_testing_data_processed[1,1]

  expect_identical(covid_data_first_column_name,"2015/2016")
})


test_that("Correct number of columns in outputs", {

  covid_model_testing_raw_data <- openxlsx::read.xlsx("C:/Users/ASEMP/Desktop/file8ec2cf5843.xlsx", sheet = 1)

  Covid_model_testing_data_processed <- agebanded_covid_model_data(covid_model_testing_raw_data,
                                                                   five_year = TRUE)

  expect_equal(ncol(Covid_model_testing_data_processed), 23)
})
