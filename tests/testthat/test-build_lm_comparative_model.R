test_that("data frame retured ", {

both_agebandings_data_from_test_xl <- openxlsx::read.xlsx("C:\\Users\\ASEMP\\R\\pchcR\\R\\mumhquarterly\\train_both_from_xl.xlsx", sheet = 1)

test_lm_data <- build_lm_comparative_model(both_agebandings_data_from_test_xl, "0401", five_year = TRUE)

expect_s3_class(test_lm_data,"lm")

})

test_that("data frame returned when lm is subject to broom::glance", {

  both_agebandings_data_from_test_xl <- openxlsx::read.xlsx("C:\\Users\\ASEMP\\R\\pchcR\\R\\mumhquarterly\\train_both_from_xl.xlsx", sheet = 1)

  test_lm_data <- build_lm_comparative_model(both_agebandings_data_from_test_xl, "0401", five_year = TRUE)

  function_testing_data <- broom::glance(test_lm_data)

  expect_s3_class(function_testing_data,"lm")

})


test_that("data frame returned when lm is subject to broom::glance", {

  both_agebandings_data_from_test_xl <- openxlsx::read.xlsx("C:\\Users\\ASEMP\\R\\pchcR\\R\\mumhquarterly\\train_both_from_xl.xlsx", sheet = 1)

  test_lm_data <- build_lm_comparative_model(both_agebandings_data_from_test_xl, "0401", five_year = TRUE)

  function_testing_data <- broom::glance(test_lm_data)

  expect_s3_class(function_testing_data,"lm")

})

test_that("data frame contains correct result when lm is subject to broom::glance", {

  both_agebandings_data_from_test_xl <- openxlsx::read.xlsx("C:\\Users\\ASEMP\\R\\pchcR\\R\\mumhquarterly\\train_both_from_xl.xlsx", sheet = 1)

  test_lm_data <- build_lm_comparative_model(both_agebandings_data_from_test_xl, "0401", five_year = TRUE)

  function_testing_data <- broom::glance(test_lm_data)

  value_output_test_figure <-  signif(function_testing_data[1,1], digits =3) |>
    as.double()

  expect_equal(value_output_test_figure, 0.992)
})
