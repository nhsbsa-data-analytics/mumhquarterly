#' Covid model function
#'
#' This function creates a linear model using the lm() function to predict number
#' of items and patients for BNF chapters specified in input data. The Covid-19
#' period is defined as February 2020 onwards.
#' Prediction intervals are set at 90%, 95% and 99%
#'
#' @param data The data to be input into the model
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#'
#' @export
#'
#' @examples
#' covid_model(data)

covid_model <- function(data) {

  #group input data by BNF section and code, create columns for prediction intervals
  #of items and patients
  df <- data %>%
    dplyr::group_by(SECTION_NAME,SECTION_CODE) %>%
    tidyr::nest %>%
    dplyr::mutate(
      ITEM_MODEL = purrr::map(
        data,
        function(df) {
          lm(ITEM_COUNT ~ MONTH_INDEX + DISPENSING_DAYS + as.factor(MONTH_NUM),
             data = filter(df, YEAR_MONTH <= 202002))
        }
      ),
      PATIENT_MODEL = purrr::map(
        data,
        function(df) {
          lm(PATIENT_COUNT ~ MONTH_INDEX + DISPENSING_DAYS + as.factor(MONTH_NUM),
             data = filter(df, YEAR_MONTH <= 202002))
        }
      ),
      PRED_ITEMS_95 = purrr::map2(ITEM_MODEL, data,
                                  ~ as.data.frame(predict(.x, .y, interval = "prediction"))),
      PRED_PATIENTS_95 = purrr::map2(PATIENT_MODEL, data,
                                     ~ as.data.frame(predict(.x, .y, interval = "prediction"))),
      PRED_ITEMS_90 = purrr::map2(ITEM_MODEL, data,
                                  ~ as.data.frame(predict(.x, .y, interval = "prediction", level = 0.9))),
      PRED_PATIENTS_90 = purrr::map2(PATIENT_MODEL, data,
                                     ~ as.data.frame(predict(.x, .y, interval = "prediction", level = 0.9))),
      PRED_ITEMS_99 = purrr::map2(ITEM_MODEL, data,
                                  ~ as.data.frame(predict(.x, .y, interval = "prediction", level = 0.99))),
      PRED_PATIENTS_99 = purrr::map2(PATIENT_MODEL, data,
                                     ~ as.data.frame(predict(.x, .y, interval = "prediction", level = 0.99))),
    ) %>%
    tidyr::unnest(cols = c(data, PRED_ITEMS_95, PRED_PATIENTS_95, PRED_ITEMS_90, PRED_PATIENTS_90, PRED_ITEMS_99, PRED_PATIENTS_99),
                  names_repair = "universal",
                  names_sep = "_") %>%
    dplyr::select(-ends_with("_MODEL")) %>%
    dplyr::rename_with(.fn = ~ toupper(gsub("data_","", .x)))

  return(df)

}
