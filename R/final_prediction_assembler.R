#' final_prediction_assembler
#'
#' @param input_data
#' @param section_code
#' @param month_pred_function
#' @param pred_month_list
#' @param covid_lm_data_with_months
#'
#' @return
#' @export
#'
#' @examples

final_prediction_assembler <- function(input_data,
                                       section_code,
                                       month_pred_function,
                                       pred_month_list,
                                       covid_lm_data_with_months){

## 0401 Hypnotics and Anxiolytics item prediction
dataframe_of_section_code <- input_data |>
  dplyr::filter(SECTION_CODE == section_code)

#default PI of 95%
pred_0401 <- lapply(pred_month_list,
                    month_pred_function,
                    data = dataframe_of_section_code,
                    model = covid_lm_data_with_months)

unlist(pred_0401)

rbindlist(pred_0401)

df_0401_sum <- df_0401 |>
  dplyr::group_by(YEAR_MONTH, SECTION_CODE) |>
  dplyr::summarise(total_items = sum(ITEM_COUNT)) |>
  left_join(rbindlist(pred_0401)) |>
  dplyr::mutate(YEAR_MONTH_string = as.character(YEAR_MONTH)) |>
  ungroup()
}
