#' get_covid_model_data_both_age_bandings
#'
#' @description - Joins dispensing days to raw data, adds columns for 5 year and 20 years age bands
#'
#' @param data - data that contains the required columns for MUMH
#'
#' @return  - data frame for creating covid model
#' @export
#'
#' @examples


get_covid_model_data_both_age_bandings <- function(data){

  data |>
    dplyr::mutate(BAND_20YR = dplyr::case_when(DALL_5YR_BAND %in% c("00-04", "05-09", "10-14", "15-19") ~ "00-19",
                                               DALL_5YR_BAND %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
                                               DALL_5YR_BAND %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
                                               DALL_5YR_BAND %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
                                               DALL_5YR_BAND == "Unknown" ~ "Unknown",
                                               TRUE ~ "80+")) |>
    dplyr::group_by(YEAR_MONTH,
                    SECTION_NAME,
                    SECTION_CODE,
                    IDENTIFIED_FLAG,
                    PDS_GENDER,
                    DALL_5YR_BAND,
                    BAND_20YR) |>
    dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT),
                     ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC)) |>
    dplyr::group_by(SECTION_NAME, SECTION_CODE, IDENTIFIED_FLAG,
                    PDS_GENDER, BAND_20YR, DALL_5YR_BAND) |>
    dplyr::mutate(
      MONTH_START = as.Date(paste0(YEAR_MONTH, "01"), format = "%Y%m%d"),
      MONTH_NUM = lubridate::month(MONTH_START),
      MONTH_INDEX = lubridate::interval(lubridate::dmy(01032015), as.Date(MONTH_START)) %/% months(1)
    ) |>
    dplyr::left_join(dispensing_days,
                     by = "YEAR_MONTH") |>
    dplyr::filter(!(IDENTIFIED_FLAG == "N" & PDS_GENDER == "F"),
                  !(IDENTIFIED_FLAG == "N" & PDS_GENDER == "M"),
                  !(PDS_GENDER == "U" | DALL_5YR_BAND == "Unknown" | BAND_20YR == "Unknown")) |>
    dplyr::ungroup()

}
