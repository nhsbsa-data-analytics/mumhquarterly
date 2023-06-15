#' agebanded_covid_model_data
#'
#' @description - Joins dispensing days to raw data, adds columns for position of month in year, position of month in full dataset, and month start date.
#'
#' @param data - data that contains the required columns for MUMH
#' @param five_year - option to choose 5 year banding
#' @param twenty_year - option to choose 20 year banding
#'
#' @return  - data frame for creating covid model
#' @export
#'
#' @examples

single_ageband_covid_model_data <-function(data,
                                  five_year = FALSE,
                                  twenty_year = FALSE){


if (five_year == TRUE & twenty_year == FALSE ){

  agebanding <- "DALL_5YR_BAND"

}
  else if(five_year == FALSE & twenty_year == TRUE){

  agebanding <- "BAND_20YR"

  agebanding_data <- agebanding_data |>
    dplyr::mutate(BAND_20YR = dplyr::case_when(DALL_5YR_BAND %in% c("00-04", "05-09", "10-14", "15-19") ~ "00-19",
                                               DALL_5YR_BAND %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
                                               DALL_5YR_BAND %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
                                               DALL_5YR_BAND %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
                                               DALL_5YR_BAND == "Unknown" ~ "Unknown",
                                               TRUE ~ "80+")) |>
    dplyr::select(!(DALL_5YR_BAND))



}
  else if(five_year == TRUE & twenty_year == TRUE){


    stop("Please set to true either 5 year agebands, 20 year ageband or both as function can not be TRUE for both")


}
  else if(five_year == FALSE & twenty_year == FALSE){

  stop("Please set to true either 5 year agebands, 20 year ageband or both as function can not be FALSE for both")
}

  data |>
  dplyr::group_by(YEAR_MONTH,
                  SECTION_NAME,
                  SECTION_CODE,
                  IDENTIFIED_FLAG,
                  PDS_GENDER,
                  glue::glue_collapse(agebanding)) |>
  dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT),
                   ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC),
                   .groups = "drop") |>
  tidyr::complete( glue::glue_collapse(agebanding),
                  tidyr::nesting(YEAR_MONTH,
                          SECTION_NAME,
                          SECTION_CODE,
                          IDENTIFIED_FLAG,
                          PDS_GENDER),
                  fill = list(ITEM_COUNT = 0,
                              ITEM_PAY_DR_NIC = 0,
                              PATIENT_COUNT = 0)) |>
  tidyr::complete(IDENTIFIED_FLAG,
                  tidyr::nesting(YEAR_MONTH,
                          SECTION_NAME,
                          SECTION_CODE,
                          glue::glue_collapse(agebanding),
                          PDS_GENDER),
                  fill = list(ITEM_COUNT = 0,
                              ITEM_PAY_DR_NIC = 0,
                              PATIENT_COUNT = 0)) |>
  tidyr::complete(PDS_GENDER,
                  tidyr::nesting(YEAR_MONTH,
                          SECTION_NAME,
                          SECTION_CODE,
                          IDENTIFIED_FLAG,
                          glue::glue_collapse(agebanding)),
                  fill = list(ITEM_COUNT = 0,
                              ITEM_PAY_DR_NIC = 0,
                              PATIENT_COUNT = 0)) |>
  dplyr::group_by(SECTION_NAME,
                  SECTION_CODE,
                  IDENTIFIED_FLAG,
                  PDS_GENDER,
                  glue::glue_collapse(agebanding)) |>
  dplyr::group_by(SECTION_NAME, SECTION_CODE, IDENTIFIED_FLAG,
                  PDS_GENDER, glue::glue_collapse(agebanding)) |>
  dplyr::mutate(
    MONTH_START = as.Date(paste0(YEAR_MONTH, "01"), format = "%Y%m%d"),
    MONTH_NUM = lubridate::month(MONTH_START),
    MONTH_INDEX = lubridate::interval(lubridate::dmy(01032015), as.Date(MONTH_START)) %/% months(1)
  ) |>
  dplyr::left_join(dispensing_days,
                   by = "YEAR_MONTH") |>
  dplyr::filter(!(IDENTIFIED_FLAG == "N" & PDS_GENDER == "F"),
                !(IDENTIFIED_FLAG == "N" & PDS_GENDER == "M"),
                !(PDS_GENDER == "U" | glue::glue_collapse(agebanding) == "Unknown")) %>%
  dplyr::ungroup()

  # add columns to separate out months into individual factor variables

data |>
    dplyr::mutate(
      m_01 = 1*("MONTH_NUM" == 1),
      m_02 = 1*("MONTH_NUM" == 2),
      m_03 = 1*("MONTH_NUM" == 3),
      m_04 = 1*("MONTH_NUM" == 4),
      m_05 = 1*("MONTH_NUM" == 5),
      m_06 = 1*("MONTH_NUM" == 6),
      m_07 = 1*("MONTH_NUM" == 7),
      m_08 = 1*("MONTH_NUM" == 8),
      m_09 = 1*("MONTH_NUM" == 9),
      m_10 = 1*("MONTH_NUM" == 10),
      m_11 = 1*("MONTH_NUM" == 11),
      m_12 = 1*("MONTH_NUM" == 12)
    )
  }
