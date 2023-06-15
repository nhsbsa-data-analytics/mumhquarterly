#' covid_lm_with_months
#'
#' @param trained_data
#' @param section_code
#'
#' @return
#' @export
#'
#' @examples


covid_lm_with_months <- function(trained_data,
                                 section_code){

lm_covid_model_with_months <- lm(ITEM_COUNT ~ MONTH_INDEX + DISPENSING_DAYS + m_02 + m_03
               + m_04 + m_05 + m_06 + m_07 + m_08 + m_09 + m_10 + m_11 + m_12
               + PDS_GENDER*as.factor(BAND_20YR),
               data = filter(trained_data, SECTION_CODE == section_code))

return(lm_covid_model_with_months)

}
