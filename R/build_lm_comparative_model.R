
# build 5yr vs 20yr agebands model for each BNF section using full set of variables

#' build_lm_comparative_model
#'
#' @param trained_data - covid model data conaining both 5 year and 20 year age bandings
#' @param section_code - section code for given subcatagory eg 0401
#'
#' @return - graphical comparison of linear regression models for covid model data
#' @export
#'
#' @examples

build_lm_comparative_model <- function(trained_data,
                                       section_code,
                                       five_year = FALSE,
                                       twenty_year = FALSE){

if (five_year == TRUE & twenty_year == FALSE){

  lm_model_0402_example <- lm(ITEM_COUNT ~ MONTH_INDEX + DISPENSING_DAYS + as.factor(MONTH_NUM)
                 + PDS_GENDER*as.factor(DALL_5YR_BAND),
                 data = filter(trained_data, SECTION_CODE == section_code))

}else if(five_year == FALSE & twenty_year == TRUE){

  lm_model_0402_example <- lm(ITEM_COUNT ~ MONTH_INDEX + DISPENSING_DAYS + as.factor(MONTH_NUM)
                  + PDS_GENDER*as.factor(BAND_20YR),
                  data = filter(trained_data, SECTION_CODE == section_code))

} else if(five_year == FALSE & twenty_year == FALSE | five_year == TRUE & twenty_year == TRUE){

  stop("Please select one age banding option" )
}

  return (lm_model_0402_example)
}
