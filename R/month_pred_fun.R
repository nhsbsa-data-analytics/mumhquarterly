#' month_pred_fun
#'
#' @param month - value for a specific month
#' @param data - input data
#' @param model - a linear rregression model object
#' @param alpha - The confidence level for the prediction intervals. Default is 0.95.
#'
#' @details This function filters input data by month then, using the `fast_agg_pred` function, produces a
#' prediction and calculates the mean, variance, and prediction intervals at the specified alpha.
#'
#' @return - A data frame for selecteted month holding the prediction values and interval.
#' @export
#'
#'@imporFrom dplyr::filter
#'@imporFrom mumhquarterly::fast_agg_pred
#'
#' @examples

month_pred_fun <- function(month, data, model, alpha = 0.95){
  data <- data %>%
    dplyr::filter(YEAR_MONTH == month)

  pred <- fast_agg_pred(rep.int(1, nrow(data)), data, lmObject = model, alpha = alpha)
  pred99 <- fast_agg_pred(rep.int(1, nrow(data)), data, lmObject = model, alpha = 0.99)
  output <- data.frame(unit = 1)

  output$YEAR_MONTH <- month
  output$mean_fit <- pred[["mean"]]
  output$var <- pred[["var"]]
  output$PIlwr <- pred[["PI"]][["lower"]]
  output$PIupr <- pred[["PI"]][["upper"]]
  output$PIlwr99 <- pred99[["PI"]][["lower"]]
  output$PIupr99 <- pred99[["PI"]][["upper"]]
  output$unit <- NULL

  return(output)

}
