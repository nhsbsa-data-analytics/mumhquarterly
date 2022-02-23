#' Apply statistical disclosure control across a dataset
#'
#' This function allows the application of SDC for numeric columns within a
#' dataframe. the function evaluates based upon the raw value of the column
#' and doesn't handle any SDC required by inference.
#'
#' natural 0s are always preserved.
#'
#' This function can be ran as part of a dplyr pipe.
#'
#' @param data a dataframe object
#'
#' @param level level determines the value that SDC should be applied from.
#'   for example, `level = 5` applies SDC to all values less than 5 and greater
#'   than 0 (1, 2, 3, 4).
#'
#' @param rounding `TRUE` or `FALSE`. determines if rounding of none SDC values
#'   should be applied.
#'
#' @param round_val determines what nearest value should be used when applying
#'   rounding. for example `round_val = 5` will round values to the nearest 5
#'
#' @param mask determines what the actual mask value is for SDC values. defaults
#'   to `-1`. can be numeric, character, or `NA_real_`.
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' raw_data$mumh_annual_stp_data %>% apply_sdc(rounding = F) %>% View

apply_sdc <- function(data, level = 5, rounding = TRUE, round_val = 5, mask = -1) {

  rnd <- round_val

  # create sequence of redacted values
  redact_vals <- seq(from = 1, to = level, by = 1)

  # check dataframe for integers that are in redacted vals
  check <- data %>%
    dplyr::summarise(
      dplyr::across(
        where(is.integer),
        .fns = ~ sum(.x %in% redact_vals),
        .names = "CHECK_{.col}"
      )
    ) %>%
    dplyr::mutate(
      CHECK = sum(dplyr::c_across())
    ) %>%
    pull(CHECK)

  # if no values needing redaction found, stop function
  if(!check > 0) stop("No SDC required")

  if(is.character(mask)) {
    type <- function(x) as.character(x)
  } else {
    type <- function(x) x
  }

  data %>% dplyr::mutate(
    dplyr::across(
      where(is.integer),
      .fns = ~ dplyr::case_when(
        .x >= level & rounding == T ~ type(rnd * round(.x/rnd)),
        .x < level & .x > 0 & rounding == T ~ mask,
        .x < level & .x > 0 & rounding == F ~ mask,
        TRUE ~ type(.x)
      ),
      .names = "SDC_{.col}"
    )
  )
}
