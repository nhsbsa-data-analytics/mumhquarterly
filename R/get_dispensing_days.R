#' get_dispensing_days function
#'
#' This function gets the dispensing days for a time period based on the input year
#' using bank holiday data from the UK government website's API.
#' Dispensing days are classed as days that are not a bank holiday or Sunday
#' in a given month.
#'
#' @param year The latest financial year of data required. For example, 2022
#' for the financial year 2021/22
#'
#' @import dplyr
#' @import tidyr
#' @import jsonlite
#' @import lubridate
#'
#' @export
#'
#' @examples
#' get_dispensing_days(2022)
#'
#' @source \url{https://www.gov.uk/bank-holidays.json}

get_dispensing_days <- function(current_year){

  input <- current_year

  # first function to get bank holidays from gov website API
  get_bank_hols <- function(input){

  # calculate last day of financial year period in March
  end_date <- as.Date(paste0(input,"-03-31"))

  # calculate first day of the financial year period
  # currently 6 years previous in 2015/16
  start_date <- end_date - lubridate::years(6) + lubridate::days(1)

  url = "https://www.gov.uk/bank-holidays.json"

  # encode url to make it readable by json
  encoded_url = utils::URLencode(url)

  # read in json file
  list = jsonlite::fromJSON(encoded_url)

  # get bank holidays
  bank_hols_ew <- as.data.frame(list$`england-and-wales`) %>%
    # change column names
    dplyr::rename(DIVISION = division,
                  NAME = events.title,
                  DATE = events.date,
                  NOTES = events.notes,
                  BUNTING = events.bunting
    ) %>%

    # filter for required dates
    dplyr::filter(DATE >= start_date & DATE <= end_date) %>%

    # create variables, including flag for substitute days
    dplyr::mutate(DATE = as.Date(DATE),
                  WEEKDAY_NAME = weekdays(DATE,abbreviate = TRUE),
                  SUB_DAY = dplyr::case_when(NOTES == "Substitute day"  ~ 1,
                                             TRUE ~ 0)
    ) %>%

    # select required columns
    dplyr::select("DATE")

  # return data frame to use in second function
  return(bank_hols_ew)

  }

  # second function to subtract sundays and bank holidays from full month
  dispensing_days_calc <- function (bank_hols_ew){

    #add vector of bank holidays from 2015 and 2016
    #gov API only contains 2017 onwards

    old_dates <- data.frame(DATE = c("2015-01-01", "2015-04-03",
                                     "2015-04-06", "2015-05-04",
                                     "2015-05-25", "2015-08-31",
                                     "2015-12-25", "2015-12-28",
                                     "2016-01-01", "2016-03-25",
                                     "2016-03-28", "2016-05-02",
                                     "2016-05-30", "2016-08-29",
                                     "2016-12-26", "2016-12-27"))

    bank_hols_ew <- rbind(bank_hols_ew, old_dates)

    bank_hols_ew$DATE <- as.Date(bank_hols_ew$DATE,
                                 format = "%d/%m/%Y")

    dispensing_days_england <-
      data.frame(DATE = seq(from = as.Date("2015-04-01"),
                            to = as.Date(paste0(input,"-03-31")), by = "days")) %>%
      dplyr::mutate(
        YEAR_MONTH = as.numeric(format(DATE, "%Y%m")),
        WEEKDAY = weekdays(DATE)
      ) %>%
      dplyr::filter(
        DATE %!in% bank_hols_ew$DATE,
        WEEKDAY != "Sunday"
      ) %>%
      dplyr::group_by(YEAR_MONTH) %>%
      dplyr::summarise(DISPENSING_DAYS = n())

    return(dispensing_days_england)

  }

  #combine functions to get output, then return
  dispensing_days <- dispensing_days_calc(get_bank_hols(input))

  return(dispensing_days)

}
