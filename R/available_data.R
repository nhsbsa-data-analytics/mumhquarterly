#' Get latest available periods of data
#'
#'`available_data` returns a message to the console with the latest available
#'month, full financial quarter, and full financial year of data from the
#'NHSBSA Data Warehouse.
#'
#' @param database String. Name of the DB to be passed to `nhsbsaR::con_nhsbsa`.
#' one of c("DALP", "DWCP")
#'
#' @return A `NULL` object. this function only sends a message to the console.
#'
#' @export
#'
#' @examples
#'available_data("DWCP")
#'
#'#The latest month of data is:
#'#202111
#'
#'#The latest complete financial quarter of data is:
#'#Jul - Sep 21
#'
#'#The latest complete financial year of data is:
#'#2020/2021
#'
#'@import dplyr


available_data <- function(database) {

  # build connection to NHSBSA DWH using function from nhsbsaR package
  # override default behaviour or username and password args
 con <-  nhsbsaR::con_nhsbsa(
   database = database,
   username = rstudioapi::showPrompt(title = "Username",
                                     message = "Enter username"),
   password = rstudioapi::askForPassword()
 )

 # bring in DIM.YEAR_MONTH_DIM
 ym_dim <- dplyr::tbl(con,
                      from = dbplyr::in_schema("DIM", "YEAR_MONTH_DIM")) %>%
   # shrink table to remove unnecessary data
   dplyr::filter(
     YEAR_MONTH >= 201401L,
     YEAR_MONTH <= dplyr::sql(
       "MGMT.PKG_PUBLIC_DWH_FUNCTIONS.f_get_latest_period('EPACT2')"
     )
   ) %>%
   dplyr::select(
     YEAR_MONTH,
     FINANCIAL_YEAR,
     FINANCIAL_QUARTER,
     FINANCIAL_QUARTER_EXT
   )  %>%
   # add month counts for financial quarters and financial years to latest
   # complete periods
   dplyr::mutate(
     # create our own financial quarter column that max/min and sort operations
     # will work on
     FINANCIAL_QUARTER_NM = dplyr::sql(
       "FINANCIAL_YEAR||' Q'||FINANCIAL_QUARTER"
     ),
     # window function to perform counts within groups
     Q_COUNT = dbplyr::win_over(
       expr = dplyr::sql("count(distinct YEAR_MONTH)"),
       partition = "FINANCIAL_QUARTER_EXT",
       con = con
     ),
     FY_COUNT = dbplyr::win_over(
       expr = dplyr::sql("count(distinct YEAR_MONTH)"),
       partition = "FINANCIAL_YEAR",
       con = con
     )
   )
 # df <- ym_dim %>% collect() %>% arrange(YEAR_MONTH)
 # extract latest available month of data
 ltst_month <- ym_dim %>%
   dplyr::filter(YEAR_MONTH == max(YEAR_MONTH, na.rm = TRUE)) %>%
   dplyr::pull(YEAR_MONTH)

 # extract latest available full financial quarter
 ltst_quarter <- ym_dim %>%
   dplyr::filter(Q_COUNT == 3) %>%
   dplyr::select(FINANCIAL_QUARTER_EXT, FINANCIAL_QUARTER_NM) %>%
   dplyr::filter(
     FINANCIAL_QUARTER_NM == max(FINANCIAL_QUARTER_NM, na.rm = TRUE)
   ) %>%
   dplyr::distinct() %>%
   dplyr::pull(FINANCIAL_QUARTER_EXT)

 # extract latest available full financial year
 ltst_year <- ym_dim %>%
   dplyr::filter(FY_COUNT == 12) %>%
   dplyr::select(FINANCIAL_YEAR) %>%
   dplyr::filter(
     FINANCIAL_YEAR == max(FINANCIAL_YEAR, na.rm = TRUE)
   ) %>%
   dplyr::distinct() %>%
   dplyr::pull(FINANCIAL_YEAR)

 # close database connection
 DBI::dbDisconnect(con)

 # send message to user with information
 cat(
   "The latest month of data is:\n",
   ltst_month, "\n\n",
   "The latest complete financial quarter of data is:\n",
   ltst_quarter, "\n\n",
   "The latest complete financial year of data is:\n",
   ltst_year, "\n"
 )

}
