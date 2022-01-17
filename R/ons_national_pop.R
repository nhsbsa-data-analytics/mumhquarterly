#' National population reference file import function, with helper function
#'
#' This function gets the latest mid-year population estimates from ONS,
#' and imports it in a form to be used within MUMH Quarterly package.
#' There are arguments to specify year and geographical area.
#'
#' Optional helper function shows users possible input values for year and area.
#'
#' @param url The url for the population data to be imported. Defaults to NULL
#'
#' @param year The year/s to filter population by, must be a vector
#'
#' @param area The geographical area/s to filter population by, must be a vector
#'
#' @import dplyr, data.table
#'
#' @export
#'
#' @examples
#' helper.ons_national_pop()
#' ons_national_pop(, year = c(2020), area = c("ENPOP"))
#' ons_national_pop(, year = c(2015, 2017, 2019), area = c("ENPOP", "EWPOP")
#'
#' @source \url{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatestimeseriesdataset/current/}


#Optional helper function to run before main function call

helper.ons_national_pop <- function(url = NULL){

  if(is.null(url)){

    url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatestimeseriesdataset/current/pop.csv"}

  #read in data and only keep YEAR variable

  df <- data.table::fread(url,
                          skip = 7, drop = 2:8, col.names = c("YEAR"))

  min_year <- df %>%
    dplyr::filter(YEAR == min(YEAR))
  max_year <- df %>%
    dplyr::filter(YEAR == max(YEAR))

  min_year <- pull(min_year)
  max_year <- pull(max_year)

  #show user the accepted values for year and area arguments, using earliest and latest year from data

  year <- cat("The year input must be a vector of any years between", min_year, "and", max_year, "\n")
  area <- cat('The area input must be a vector of any areas from the following: "SCPOP","GBPOP","ENPOP","UKPOP",
              "EWPOP","NIPOP","WAPOP"', "\n")

  return(c(year, area))

  }

#Main function

ons_national_pop <- function(url = NULL, year, area) {

  #import pipe operator from magrittr

  `%>%` <- magrittr::`%>%`

  if(is.null(url)){

    url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatestimeseriesdataset/current/pop.csv"}

  #read in data

  df <- data.table::fread(url,
                          skip = 7,
                          col.names = c("YEAR","SCPOP","GBPOP","ENPOP","UKPOP",
                                        "EWPOP","NIPOP","WAPOP"))

  #filter YEAR variable by any years in year input, select all area variables from area input

    pop_df <- df %>%
      dplyr::filter(YEAR %in% (!!year)) %>%
      dplyr::select(YEAR, all_of(!!area))

  return(pop_df)

}
