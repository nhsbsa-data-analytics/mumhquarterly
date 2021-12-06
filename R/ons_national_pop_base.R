#' National population reference file import function
#'
#' This function gets the latest mid-year population estimates from ONS,
#' and imports it in a form to be used within MUMH Quarterly package.
#'
#' @param url The url for the population data to be imported. Defaults to NULL.
#'
#' @keywords
#' @export
#' @examples
#'
#' @source \url{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatestimeseriesdataset/current/}


ons_national_pop <- function(url = NULL){

  # import pipe operator from magrittr

  `%>%` <- magrittr::`%>%`

  if(is.null(url)){

    url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatestimeseriesdataset/current/pop.csv"

  }

  #read in data

  df <- data.table::fread(url,
                          skip = 7,
                          col.names = c("YEAR","SCPOP","GBPOP","ENPOP","UKPOP",
                                        "EWPOP","NIPOP","WAPOP"))

  #filter data to England population

  df <- df %>%
    dplyr::select(YEAR, ENPOP)


  return(df)

}

ons_national_pop()
