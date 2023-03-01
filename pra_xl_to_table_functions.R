## gives a pop that allows to choose a file and then read that file in

#' find_pra_list
#'
#' @param x
#'
#' @return the file path (assigned to object called  pra_list) of the excel file contain the PRA list data
#' @export
#'
#' @examples pra_list_unfiltered <- find_pra_list()

find_pra_list <- function(x) {
  file_path_of_pra_list <- file.choose()

pra_list <- readxl::read_excel(file_path_of_pra_list,
                               col_names = TRUE)
}



#' pra_list_org_extraction
#'
#' @param data - object that contains PRA data
#' @param org - filter parameter to extract the data of a chosen organisation from pra data
#'
#' @return - a dataframe that contains the job titles and number of indeviduals per role for the PRA release
#' @export
#'
#' @examples - pra_list_org_extraction(pra_list, NHSBSA)

pra_list_org_extraction <-function (data,org) {

pra_filtered <- data |>
  dplyr::filter( Organisation == org) |>
  dplyr::group_by(Title) |>
  dplyr::summarise("Individuals per Role" = n(),
                   .groups = "drop") |>
  dplyr::arrange(Title)
}


devtools::document()
