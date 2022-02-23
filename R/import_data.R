#' Import latest CSV
#'
#' @description
#' Import a CSV file ouputted from `mumhquarterly::save_data`. The function uses
#' the timestamp of the file to determine if it is 'latest'.
#'
#' @param path file path folder where data is stored. defaults to "data" on
#' assumption that the function will be used in an Rproj
#' @param ... additional parameters to be passed to `data.table::fread`
#'
#' @return
#' returns a data.frame
#'
#' @export
#'
#' @examples
#' df <- import_data()
#'
#' df <- import_data("Y:/Official Stats")
#'
#' tmp <- tempdir()
#'
#' mumhquarterly::save_data(mtcars, dir = tmp)
#'
#' df <- import_data(file.path(tmp, "data"), stringsAsFactors = TRUE)
import_data <- function (path = "data", ...) {

  # get files from directorary. remove subdirectories
  lf <- setdiff(
    list.files(path),
    list.dirs(path, full.names = FALSE, recursive = FALSE)
  )

  # get latest version of file available by looking at time stamp.
  # reg ex to extract any number of digits "\\d*" preceded by a hyphen "(?<=-)"
  # and followed by a dot "(?=\\.)"
  ts <- stringr::str_extract(lf, "(?<=-)\\d*(?=\\.)")

  # get max timestamp to identify latest file
  ltst_ts <- max(ts)

  # subset file list for latest file name
  ltst_lf <- lf[grepl(ltst_ts, lf)]

  # read in data using data.table::fread. include options by default to handle
  # BNF codes and SNOMED codes correctly
  df <- data.table::fread(
    file.path(path, lsts_lf),
    keepLeadingZeros = TRUE,
    integer64 = "character",
    ...
  )

}
