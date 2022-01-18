#' Easy helper for 'create_wb'
#'
#' Create base workbook 'wb' and add sheets required
#'
#' @param filepath the filepath to save the workbook to
#' @param sheets a list of sheet names require
#'
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' sheetNames <- c("test1","test2","test3")
#' create_wb("C:/test.xlsx", sheetNames)

create_wb <- function(
  filepath,
  sheets
) {
  ##create workbook with relevant named tabs
  wb <- openxlsx::createWorkbook()

  ##assign wb to use in global environment for future functions
  assign("wb", wb, envir=globalenv())

  openxlsx::addWorksheet(wb, sheetName = "Cover_sheet")
  openxlsx::addWorksheet(wb, sheetName = "Metadata")

  #remove gridlines on base tabs
  openxlsx::showGridLines(wb, sheet = "Cover_sheet", showGridLines = FALSE)
  openxlsx::showGridLines(wb, sheet = "Metadata", showGridLines = FALSE)

  for(i in 1:length(sheets)) {
    openxlsx::addWorksheet(wb, sheetName = sheets[i])
  }

  #remove gridlines on other tabs
  for(j in 1:length(sheets)) {
    openxlsx::showGridLines(wb, sheet = sheets[j], showGridLines = FALSE)
  }

  #set font to Arial
  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10)

  openxlsx::saveWorkbook(wb,
                         file = filepath,
                         overwrite = TRUE)
}
