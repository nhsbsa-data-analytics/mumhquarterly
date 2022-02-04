#' Easy helper for 'format_data'
#'
#' Format data on selected sheet of base 'wb' created using create_wb function
#'
#' @param workbook the name of the workbook object created using the create_wb function
#' @param sheetname the name of the sheet to apply style to
#' @param column the numeric values of the column or columns you wish to apply the style to
#' @param alignment Horizontal alignment of cell contents
#' \itemize{
#'   \item \strong{left} Left horizontal align cell contents
#'   \item \strong{right} Right horizontal align cell contents
#'   \item \strong{center} Center horizontal align cell contents
#'   \item \strong{justify} Justify horizontal align cell contents
#' }
#' @param number_format the Excel number format code you wish to apply
#' @param filepath the file path to save the workbook to
#'
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' format_data(myworkbook,
#' "test1",
#' 1,
#' "right",
#' "#,###",
#' C:/test.xlsx"
#' )
#' 
#' format_data(myworkbook,
#' "test1",
#' 1,
#' "right",
#' "#,##0.00",
#' C:/test.xlsx"
#' )

format_data <- function(workbook,
                        sheetname,
                        column,
                        alignment,
                        number_format,
                        filepath) {
  
  #name workbook
  wb <- workbook
  
  #get full data
  data1 <- openxlsx::read.xlsx(wb, "test1")
  
  #get full data minus title/notes
  data2 <- openxlsx::read.xlsx(wb, "test1") %>% na.omit()
  
  #calculate starting row of data
  first_row <- as.numeric(nrow(data1) - nrow(data2) + 2)
  
  #calculate end row
  last_row <- as.numeric(nrow(data1)) + 1
  
  #create style for column header
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = alignment
  )
  
  #add style to header
  openxlsx::addStyle(
    wb,
    sheetname,
    header_style,
    first_row,
    column,
    gridExpand = TRUE
  )
  
  #create style for data
  style <- openxlsx::createStyle(
    halign = alignment,
    numFmt = number_format
  )
  
  #add style to data
  openxlsx::addStyle(
    wb,
    sheetname,
    style,
    first_row + 1:last_row,
    column,
    gridExpand = TRUE
  )
  
  #save workbook
  openxlsx::saveWorkbook(wb,
                         file = filepath,
                         overwrite = TRUE)
}