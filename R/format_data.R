#' Easy helper for 'format_data'
#'
#' Format data on selected sheet of base 'wb' created using create_wb function
#'
#' @param workbook the name of the workbook object created using the create_wb function
#' @param sheetname the name of the sheet to apply style to
#' @param column the excel column letter(s) you wish to apply the style to eg. "F" or c("A", "B")
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
#' "A",
#' "right",
#' "#,###",
#' "C:/test.xlsx"
#' )
#' format_data(myworkbook,
#' "test1",
#' c("A", "B", "C"),
#' "right",
#' "#,###.00",
#' "C:/test.xlsx"
#' )
#' format_data(myworkbook,
#' "test1",
#' C("D", "AA", "X"),
#' "left",
#' "",
#' "C:/test.xlsx"
#' )
format_data <- function(workbook,
                        sheetname,
                        column,
                        alignment,
                        number_format) {
  
  #build function to convert excel column letter to number
  excel_column_to_numeric <- function(column_letter){
    # Uppercase
    s_upper <- toupper(column_letter)
    # Convert string to a vector of single letters
    s_split <- unlist(strsplit(s_upper, split=""))
    # Convert each letter to the corresponding number
    s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
    # Derive the numeric value associated with each letter
    numbers <- 26^((length(s_number)-1):0)
    # Calculate the column number
    column_number <- sum(s_number * numbers)
    column_number
  }
  
  #vectorise to allow multiple columns
  excel_column_to_numeric <- Vectorize(excel_column_to_numeric)
  
  #convert column(s) to numeric values
  column_number <- excel_column_to_numeric(column)
  
  #name workbook
  wb <- workbook
  
  #get full data
  data1 <- openxlsx::read.xlsx(wb, sheetname)
  
  #get full data minus title/notes
  data2 <- openxlsx::read.xlsx(wb, sheetname) %>% na.omit()
  
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
    column_number,
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
    column_number,
    gridExpand = TRUE
  )
}