#' Easy helper for 'Excel column to numeric'
#'
#' Convert charater string of Excel column to a numeric
#'
#' @param column_letter the letter of the Excel column to be converted
#'
#' @export
#'
#' @examples
#' excel_column_to_numeric("A")
#' excel_column_to_numeric("AAA")

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