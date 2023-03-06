#' Build infoBox with a border
#'
#' @description Create an info box with border to use in a R markdown file
#'
#' @param header The header text (h4) to be given to the infoBox
#' @param text The further text needed for the infoBox
#' @param backgroundColour The fill colour of the infoBox
#' @param borderColour The colour of the border to be used
#' @param width The width of the infoBox (e.g "33%" for 3 column layout)
#' @param fontColour The font colour to use in the infoBox
#'
#' @export
#'
#' @examples
#' `r infoBox_border()`
infoBox_border <- function(
    header = "Header here",
    text = "More text here",
    backgroundColour = "#ccdff1",
    borderColour = "#005EB8",
    width = "31%",
    fontColour = "black") {
  paste(
    "<div class='infobox_border' style = 'border: 1px solid ", borderColour,"!important;
  border-left: 5px solid ", borderColour,"!important;
  background-color: ", backgroundColour,"!important;
  padding: 10px;
  margin-bottom: 20px;
  width: ", width,"!important;
  display: inline-block;
  vertical-align: top;
  flex: 1;
  height: 100%;'>
  <h4 style = 'color: ", fontColour, ";
  font-weight: bold;
  font-size: 18px;
  margin-top: 0px;
  margin-bottom: 10px;'>", header, "</h4>
  <p style = 'color: ", fontColour, ";
  font-size: 16px;
  margin-top: 0px;
  margin-bottom: 0px;'>", text, "</p>
</div>"
  )
}
