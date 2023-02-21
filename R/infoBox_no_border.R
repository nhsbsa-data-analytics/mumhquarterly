#' Build infoBox with a border
#'
#' @description Create an info box with solid colour to us in a R markdown file
#'
#' @param header The header text (h4) to be given to the infoBox
#' @param text The further text needed for the infoBox
#' @param backgroundColour The fill colour of the infoBox
#' @param width The width of the infoBox (e.g "33%" for 3 column layout)
#' @param fontColour The font colour to use in the infoBox
#'
#' @export
#'
#' @examples
#' `r infoBox_no_border()`
infoBox_no_border <- function(
    header = "Header here",
    text = "More text here",
    backgroundColour = "#005EB8",
    width = "31%",
    fontColour = "white") {
  paste(
    "<div class='infobox_no_border',
    style = 'background-color: ",backgroundColour,
    "!important;padding: 10px;
    margin-bottom: 20px;
    width: ",width,";
    display: inline-block;
    vertical-align: top;'>
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
