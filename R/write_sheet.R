#' Easy helper for 'write_sheet'
#'
#' Write data to selected sheet of base 'wb' created using create_wb function
#'
#' @param filepath the file path to save the workbook to
#' @param sheetname the name of the blank sheet to write to
#' @param title the title of the sheet which will go into cell A1
#' @param notes a list object of the notes to be included on the sheet
#' @param dataset the name of the dataset to be written to the names sheet
#'
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' write_sheet("C:\\test.xlsx",
#' "test1",
#' "title1",
#' c("note1", "note2", "note3", "note4"),
#' mtcars)

write_sheet <-  function(filepath,
                         sheetname,
                         title,
                         notes,
                         dataset) {

  notes_list <- notes

  #write title
  openxlsx::writeData(
    wb,
    sheet = sheetname,
    x = title,
    xy = c(1,1)
  )

  #bold title
  addStyle(
    wb,
    sheet = sheetname,
    style = createStyle(textDecoration = "bold"),
    cols = 1,
    rows = 1
  )

  #write notes header
  openxlsx::writeData(
    wb,
    sheet = sheetname,
    x = "Notes",
    xy = c(1,3)
  )

  #bold notes
  openxlsx::addStyle(
    wb,
    sheet = sheetname,
    style = createStyle(textDecoration = "bold"),
    cols = 1,
    rows = 3
  )

  #loop to write all notes
  for(i in 1:length(notes_list)) {

    openxlsx::writeData(
      wb,
      sheet = sheetname,
      x = notes_list[i],
      xy = c(1,(i + 3))
    )

  }

  #write data as named data table
  openxlsx::writeDataTable(wb,
                           sheet = sheetname,
                           x = dataset,
                           startRow = (length(notes_list) + 5),
                           tableStyle = "none",
                           withFilter = FALSE,
                           tableName = sheetname)

  #save workbook
  openxlsx::saveWorkbook(wb,
                         file = filepath,
                         overwrite = TRUE)
}
