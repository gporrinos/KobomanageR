


#' Write a data frame to CSV or Excel file
#'
#' This function writes a data frame to disk in either CSV or Excel format.
#' For CSV files, the field separator can be specified manually or inferred
#' from the system locale. For Excel files, the data is written to a single
#' worksheet.
#'
#' @param data A data frame to be written to file.
#' @param fileformat A character string specifying the output format.
#'   Supported values are `"csv"` and `"xlsx"`.
#' @param filepath A character string specifying the file path to write to.
#' @param csv_separator Optional character string specifying the CSV separator.
#'   Must be either `","` (comma) or `";"` (semicolon). If `NULL`, the separator
#'   is determined based on the system locale.
#' @param sheetname A character string specifying the worksheet name when writing
#'   Excel files. Must be provided if `fileformat = "xlsx"`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @details
#' For CSV output:
#' \itemize{
#'   \item If `csv_separator` is provided, it determines whether `utils::write.csv()`
#'   or `utils::write.csv2()` is used.
#'   \item If `csv_separator` is `NULL`, the system decimal separator is used
#'   to infer the appropriate CSV format.
#' }
#'
#' For Excel output:
#' \itemize{
#'   \item The data is written as a single worksheet using
#'   `writexl::write_xlsx()`.
#' }
#' @export
writeDAT <- function(data, fileformat, filepath, csv_separator = NULL, sheetname = NULL){
  if(!fileformat %in% c("csv", "xlsx")) {
    stop("Unsupported file format")
  }
  if(fileformat == "csv"){
    if(!is.null(csv_separator)) {
      if(csv_separator == ",") {
        utils::write.csv(data, filepath, row.names = FALSE)
      } else if(csv_separator == ";") {
        utils::write.csv2(data, filepath, row.names = FALSE)
      } else {
        stop("csv_separator must be ',' or ';'")
      }
    } else {
      if(Sys.localeconv()[which(names(Sys.localeconv()) == "decimal_point")] == ".") {
        utils::write.csv(data, filepath, row.names = FALSE)
      } else {
        utils::write.csv2(data, filepath, row.names = FALSE)
      }
    }
  }
  if(fileformat == "xlsx"){
    if(is.null(sheetname)) stop("Please provide a sheet name")
    data_output <- list(data)
    names(data_output) <- sheetname
    writexl::write_xlsx(data_output, filepath, col_names = TRUE)
  }
}




#' Read a data frame from CSV or Excel file
#'
#' This function imports data from a CSV or Excel file and returns it as a
#' data frame. For CSV files, the separator can be specified manually or
#' inferred from the system locale. For Excel files, a specific worksheet
#' must be provided.
#'
#' @param sheetname A character string specifying the worksheet name when
#'   reading Excel files. Required if `fileformat = "xlsx"`.
#' @param filepath A character string specifying the file path to read from.
#' @param fileformat A character string specifying the input format.
#'   Supported values are `"csv"` and `"xlsx"`.
#' @param csv_separator Optional character string specifying the CSV separator.
#'   Must be either `","` (comma) or `";"` (semicolon). If `NULL`, the separator
#'   is determined based on the system locale.
#'
#' @return A data frame containing the imported data.
#'
#' @details
#' For CSV files:
#' \itemize{
#'   \item If `csv_separator` is provided, it determines whether `utils::read.csv()`
#'   or `utils::read.csv2()` is used.
#'   \item If `csv_separator` is `NULL`, the system decimal separator is used
#'   to infer the appropriate format.
#' }
#'
#' For Excel files:
#' \itemize{
#'   \item Data is imported using `readxl::read_excel()`.
#'   \item The result is converted to a data frame.
#'   \item `guess_max` is set to 100000 to improve type inference.
#' }
#'
#' @examples
#' \dontrun{
#' df <- readDAT(filepath = "mtcars.csv", fileformat = "csv")
#' df <- readDAT(filepath = "mtcars_semicolon.csv", fileformat = "csv", csv_separator = ";")
#' df <- readDAT(filepath = "mtcars.xlsx", fileformat = "xlsx", sheetname = "Sheet1")
#' }
#'
#' @export
readDAT <- function(sheetname = NULL, filepath, fileformat, csv_separator = NULL){
  if(!fileformat %in% c("csv", "xlsx")) {
    stop("Unsupported file format")
  }
  if(fileformat == "csv"){
    if(!is.null(csv_separator)) {
      if(csv_separator == ",") {
        return(utils::read.csv(filepath))
      } else  if(csv_separator == ";") {
        return(utils::read.csv2(filepath))
      } else {
        stop("csv_separator must be ',' or ';'")
      }
    } else {
      if(Sys.localeconv()[which(names(Sys.localeconv()) == "decimal_point")] == ".") {
        return(utils::read.csv(filepath))
      } else {
        return(utils::read.csv2(filepath))
      }
    }
  }
  if(fileformat == "xlsx"){
    if(is.null(sheetname)) stop("Please provide a sheet name")
    return(as.data.frame(readxl::read_excel(path = filepath, sheet = sheetname, guess_max = 100000)))
  }
}
