
#' Read table of data from .csv or .xlsx Excel file
#'
#' If in RStudio, interactively can select file from local drive. Also used by EJAM shiny app for file uploads.
#'
#' @description Read simple table from csv or xls or xlsx.
#' For excel format, must be simple table on first tab,
#' one row for header (column names),
#' data itself starting in first cell of second row, like A2, and
#' all other rows and columns must be empty.
#'
#' @param fname full path to folder and filename
#' @param path optional, ignored if name provided or !interactive().
#'   If fname NOT provided, and interactive(), the folder to look in by default
#'   when interactively picking a file to read.
#' @param show_col_types FALSE makes it print less to console as it reads using readr::read_csv()
#' @param rowsize_warn Give warning if there are more than this many rows in the table
#' @return data.frame with contents of table it read
#'
#' @keywords internal
#' @export
#'
read_csv_or_xl <- function(fname = NULL, path = NULL, show_col_types = FALSE, rowsize_warn = 30 * 1000) {
  
  #See if in shiny application
  in_shiny <- shiny::isRunning()
  
  if(!in_shiny){
    if (is.null(fname)) {
      if (interactive()) {
        if (rstudioapi::isAvailable()) {
          if (missing(path) || is.null(path) || !dir.exists(dirname(normalizePath(path)))) {path <- testdata(quiet = TRUE)}
          fname <- rstudioapi::selectFile("Select a file",
                                          caption = "Select a file to upload",
                                          filter = "excel/csv Files (*.xlsx,*.xls,*.csv)",
                                          path = path) # or could use getwd()
        }
        else {
          fname <- file.choose()
        }
      }
      else {
        stop("fname (file path/name) needed but not provided")
      }
    }
  }
  
  if (is.data.frame(fname)) {
    return(fname)
  }
  
  # NEW: Retry wrapper for file operations
  read_with_retry <- function(read_func, max_retries = 3) {
    for (attempt in 1:max_retries) {
      result <- tryCatch({
        if (!file.exists(fname) || file.info(fname)$size == 0) {
          stop("File not ready")
        }
        read_func()
      }, error = function(e) {
        if (attempt < max_retries) {
          Sys.sleep(0.3 * attempt)
          return(NULL)
        } else {
          stop(e$message)
        }
      })
      if (!is.null(result)) return(result)
    }
  }
  
  file_type <-tolower(tools::file_ext(fname))
  
  filecontents <- switch(file_type,
                         csv = {
                           tryCatch({
                             data <- read_with_retry(function() {
                               readr::read_csv(fname, show_col_types = show_col_types) %>% as.data.frame()
                             })
                             if (NROW(data) > rowsize_warn) {
                               warning("There are more than ", rowsize_warn, " rows in this dataset!")
                             }
                             data
                           }, error = function(e) {
                             if (in_shiny) {
                               shiny::validate(paste("This CSV file caused an error:", e$message))
                             } else {
                               stop("Error reading CSV file: ", e$message)
                             }
                             NULL
                           })
                         },
                         xls = {
                           tryCatch({
                             data <- read_with_retry(function() {
                               sheets <- readxl::excel_sheets(fname)
                               if (length(sheets) > 1) {
                                 if (in_shiny) {
                                   shiny::showNotification(
                                     "This Excel file contains multiple sheets. Only the first sheet is processed",
                                     type = "warning",
                                     duration = 5
                                   )
                                 } else {
                                   warning("This Excel file contains multiple sheets. Only the first sheet is processed.")
                                 }
                               }
                               readxl::read_excel(fname, sheet = 1) %>% as.data.frame()
                             })
                             if (NROW(data) > rowsize_warn) {
                               warning("There are more than ", rowsize_warn, " rows in this dataset!")
                             }
                             data
                           }, error = function(e) {
                             if (in_shiny) {
                               shiny::validate(paste("This Excel file caused an error:", e$message))
                             } else {
                               stop("Error reading Excel file: ", e$message)
                             }
                             NULL
                           })
                         },
                         xlsx = {
                           tryCatch({
                             data <- read_with_retry(function() {
                               sheets <- readxl::excel_sheets(fname)
                               if (length(sheets) > 1) {
                                 if (in_shiny) {
                                   shiny::showNotification(
                                     "This Excel file contains multiple sheets. Only the first sheet is processed",
                                     type = "warning",
                                     duration = 5
                                   )
                                 } else {
                                   warning("This Excel file contains multiple sheets. Only the first sheet is processed.")
                                 }
                               }
                               readxl::read_excel(fname, sheet = 1) %>% as.data.frame()
                             })
                             if (NROW(data) > rowsize_warn) {
                               warning("There are more than ", rowsize_warn, " rows in this dataset!")
                             }
                             data
                           }, error = function(e) {
                             if (in_shiny) {
                               shiny::validate(paste("This Excel file caused an error:", e$message))
                             } else {
                               stop("Error reading Excel file: ", e$message)
                             }
                             NULL
                           })
                         },
                         stop("Invalid file type. Please upload a .csv, .xls, or .xlsx file")
  )
  
  return(filecontents)
}