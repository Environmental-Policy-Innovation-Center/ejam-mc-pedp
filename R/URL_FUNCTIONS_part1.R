################################################### #################################################### #

# This file has generic url-related functions/helpers/utilities
#
# LIST OF FUNCTIONS HERE ####
#
#   see outline via ctrl-shift-O
#
#   also see URL_*.R and url_*.R

################################################### #################################################### #

#' utility - check if URL available, such as if an API is online or offline
#' @param url the URL to check
#' @returns TRUE or FALSE (but NA if no internet connection seems to be available at all)
#' @details
#' Also see EJAM:::global_or_param("ejamapi_is_down") and EJAM:::global_or_param("ejscreenapi_is_down")
#'    as set in global_defaults_package.R
#'
#' @keywords internal
#'
url_online <- function(url = c("ejscreen.epa.gov",)) {
  if (missing(url)) {stop("must specify a URL")}
  if (length(url) > 1) {stop("can only check one URL at a time using url_online()")}
  if (offline()) {
    warning("Cannot check URL when offline -- internet connection does not seem to be available")
    return(NA)
  }
  x <- httr2::request(url)
  junk <- capture.output({x <- try(httr2::req_perform(x), silent = TRUE)})
  if (inherits(x, "try-error")) {
    return(FALSE)
  }
  if (!("status_code" %in% names(x))) {
    return(FALSE)
  }
  if (x$status_code != 200) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
################################################### #################################################### #

#' utility to make html link from URL
#'
#' Convert URL to HTML link that opens in new tab
#'
#' @param url string that is URL
#' @param text string that is label
#'
#' @return url_linkify('epa.gov','EPA') returns `"<a href=\"epa.gov\", target=\"_blank\">EPA</a>"`
#'
#' @keywords internal
#' @export
#'
url_linkify <- function(url, text) {

  if (missing(text)) {text = gsub(pattern = "http[s]?://","",url)}
  paste0('<a href=\"', URLencode(url), '\", target=\"_blank\">', text, '</a>')

  # Consider instead using something like golem utility enurl()
  #
  #   enurl <- function(url, text) {tags$a(href = url, text)}
}
################################################### #################################################### #

# convert EJAM html versions of weblinks back to simple URLs
# in the output tables from ejamit or doaggregate

unlinkify = function(x) {

  unlinkify_column <- function(z) {gsub('.*https', 'https', gsub('=report.*', '=report', gsub('., target.*', '', as.vector(unlist(z))))) }
  if (NCOL(x) > 1) {
    fixed = lapply(x, unlinkify_column)
  } else {
    fixed = unlinkify_column(x)
  }
  if (is.data.table(x)) {return(as.data.table(fixed))}
  if (is.data.frame(x)) {return(data.frame(fixed))}
  return(fixed)
}
# test_vec = testoutput_ejamit_10pts_1miles$results_bysite$`EJScreen Report`
# test_df1 = as.data.frame(testoutput_ejamit_10pts_1miles$results_bysite[ , 1])
# test_df2 = as.data.frame(testoutput_ejamit_10pts_1miles$results_bysite[ , 1:2])
# test_dt1 = testoutput_ejamit_10pts_1miles$results_bysite[ , 1]
# test_dt2 = testoutput_ejamit_10pts_1miles$results_bysite[ , 1:2]
#
# unlinkify(test_df1[1,1])
# unlinkify(test_vec); class(unlinkify(test_vec))
# unlinkify(test_df1); class(unlinkify(test_df1))
# unlinkify(test_dt1); class(unlinkify(test_dt1))
# unlinkify(test_df2); class(unlinkify(test_df2))
# unlinkify(test_dt2); class(unlinkify(test_dt2))
################################################### #################################################### #

#' utility to prep URLs for being written to Excel
#'
#' @param urls vector of urls such as from [url_ejscreen_report()]
#' @param urltext The text to appear in Excel cells instead of just the URL showing
#'
#' @details
#'   See table_xls_format()
#'
#'   Works best if using [openxlsx::writeData()] not [openxlsx::write.xlsx()]
#'
#'   To write this column of urls to a worksheet:
#'   ```
#'   lat <- c(30.977402, 32.515813); lon = c(-83.368997, -86.377325)
#'   radius <- 1
#'   urls <- url_ejscreenmap(lat=lat, lon=lon, radius=radius)
#'
#'   urlx <- EJAM:::url_xl_style(urls, urltext = paste0("Report ", 1:2))
#'
#'   wb <- openxlsx::createWorkbook()
#'   openxlsx::addWorksheet(wb, sheetName = 'tab1')
#'   openxlsx::writeData(wb, sheet = 1, x = urlx, startCol = 1, startRow = 2)
#'   openxlsx::saveWorkbook(wb, file = '~/test1.xlsx', overwrite = TRUE)
#'
#'   # using just [openxlsx::write.xlsx()] is simpler but ignores the urltext param:
#'   openxlsx::write.xlsx(data.frame(lat = lat, lon = lon, urlx), file = 'test2.xlsx')
#'   ```
#'
#' @keywords internal
#'
url_xl_style <- function(urls, urltext = urls) {

  x <- urls
  names(x) <- urltext
  class(x) <- 'hyperlink'
  return(x)
}
################################################### #################################################### #

# simplify by removing unused / empty parameters

url_drop_empty_parameters = function(quer) {

  # simplify by removing unused / empty parameters
  quer =  gsub("[^=&]*=&", "", x = quer) # drop any empty one except first or last param
  quer = gsub("?[^&=]*=&", "?", x = quer) # drop any empty one at start
  quer = gsub("&[^&=]*=$", "", x = quer) # drop any empty one at end
  return(quer)
}
