

#' utility - check if EJSCREEN API seems to be online or offline (went down 1/2025)
#' @param url DEFAULT is the 2024 base url of the EJSCREEN API
#' @returns TRUE or FALSE (but NA if no internet connection seems to be available at all)
#'
#' @keywords internal
#'
ejscreenapi_online <- function(url = "https://ejscreen.epa.gov") {
  url_online(url)
}
######################################## #


#' Add Links to ejscreenapi output, to launch EJSCREEN report for given point(s)
#' Used only by pre-Jan-2025 ejscreenapi-related code, not EJAM-related API of 2025+
#' @description Add or update, and reorder, columns with results
#' @details Was used for [ejscreenapi_plus()] not for [ejamit()], to create weblinks to maps, for output table.
#'
#' 1. Adds weblinks in column.
#' 2. Adds a column to flag sites that are close to other sites, and
#' 3. Puts certain columns first.
#'
#' @param results_table from [ejscreenapi()] function for example
#' @seealso [url_columns_bysite()] for the EJAM version. [url_ejscreenmap()] [distance_near_eachother()]
#' @return the input table but with extra columns
#'
#' @keywords internal
#'
url_ejscreenapi_clusters_and_sort_cols <- function(results_table) {

  if (is.data.table(results_table)) {
    setDF(results_table)
    on.exit(setDT(results_table))
  }
  ########################################### #
  # Add columns with hyperlinks to site reports
  #
  ## 1. EJSCREEN REPORT URL = pdfurl ####
  #
  ## Fix existing link to pdf-like report
  # to make URL clickable in table, move to near 1st column,
  # NOTE: browser can print that report to pdf with margins = c(0.3, 0.3, 0.3, 1.75) # Left Top Right Bottom

  if ("areaid" %in% names(results_table)) {
    areaid   <- results_table$areaid
    lat = NULL
    lon = NULL
  } else {
    lon = results_table$lon
    lat = results_table$lat
    areaid   <- ""
  }
  if ("areatype" %in% names(results_table)) {
    areatype <- results_table$areatype
  } else {
    areatype <- ""
  }
  if ("namestr" %in% names(results_table)) {
    namestr  <- results_table$namestr
  } else {
    namestr  <- ""
  }
  pdfurl <- url_ejscreen_report(lon = lon, lat = lat, radius = results_table$distance,
                                areaid = areaid, areatype = areatype, namestr = namestr,
                                as_html = FALSE, linktext = "EJSCREEN Report")
  encodedlink <- URLencode( pdfurl)
  pdfurl <- paste0('<a href=\"', encodedlink, '\", target=\"_blank\">EJSCREEN Report ', rownames(results_table), '</a>')
  # (but does not work like that for csv/excel download)
  if ("pdfurl" %in% names(results_table) ) results_table$pdfurl <- NULL # gets recreated later below

  ## 2. EJSCREEN MAP URL = mapurl ####
  if (!all(areaid == '')) {
    mapurl <- url_ejscreenmap(wherestr = fips2name(areaid) )  # e.g.,  "https://ejscreen.epa.gov/mapper/index.html?wherestr=35.3827475,-86.2464592"
  } else {
    mapurl <- url_ejscreenmap(lat = lat, lon = lon  )  # e.g.,  "https://ejscreen.epa.gov/mapper/index.html?wherestr=35.3827475,-86.2464592"
  }

  mapurl  <- paste0('<a href=\"', mapurl, '\", target=\"_blank\">EJSCREEN Map ', rownames(results_table), '</a>')
  # (but does not work like that for csv/excel download)



  ########################################### #
  # Add column to flag sites that are near each other ####
  #
  # want this to reflect radius in this data run, not whatever user may have just changed it to for the next run, so do not use is_clustered()
  if (!is.null(lat)) {
    results_table$overlaps_another <- distance_near_eachother(lon = lon, lat = lat,
                                                              distance = 2 * results_table$distance) # not radius_miles() !
  } else {
    results_table$overlaps_another <- NA
  }
  ########################################### #
  # Re-order Columns ####
  #
  # "id"  "RAW_D_MINOR" "RAW_D_INCOME"  "totalPop" "distance" etc. are the output names at this point
  #  something like  "siteid"  "sitename"  "lon" "lat" may be uploaded

  firstcols <- c('id', 'distance', 'overlaps_another', 'totalPop') # these after the input points;  then the rest of the outputs

  results_table <- data.frame(

    a = pdfurl, #
    b = mapurl, #

    results_table[ , firstcols],
    results_table[, !(names(results_table) %in% firstcols)],
    stringsAsFactors = FALSE)
  names(results_table) <- gsub("^a$", "EJSCREEN Report", names(results_table))
  names(results_table) <- gsub("^b$", "EJSCREEN Map", names(results_table))

  ########################################### #

  return(results_table)
}
########################################### #  ########################################### #
