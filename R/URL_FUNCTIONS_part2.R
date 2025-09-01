################################################### #################################################### #

# This file has functions that generate URLs that are links to useful info like reports via API
# or at a webpage (usually via "deep-linking" like a specific place on a map or page with a report on one site or county).

# LIST OF FUNCTIONS HERE ####
#
#   see outline via ctrl-shift-O
#   also see URL_*.R and url_*.R


## NOTES ON URL LINKS/ SITES / REPORTS:

# ECHO reports on facilities:
# [url_echo_facility_webpage()]
# <https://echo.epa.gov/tools/web-services>
# browseURL("https://echo.epa.gov/tools/web-services")
# browseURL("https://echo.epa.gov/detailed-facility-report?fid=110068700043#")
# paste0("https://echo.epa.gov/detailed-facility-report?fid=", regid, "#")

# FRS reports on facilities:
# [url_frs_report()]
# see also  https://www.epa.gov/frs/frs-rest-services  or https://www.epa.gov/frs
# browseURL("https://www.epa.gov/frs/frs-physical-data-model")
# browseURL("https://frs-public.epa.gov/ords/frs_public2/fii_query_detail.disp_program_facility?p_registry_id=110035807259")
# browseURL("https://www.epa.gov/frs/frs-query#industrial naics")

# EnviroMapper app webpage:
# [url_enviromapper()]

# Envirofacts API, data on facilities:
# browseURL("https://www.epa.gov/enviro/web-services")
# browseURL("https://www.epa.gov/enviro/envirofacts-data-service-api")
# <https://www.epa.gov/enviro/web-services> and
# <https://www.epa.gov/enviro/envirofacts-data-service-api>

# EJScreen report on a location

# [url_ejscreen_report()]

################################################### #################################################### #


#' Get URL(s) for (new) EJSCREEN app with map centered at given point(s)
#'
#' @param sitepoints data.frame with colnames lat, lon (or lat, lon parameters can be provided separately)
#' @param lon one or more longitudes
#' @param lat one or more latitudes
#' @param fips The FIPS code of a place to center map on (blockgroup, tract, city/cdp, county, state FIPS).
#'   It gets translated into the right wherestr parameter if fips is provided.
#'
#' @param wherestr If fips and sitepoints (or lat and lon) are not provided,
#'   wherestr should be the street address, zip code, or place name (not FIPS code!).
#'
#'   Note that nearly half of all county fips codes are impossible to distinguish from
#'   5-digit zipcodes because the same numbers are used for both purposes.
#'
#'   For zipcode 10001, use url_ejscreenmap(wherestr =  '10001')
#'
#'   For County FIPS code 10001, use url_ejscreenmap(fips = "10001")
#'
#'   This parameter is passed to the API as wherestr= , if point and fips are not specified.
#'
#'   Can be State abbrev like "NY" or full state name,
#'   or city like "New Rochelle, NY" as from fips2name() -- using fips2name()
#'   works for state, county, or city FIPS code converted to name,
#'   but using the fips parameter is probably a better idea.
#'
#' @param shapefile shows URL of a EJSCREEN app map centered on the centroid of a given polygon,
#'   but does not actually show the polygon.
#' @param ... unused
#' @return URL(s)
#' @seealso  [url_ejscreen_report()]  [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]
#'
#' @export
#'
url_ejscreenmap <- function(sitepoints = NULL, lat = NULL, lon = NULL,
                            shapefile = NULL,
                            fips = NULL, wherestr = NULL,
                            as_html = FALSE, linktext = "EJSCREEN Map",
                            baseurl = "https://pedp-ejscreen.azurewebsites.net/index.html", ...) {

  if (!is.null(shapefile)) {
    # at least get points that are coordinates of centroids of polygons if cannnot open ejscreen app showing the actual polygon
    sitepoints = sf::st_coordinates(sf::st_centroid(shapefile) )
    colnames(sitepoints) <- c("lon", "lat")
  }
  if (!is.null(sitepoints)) {
    lat = sitepoints$lat
    lon = sitepoints$lon
  }

  ##   linktext could also} be numbered:
  # linktext = paste0("EJScreen Map ", 1:NROW(sitepoints))

  #"https://ejanalysis.com/ejscreenapp"
  # redirects to current homepage of live app
  ### use newer api via new baseurl
  # old EPA API:
  # https://ejscreen.epa.gov/mapper/index.html?wherestr=30.450000,-91.090000
  # baseurl <- 'https://ejscreen.epa.gov/mapper/index.html'
  # 8/25 new nonEPA API:
  # baseurl <- "https://pedp-ejscreen.azurewebsites.net/index.html"
  ## if both down, return NA values
  if (EJAM:::global_or_param("ejscreen_is_down")) {
    if (EJAM:::global_or_param("ejamapi_is_down")) {
      urlx <- rep(NA, length(urlx))
      return(urlx)
    }
  }

  baseurl_query <- paste0(baseurl, "?wherestr=")
  whereq <- ""
  if (!is.null(lat)) {
    whereq <- paste( lat,  lon, sep = ',') # points (or centroids of polygons)
  }
  if (!is.null(fips) && is.null(wherestr)) {
    wherestr <- fips2name(fips) # fips-based
  }
  if (!is.null(wherestr) && is.null(lat)) {
    whereq <- wherestr # name-based not latlon-based
  }
  urlx <- paste0(baseurl_query, whereq)

  if (as_html) {
    urlx <- URLencode(urlx)
    urlx <- url_linkify(urlx, text = linktext)
  }
  return(urlx)
}
################################################### #################################################### #


#' Get URLs of ECHO reports
#'
#' Get URL(s) for EPA ECHO webpage with facility information
#'
#' @param regid EPA FRS Registry ID
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param validate_regids if set TRUE, returns NA where a regid is not found in the FRS dataset that is
#'   currently being used by this package (which might not be the very latest from EPA).
#'   If set FALSE, faster since avoids checking but some links might not work and not warn about bad regid values.
#' @param ... unused
#' @seealso  [url_ejscreen_report()]  [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]
#'
#' @seealso  [url_ejscreen_report()]  [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]
#' @return URL(s)
#' @examples  \donttest{
#'  browseURL(url_echo_facility_webpage(110070874073))
#'  }
#'
#' @export
#'
url_echo_facility_webpage <- function(regid, as_html=FALSE, linktext = regid, validate_regids = FALSE, ...) {

  # do error checking here
  if (!is.atomic(regid))
    baseurl <- "https://echo.epa.gov/detailed-facility-report?fid="
  urlx <-  paste0(baseurl, regid)
  if (as_html) {
    urlx = URLencode(urlx)
    if (is.null(linktext)) {linktext <- regid}
    urlx <- url_linkify(urlx, text = linktext)
  }
  if (validate_regids) {
    bad_id = !(regid %in% frs$REGISTRY_ID)
    urlx[bad_id] <- NA
    if (any(bad_id)) {
      warning("Some regid were not found in frs dataset being used by the package")
    }
  }
  return(urlx)
}
################################################### #################################################### #


#' Get URLs of FRS reports
#'
#' Get URL(s) for reports on facilities from EPA FRS (facility registry service)
#'
#' @param regid one or more EPA FRS Registry IDs.
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param validate_regids if set TRUE, returns NA where a regid is not found in the FRS dataset that is
#'   currently being used by this package (which might not be the very latest from EPA).
#'   If set FALSE, faster since avoids checking but some links might not work and not warn about bad regid values.
#' @param ... unused
#' @seealso  [url_ejscreen_report()]  [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]
#'
#' @return URL(s)
#' @examples
#' x = url_frs_report(testinput_regid)
#' \donttest{
#' browseURL(x[1])
#' }
#' url_frs_report(testinput_registry_id)
#'
#' @export
#'
url_frs_report <- function(regid, as_html = FALSE, linktext = "FRS", validate_regids = FALSE, ...) {

  # both of these URLs seem to work:
  #baseurl <- "https://ofmpub.epa.gov/frs_public2/fii_query_dtl.disp_program_facility?p_registry_id="
  baseurl <- "https://frs-public.epa.gov/ords/frs_public2/fii_query_dtl.disp_program_facility?p_registry_id="

  urlx <- paste0(baseurl, regid)
  if (as_html) {
    urlx = URLencode(urlx)
    if (is.null(linktext)) {linktext <- "FRS"}
    urlx <- url_linkify(urlx, text = linktext)
  }
  if (validate_regids) {
    bad_id = !(regid %in% frs$REGISTRY_ID)
    urlx[bad_id] <- NA
    if (any(bad_id)) {
      warning("Some regid were not found in frs dataset being used by the package")
    }
  }
  return(urlx)
}
################################################### #################################################### #


#' Get URLs of EnviroMapper reports
#'
#' Get URL(s) for EnviroMapper web-based tool, to open map at specified point location(s)
#'
#' @details EnviroMapper lets you view EPA-regulated facilities and other information on a map, given the lat,lon
#'
#' @param sitepoints data.frame with colnames lat, lon (or lat, lon parameters can be provided separately)
#' @param lon one longitude
#' @param lat one latitude
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param zoom initial map zoom extent, with smaller numbers if zoomed in closer
#' @param ... unused
#' @seealso  [url_ejscreen_report()]  [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]
#'
#' @return URL of one webpage (that launches the mapping tool)
#' @examples
#' x = url_enviromapper(lat = 38.895237, lon = -77.029145)
#' \dontrun{
#'  browseURL(x)
#' }
#'
#' @export
#'
url_enviromapper <- function(sitepoints = NULL, lon = NULL, lat = NULL, as_html = FALSE, linktext = "EnviroMapper", shapefile = NULL, fips = NULL, zoom=13, ...) {

  if (!is.null(shapefile)) {
    # at least get points that are coordinates of centroids of polygons if cannnot open ejscreen app showing the actual polygon
    sitepoints = sf::st_coordinates(sf::st_centroid(shapefile) )
    colnames(sitepoints) <- c("lon", "lat")
  }
  if (!is.null(sitepoints)) {
    lat = sitepoints$lat
    lon = sitepoints$lon
  }

  # "https://geopub.epa.gov/myem/efmap/index.html?ve=13,38.895237,-77.029145"

  baseurl <- "https://geopub.epa.gov/myem/efmap/index.html?ve="

  urlx <- paste0(baseurl, zoom, ",", lat, ",", lon)
  if (as_html) {
    urlx = URLencode(urlx)
    if (missing(linktext) || is.null(linktext)) {linktext <- paste0("EnviroMapper")}
    urlx <- url_linkify(urlx, text = linktext)
  }
  return(urlx)
}
################################################### #################################################### #


#' URL functions - Get URLs of useful report(s) on Counties from countyhealthrankings.org
#'
#' @param fips vector of fips codes of counties, 5 characters each, like "10003"
#' @param year e.g., 2025
#'
#' @return vector of URLs
#' @examples
#'  url_countyhealthrankings(fips_counties_from_state_abbrev("DE"))
#'  # browseURL(url_countyhealthrankings(fips_counties_from_state_abbrev("DE"))[1])
#'
#' @export
#'
url_countyhealthrankings <- function(fips, year = 2025, as_html = FALSE, ...) {

  # Could check if site or API is available?

  if (missing(year) && year != as.numeric(substr(Sys.Date(), 1, 4))) {
    warning("default year is ", year, " but newer data might be available")
  }
  if (missing(fips)) {
    return("https://www.countyhealthrankings.org")
  }
  fips = fips2countyfips(fips)
  statename  <- tolower(EJAM::fips2statename( fips))
  countyname <- tolower(EJAM::fips2countyname(fips, includestate = ""))
  countyname <- trimws(gsub(" county", "", countyname))
  countyname <- gsub(" ", "-", countyname)
  # https://www.countyhealthrankings.org/explore-health-rankings/maryland/montgomery?year=2023
  baseurl <- "https://www.countyhealthrankings.org/explore-health-rankings/"
  urlx <- paste0(baseurl, statename, "/", countyname, "?year=", year)

  return(urlx)
}
######################################################################### #

#' URL functions - url_naics.com - Get URL for page with info about industry sectors by text query term
#'
#' See (https://naics.com) for more information on NAICS codes
#'
#' @param query string query term like "gasoline" or "copper smelting"
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @return URL as string
#'
#' @export
#'
url_naics.com <- function(query, as_html=FALSE, linktext) {

  # Could check if site or API is available?

  query <- gsub(" ", "+", query)
  urlx = paste0("https://www.naics.com/code-search/?trms=", query, "&v=2017&styp=naics")
  if (as_html) {
    if (missing(linktext)) {linktext <- query}  #   paste0("EJScreen Map ", 1:length(lon))
    urlx <- url_linkify(urlx, text = linktext)
  }
  return(urlx)
}
######################################################################### #


#' utility to view rendered .html file stored in a github repo
#'
#' @param ghurl URL of HTML file in a github repository
#' @param launch_browser set FALSE to get URL but not launch a browser
#'
#' @returns URL
#' @examples
#' url_github_preview(fold = "docs", file = "index.html", launch_browser = F)
#' url_github_preview(fold = "docs/reference", file = "ejam2excel.html", launch_browser = F)
#'
#' #   Compare versions of the HTML summary report:
#'
#' myfile = "testoutput_ejam2report_100pts_1miles.html"
#' \dontrun{
#' # in latest main branch on GH (but map does not render using this tool)
#' url_github_preview(file = myfile)
#'
#' # from a specific release on GH (but map does not render using this tool)
#' url_github_preview(ver = "v2.32.5", fold = "inst/testdata/examples_of_output", file = myfile)
#'
#' # local installed version
#' browseURL( system.file(file.path("testdata/examples_of_output", myfile), package="EJAM") )
#'
#' # local source package version in checked out branch
#' browseURL( file.path(testdatafolder(installed = F), "examples_of_output", myfile) )
#' }
#'
#' @keywords internal
#' @export
#'
url_github_preview = function(ghurl = NULL,
                              repo = "https://github.com/ejanalysis/EJAM",
                              blob = "blob",
                              ver = "main", # or "v2.32.5"
                              fold = "inst/testdata/examples_of_output", # or "docs/reference"
                              file = "testoutput_ejam2report_10pts_1miles.html",
                              launch_browser = TRUE
                              ) {

  if (is.null(ghurl)) {
    # repo = "https://github.com/ejanalysis/EJAM"
    # blob = "blob"
    # ver = "main" # or "v2.32.5"
    # fold = "inst/testdata/examples_of_output" # or "docs/reference"
    # file = "testoutput_ejam2report_10pts_1miles.html"

    ghurl <- file.path(repo, blob, ver, fold, file)
  }
  urlx <- paste0("http://htmlpreview.github.io/?", ghurl)

  if (launch_browser) {browseURL(urlx[1])}
  return(urlx)

}
######################################################################### #
