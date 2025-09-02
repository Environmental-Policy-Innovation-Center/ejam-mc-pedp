################################################### #################################################### #

# This file has functions that generate URLs that are links to useful info like reports via API
# or at a webpage (usually via "deep-linking" like a specific place on a map or page with a report on one site or county).

# Check for which API is available within each url_xyz function?

# LIST OF FUNCTIONS HERE ####
#
#   see outline via ctrl-shift-O
#   also see URL_*.R and url_*.R

## site-specific reports ####
################################################### #################################################### #
## NOTES ON URL LINKS/ SITES / REPORTS:

# > reportinfo = EJAM:::global_or_param("default_reports")
# data.frame(header = sapply(reportinfo, function(x) x$header), text = sapply(reportinfo, function(x) x$text))
#                header         text
# 1         EJAM Report       Report
# 2        EJSCREEN Map     EJSCREEN
# 3         ECHO Report         ECHO
# 4          FRS Report          FRS
# 5 Enviromapper Report Enviromapper
# 6       County Health Report       County
# 7       State Health Report       State

# > rm(reportinfo)

# ECHO reports on facilities:
# [url_echo_facility()]
# <https://echo.epa.gov/tools/web-services>
# browseURL("https://echo.epa.gov/tools/web-services")
# browseURL("https://echo.epa.gov/detailed-facility-report?fid=110068700043#")
# paste0("https://echo.epa.gov/detailed-facility-report?fid=", regid, "#")

# FRS reports on facilities:
# [url_frs_facility()]
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
#'
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param ifna URL shown for missing, NA, NULL, bad input values
#' @param baseurl do not change unless endpoint actually changed
#' @param ... unused
#'
#' @return URL(s)
#' @seealso  [url_ejamapi()]  [url_ejscreenmap()]
#'   [url_echo_facility()] [url_frs_facility()]  [url_enviromapper()]
#'
#' @export
#'
url_ejscreenmap <- function(sitepoints = NULL, lat = NULL, lon = NULL,
                            shapefile = NULL,
                            fips = NULL, wherestr = NULL,
                            as_html = FALSE,
                            linktext = "EJSCREEN",
                            ifna = "https://pedp-ejscreen.azurewebsites.net/index.html",
                            baseurl = "https://pedp-ejscreen.azurewebsites.net/index.html",
                            ...) {
  if (is.null(linktext)) {linktext <- paste0("EJSCREEN")}

  ##   linktext could also} be numbered:
  # linktext = paste0("EJScreen Map ", 1:NROW(sitepoints))

  if (!is.null(shapefile)) {
    if ("INTPTLAT" %in% names(shapefile) & "INTPTLON" %in% names(shapefile)) {
      sitepoints = data.frame(lat = shapefile$INTPTLAT, lon = shapefile$INTPTLON)
    } else {
      # at least get points that are coordinates of centroids of polygons if cannnot open ejscreen app showing the actual polygon
      suppressWarnings({
        sitepoints = sf::st_coordinates(sf::st_centroid(shapefile) )
      })
      colnames(sitepoints) <- c("lon", "lat")
      sitepoints = as.data.frame(sitepoints)
    }
  }
  if (!is.null(sitepoints)) {
    lat = sitepoints$lat
    lon = sitepoints$lon
  }

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
      urlx <- rep('https://ejanalysis.org', length(urlx))
      if (as_html) {
        urlx <- URLencode(urlx)
        urlx <- url_linkify(urlx, text = linktext)
      }
      return(urlx)
    }
  }

  baseurl_query <- paste0(baseurl, "?wherestr=")
  whereq <- ""
  if (!is.null(lat)) {
    whereq <- paste( lat,  lon, sep = ',') # points (or centroids of polygons)
    whereq[is.na(lat) | is.na(lon)] <- NA
  }
  if (!is.null(fips) && is.null(wherestr)) {
    wherestr <- fips2name(fips) # fips-based
    wherestr[is.na(fips)] <- NA
  }
  if (!is.null(wherestr) && is.null(lat)) {
    whereq <- wherestr # name-based not latlon-based
  }
  urlx <- paste0(baseurl_query, whereq)

  ok <- !is.na(whereq)

  urlx[!ok] <- ifna
  ok <- !is.na(urlx)  # now !ok mean it was a bad input and also  ifna=NA
  if (as_html) {
    urlx[ok] <- URLencode(urlx[ok]) # consider if we want  reserved = TRUE
    urlx[ok] <- url_linkify(urlx[ok], text = linktext)
  }
  urlx[!ok] <- ifna # only use non-linkified ifna for the ones where user set ifna=NA and it had to use ifna
  return(urlx)
}
################################################### #################################################### #

#' Get URLs of ECHO reports
#'
#' Get URL(s) for EPA ECHO webpage with facility information
#'
#' @param regid EPA FRS Registry ID
#' @param validate_regids if set TRUE, returns NA where a regid is not found in the FRS dataset that is
#'   currently being used by this package (which might not be the very latest from EPA).
#'   If set FALSE, faster since avoids checking but some links might not work and not warn about bad regid values.
#'
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param ifna URL shown for missing, NA, NULL, bad input values
#' @param baseurl do not change unless endpoint actually changed
#' @param ... unused
#'
#' @seealso  [url_ejamapi()]  [url_ejscreenmap()]
#'   [url_echo_facility()] [url_frs_facility()]  [url_enviromapper()]
#'
#' @seealso  [url_ejamapi()]  [url_ejscreenmap()]
#'   [url_echo_facility()] [url_frs_facility()]  [url_enviromapper()]
#' @return URL(s)
#' @examples  \donttest{
#'  browseURL(url_echo_facility(110070874073))
#'  }
#'
#' @export
#'
url_echo_facility <- function(regid = NULL,
                              validate_regids = TRUE,
                              as_html = FALSE,
                              linktext = "ECHO", # regid,
                              ifna = "https://echo.epa.gov",
                              baseurl = "https://echo.epa.gov/detailed-facility-report?fid=",
                              ...) {
  if (is.null(linktext)) {linktext <- paste0("ECHO")}

  if (is.null(regid) || length(regid) == 0) {
    urlx <- ifna
    return(urlx) # length is 0
  }
  if (!is.vector(regid)) {
    warning("regid should be a vector")
    urlx <- rep(ifna, NROW(regid))
  } else {
    ok <- regids_seem_ok(regid)
    urlx <- rep(ifna, NROW(regid))
    urlx[ok] <- paste0(baseurl, regid[ok])
  }
  ok <- !is.na(regid)

  if (validate_regids) {
    if (!exists("frs")) {
      dataload_dynamic("frs")
    }
    bad_id = !(regid %in% frs$REGISTRY_ID)
    urlx[bad_id] <- ifna
    if (any(bad_id)) {
      warning("Some regid were not found in frs dataset being used by the package")
    }
  }

  urlx[!ok] <- ifna
  ok <- !is.na(urlx)  # now !ok mean it was a bad input and also  ifna=NA
  if (as_html) {
    urlx[ok] <- URLencode(urlx[ok]) # consider if we want  reserved = TRUE
    urlx[ok] <- url_linkify(urlx[ok], text = linktext)
  }
  urlx[!ok] <- ifna # only use non-linkified ifna for the ones where user set ifna=NA and it had to use ifna
  return(urlx)
}
################################################### #################################################### #

#' Get URLs of FRS reports
#'
#' Get URL(s) for reports on facilities from EPA FRS (facility registry service)
#'
#' @param regid one or more EPA FRS Registry IDs.
#' @param validate_regids if set TRUE, returns NA where a regid is not found in the FRS dataset that is
#'   currently being used by this package (which might not be the very latest from EPA).
#'   If set FALSE, faster since avoids checking but some links might not work and not warn about bad regid values.
#'
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param ifna URL shown for missing, NA, NULL, bad input values
#' @param baseurl do not change unless endpoint actually changed
#' @param ... unused
#'
#' @seealso  [url_ejamapi()]  [url_ejscreenmap()]
#'   [url_echo_facility()] [url_frs_facility()]  [url_enviromapper()]
#'
#' @return URL(s)
#' @examples
#' x = url_frs_facility(testinput_regid)
#' \donttest{
#' browseURL(x[1])
#' }
#' url_frs_facility(testinput_registry_id)
#'
#' @export
#'
url_frs_facility <- function(regid = NULL,
                             validate_regids = FALSE,
                             as_html = FALSE,
                             linktext = "FRS",
                             ifna = "https://www.epa.gov/frs",
                             baseurl = "https://frs-public.epa.gov/ords/frs_public2/fii_query_dtl.disp_program_facility?p_registry_id=",
                             ...) {
  if (is.null(linktext)) {linktext <- paste0("FRS")}

  # both of these URLs seem to work:
  #baseurl <- "https://ofmpub.epa.gov/frs_public2/fii_query_dtl.disp_program_facility?p_registry_id="
  # baseurl = "https://frs-public.epa.gov/ords/frs_public2/fii_query_dtl.disp_program_facility?p_registry_id="

  if (is.null(regid) || length(regid) == 0) {
    urlx <- ifna
    return(urlx) # length is 0
  }
  if (!is.vector(regid)) {
    warning("regid should be a vector")
    urlx <- rep(ifna, NROW(regid))
  } else {
    ok <- regids_seem_ok(regid)
    urlx <- rep(ifna, NROW(regid))
    urlx[ok] <- paste0(baseurl, regid[ok])
  }
  ok <- !is.na(regid)

  if (validate_regids) {
    if (!exists("frs")) {
      dataload_dynamic("frs")
    }
    # bad_id = !(regid %in% frs$REGISTRY_ID)
    bad_id = !regids_valid(regid)
    urlx[bad_id] <- ifna
  }

  urlx[!ok] <- ifna
  ok <- !is.na(urlx)  # now !ok mean it was a bad input and also  ifna=NA
  if (as_html) {
    urlx[ok] <- URLencode(urlx[ok]) # consider if we want  reserved = TRUE
    urlx[ok] <- url_linkify(urlx[ok], text = linktext)
  }
  urlx[!ok] <- ifna # only use non-linkified ifna for the ones where user set ifna=NA and it had to use ifna
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
#' @param shapefile if provided function uses centroids of polygons for lat lon
#' @param fips ignored
#' @param zoom initial map zoom extent
#'
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param ifna URL shown for missing, NA, NULL, bad input values
#' @param baseurl do not change unless endpoint actually changed
#' @param ... unused
#'
#' @seealso  [url_ejamapi()]  [url_ejscreenmap()]
#'   [url_echo_facility()] [url_frs_facility()]  [url_enviromapper()]
#'
#' @return URL of one webpage (that launches the mapping tool)
#' @examples
#' x = url_enviromapper(testpoints_10[1,])
#' \dontrun{
#'  browseURL(x)
#'  browseURL(url_enviromapper(lat = 38.895237, lon = -77.029145, zoom = 17))
#' }
#'
#' @export
#'
url_enviromapper <- function(sitepoints = NULL, lon = NULL, lat = NULL, shapefile = NULL, fips = NULL,
                             zoom = 13,
                             as_html = FALSE,
                             linktext = "Enviromapper",
                             ifna = "https://geopub.epa.gov/myem/efmap/",
                             baseurl = "https://geopub.epa.gov/myem/efmap/index.html?ve=",
                             ...) {

  if (is.null(linktext)) {linktext <- paste0("Enviromapper")}

  # "https://geopub.epa.gov/myem/efmap/index.html?ve=13,38.895237,-77.029145"

  if (!is.null(fips) && is.null(sitepoints)) {
    # get lat,lon from fips
    # sitepoints <- latlon_from_fips(fips)
  }

  if (!is.null(shapefile) && is.null(sitepoints)) {
    # at least get points that are coordinates of centroids of polygons if cannnot open ejscreen app showing the actual polygon
    if ("INTPTLAT" %in% names(shapefile) & "INTPTLON" %in% names(shapefile)) {
      sitepoints = data.frame(lat = shapefile$INTPTLAT, lon = shapefile$INTPTLON)
    } else {
      suppressWarnings({
        sitepoints = sf::st_coordinates(sf::st_centroid(shapefile) )
      })
      colnames(sitepoints) <- c("lon", "lat")
      sitepoints = as.data.frame(sitepoints)
    }
  }
  if (!is.null(sitepoints)) {
    lat = sitepoints$lat
    lon = sitepoints$lon
  }
  if (is.null(lat) || is.null(lon) || length(lat) == 0 || length(lon) == 0) {
    urlx <- ifna
    return(urlx) # length is 0
    # or # return(NULL)
  }

  urlx <- paste0(baseurl, zoom, ",", lat, ",", lon)
  ok <- !(is.na(lat) | is.na(lon))

  urlx[!ok] <- ifna
  ok <- !is.na(urlx)  # now !ok mean it was a bad input and also  ifna=NA
  if (as_html) {
    urlx[ok] <- URLencode(urlx[ok]) # consider if we want  reserved = TRUE
    urlx[ok] <- url_linkify(urlx[ok], text = linktext)
  }
  urlx[!ok] <- ifna # only use non-linkified ifna for the ones where user set ifna=NA and it had to use ifna
  return(urlx)
}
################################################### #################################################### #

#' URL functions - Get URLs of useful report(s) on States containing the given fips, from countyhealthrankings.org
#'
#' @inheritParams url_countyhealth
#' @param fips vector of fips codes of States, 2 characters each, like "10",
#'   or other fips codes like blockgroups in which case it returns info for parent States
#' @param sitepoints if provided and fips is NULL, gets county fips from lat,lon columns of sitepoints
#' @param shapefile if provided and fips is NULL, gets county fips from lat,lon of polygon centroid
#'
#' @return vector of URLs
#' @examples
#' x = url_statehealth(fips_state_from_state_abbrev(c("DE", "GA", "MS")))
#' \dontrun{
#' browseURL(x)
#' }
#' @export
#'
url_statehealth <- function(fips = NULL, year = 2025,
                            sitepoints = NULL,
                            shapefile = NULL,
                            as_html = FALSE,
                            linktext = "State", # "State Health Report",
                            ifna = "https://www.countyhealthrankings.org",
                            baseurl = "https://www.countyhealthrankings.org/health-data/",
                            ...
) {

  if (is.null(fips) && !is.null(shapefile)) {
    sitepoints = as.data.frame(sf::st_coordinates(sf::st_centroid(  shapefile )))
    ST = state_from_latlon(lat = sitepoints$Y, lon = sitepoints$X)$ST
    fips <- fips_state_from_state_abbrev(ST)
  }
  if (is.null(fips) && !is.null(sitepoints)) {
    sitepoints = sitepoints_from_any(sitepoints)
    ST = state_from_latlon(lat = sitepoints$lat, lon = sitepoints$lon)$ST
    fips <- fips_state_from_state_abbrev(ST)
  }

  url_countyhealth(
    fips = fips2state_fips(fips),
    year = year,
    # sitepoints = sitepoints,
    # shapefile = shapefile,
    as_html = as_html,
    linktext = linktext,
    ifna = ifna,
    baseurl = baseurl,
    ... = ...
  )
}
################################################### #################################################### #


#' URL functions - Get URLs of useful report(s) on Counties containing the given fips, from countyhealthrankings.org
#'
#' @param fips vector of fips codes of counties, 5 characters each, like "10003",
#'   or other fips codes like blockgroups in which case it returns info for parent counties
#' @param year e.g., 2025
#' @param sitepoints if provided and fips is NULL, gets county fips from lat,lon columns of sitepoints
#' @param shapefile if provided and fips is NULL, gets county fips from lat,lon of polygon centroid
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param ifna URL shown for missing, NA, NULL, bad input values
#' @param baseurl do not change unless endpoint actually changed
#' @param ... unused
#'
#' @return vector of URLs
#' @examples
#'  url_countyhealth(fips_counties_from_state_abbrev("DE"))
#'  # browseURL(url_countyhealth(fips_counties_from_state_abbrev("DE"))[1])
#'
#' @export
#'
url_countyhealth <- function(fips = NULL, year = 2025,
                             sitepoints = NULL,
                             shapefile = NULL,
                             as_html = FALSE,
                             linktext = "County", # "County Health Report",
                             ifna = "https://www.countyhealthrankings.org",
                             baseurl = "https://www.countyhealthrankings.org/health-data/",
                             ...) {

  if (is.null(fips) && !is.null(shapefile)) {
    sitepoints = as.data.frame(sf::st_coordinates(sf::st_centroid(  shapefile )))
    sitepoints$lat = sitepoints$Y
    sitepoints$lon = sitepoints$X
    fips = fips_county_from_latlon(sitepoints = sitepoints)
  }
  if (is.null(fips) && !is.null(sitepoints)) {
    sitepoints = sitepoints_from_any(sitepoints)
    fips = fips_county_from_latlon(sitepoints = sitepoints)
  }

  if (is.null(linktext)) {linktext <- paste0("County")}
  if (  is.null(fips) || length(fips) == 0) {
    return(ifna)
    # return(NULL)
  }
  # Could check if site or API is available?

  if (missing(year) && year != as.numeric(substr(Sys.Date(), 1, 4))) {
    warning("default year is ", year, " but newer data might be available")
  }
  is.state <- fipstype(fips) %in% "state"

  url_countyhealth_not_statefips = function(fips, year, as_html) {
    if (is.null(fips) || length(fips) == 0) {return(NULL)}
    fips <- fips2countyfips(fips)
    statename  <- tolower(EJAM::fips2statename( fips))
    countyname <- tolower(EJAM::fips2countyname(fips, includestate = ""))
    countyname <- trimws(gsub(" county", "", countyname))
    countyname <- gsub(" ", "-", countyname)
    # https://www.countyhealthrankings.org/health-data/maryland/montgomery?year=2023
    # baseurl = "https://www.countyhealthrankings.org/health-data/"
    urlx <- paste0(baseurl, statename, "/", countyname, "?year=", year)
    urlx[is.na(fips) | is.na(countyname)] <- NA
    return(urlx)
  }
  url_countyhealth_statefips <- function(fips, year, as_html) {
    if (is.null(fips) || length(fips) == 0) {return(NULL)}
    statename <- tolower(fips2statename(fips))
    statename <- gsub(" ", "-", statename)
    # baseurl = "https://www.countyhealthrankings.org/health-data/"
    urlx <- paste0(baseurl, statename, "?year=", year)
    urlx[is.na(statename)] <- NA
    return(urlx)
  }

  urlx <- rep(ifna, length(fips))
  if (any(is.state)) {
    urlx[is.state]  <- url_countyhealth_statefips(    fips[is.state], year = year, as_html = as_html)
  }
  if (any(!is.state)) {
    urlx[!is.state] <- url_countyhealth_not_statefips(fips[!is.state], year = year, as_html = as_html)
  }

  ok <- !is.na(fips)

  urlx[!ok] <- ifna
  ok <- !is.na(urlx)  # now !ok mean it was a bad input and also  ifna=NA
  if (as_html) {
    urlx[ok] <- URLencode(urlx[ok]) # consider if we want  reserved = TRUE
    urlx[ok] <- url_linkify(urlx[ok], text = linktext)
  }
  urlx[!ok] <- ifna # only use non-linkified ifna for the ones where user set ifna=NA and it had to use ifna
  return(urlx)
}
######################################################################### #
## not site-specific ####
######################################################################### #

# . -------------------------- ####


#' URL functions - url_naics.com - Get URL for page with info about industry sectors by text query term
#' @details
#' See (https://naics.com) for more information on NAICS codes.
#'
#' Unlike url_xyz() functions, which provide a unique link for each site,
#' this url_ function provides just a link for a whole industry or set of industries based on a query,
#' so it is not meant to be used in a column of site by site results the way the other url_xyz() functions are.
#'
#' @param query string query term like "gasoline" or "copper smelting"
#'
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param ifna URL shown for missing, NA, NULL, bad input values
#' @param baseurl do not change unless endpoint actually changed
#' @param ... unused
#'
#' @return URL as string
#'
#' @export
#'
url_naics.com <- function(query,
                          as_html = FALSE,
                          linktext = query, # "NAICS.com",
                          ifna = "https://www.naics.com",
                          baseurl = "https://www.naics.com/code-search/?trms=",
                          ...) {




  # Could check if site or API is available?

  query <- gsub(" ", "+", query)
  urlx = paste0(baseurl, query, "&v=2017&styp=naics")

  if (as_html) {
    if (is.null(linktext)) {linktext <- query}
    urlx <- url_linkify(urlx, text = linktext)
  }
  return(urlx)
}
######################################################################### #
# . ####

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
