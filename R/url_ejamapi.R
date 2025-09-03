

#' Get URL(s) of HTML summary reports for use with EJAM-API
#' @details
#' - Work in progress - initial draft relied on API from
#'   https://github.com/edgi-govdata-archiving/EJAM-API
#'
#'   (see parameter `baseurl` that used the /report endpoint)
#'
#' - Another option in the future might be to construct a URL that is a link to the live EJAM
#'   app but has url-encoded parameters that are app settings, such as sitepoints, radius_default, etc.
#'
#' - Will try to use the same input parameters as [ejamit()] does.
#'
#' @param sitepoints see [ejamit()]
#' @param lat,lon can be provided as vectors of coordinates instead of providing sitepoints table
#' @param radius  see [ejamit()], default is 0 if fips or shapefile specified
#'
#' @param fips  see [ejamit()] but this initial version only works for a blockgroup FIPS!
#'
#' @param shapefile  see [ejamit()], but each polygon is encoded as geojson string
#'   which might get too long for encoding in a URL for the API using GET
#'
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param ifna URL shown for missing, NA, NULL, bad input values
#' @param baseurl do not change unless endpoint actually changed
#' @param ... unused
#'
#' @returns vector of character string URLs
#'
#' @export
#'
#' @examples
#'  pts = data.frame(lat=37.64122, lon=-122.41065)
#'  pts = data.frame(lat = c(37.64122, 43.92249), lon = c(-122.41065, -72.663705))
#'  x = url_ejamapi(sitepoints = pts)
#'  x = url_ejamapi(testpoints_10, radius = 3.1)
#'  x = url_ejamapi(system.file("testdata/latlon/testpoints_10.xlsx", package="EJAM"))
#'
#'  y = url_ejamapi(fips = c("050014801001", "050014802001"))
#'  y = url_ejamapi(fips = testinput_fips_blockgroups)
#'
#'  z = url_ejamapi(shapefile = testinput_shapes_2[2, c("geometry", "FIPS")])
#'
#'  \dontrun{
#'  browseURL("https://ejamapi-84652557241.us-central1.run.app/report?lat=33&lon=-112&buffer=4")
#'
#'  browseURL(x[1])
#'  browseURL(y[1])
#'  browseURL(z[1])
#' }
#'
url_ejamapi = function(

  sitepoints = NULL, lat = NULL, lon = NULL,
  radius = 3,

  ## unused so far:
  # radius_donut_lower_edge = 0,
  # maxradius = 31.07,
  # avoidorphans = FALSE,
  # quadtree = NULL, # not relevant

  fips = NULL,
  shapefile = NULL,

  ## unused so far:
  # countcols = NULL,
  # wtdmeancols = NULL,
  # calculatedcols = NULL,
  # calctype_maxbg = NULL,
  # calctype_minbg = NULL,
  # subgroups_type = "nh",
  # include_ejindexes = TRUE,
  # calculate_ratios = TRUE,
  # extra_demog = TRUE,
  # need_proximityscore = FALSE,
  # infer_sitepoints = FALSE,
  # need_blockwt = TRUE,

  # thresholds = list(80, 80),
  # threshnames = list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
  # threshgroups = list("EJ-US-or-ST", "Supp-US-or-ST"),

  # updateProgress = NULL,
  # updateProgress_getblocks = NULL,
  # progress_all = NULL,
  # in_shiny = FALSE,
  # quiet = TRUE,
  # silentinteractive = FALSE,
  # called_by_ejamit = TRUE,
  # testing = FALSE,
  # showdrinkingwater = TRUE,
  # showpctowned = TRUE,
  # download_city_fips_bounds = TRUE,
  # download_noncity_fips_bounds = FALSE,

  linktext = "Report",
  as_html = FALSE,
  ifna = "https://ejanalysis.com",
  baseurl = "https://ejamapi-84652557241.us-central1.run.app/report?",
  ...
) {

  ## unused so far:
  {
    xxx = "
  @param radius_donut_lower_edge
  @param maxradius
  @param avoidorphans
  @param quadtree

  @param countcols
  @param wtdmeancols
  @param calculatedcols
  @param calctype_maxbg
  @param calctype_minbg
  @param subgroups_type
  @param include_ejindexes
  @param calculate_ratios
  @param extra_demog
  @param need_proximityscore
  @param infer_sitepoints
  @param need_blockwt
  @param thresholds
  @param threshnames
  @param threshgroups
  @param updateProgress
  @param updateProgress_getblocks
  @param progress_all
  @param in_shiny
  @param quiet
  @param silentinteractive
  @param called_by_ejamit
  @param testing
  @param showdrinkingwater
  @param showpctowned
  @param download_city_fips_bounds
  @param download_noncity_fips_bounds
  "
  }

  if (is.null(linktext)) {linktext <- paste0("Report")}

  if (is.null(baseurl)) {
    baseurl <- "https://ejamapi-84652557241.us-central1.run.app/report?"
  }
  if (is.null(ifna)) {
    ifna <- "https://ejanalysis.com"
  }

  # see https://github.com/edgi-govdata-archiving/EJAM-API/tree/main
  # baseurl = "https://ejamapi-84652557241.us-central1.run.app/report?"
  # e.g.,
  # https://ejamapi-84652557241.us-central1.run.app/report?lat=33&lon=-112&buffer=4

  ###################################### #  shapefile ?
  if (!is.null(shapefile)) {
    if (missing(radius)) {radius <- 0}
    # geojson format
    # %7B"type"%3A"FeatureCollection"%2C"features"%3A%5B%7B"type"%3A"Feature"%2C"properties"%3A%7B%7D%2C"geometry"%3A%7B"coordinates"%3A%5B%5B%5B-112.01991856401462%2C33.51124624304089%5D%2C%5B-112.01991856401462%2C33.47010908826502%5D%2C%5B-111.95488826248605%2C33.47010908826502%5D%2C%5B-111.95488826248605%2C33.51124624304089%5D%2C%5B-112.01991856401462%2C33.51124624304089%5D%5D%5D%2C"type"%3A"Polygon"%7D%7D%5D%7D
    geotxt <- shape2geojson(shapefile, combine_in_one_string = FALSE) #

    url_of_report <- paste0(
      baseurl,
      "shape=", geotxt, "&",
      "buffer=", radius
    )
    url_of_report[is.na(geotxt)] <- NA
  } else {
    ###################################### # fips ?
    if (!is.null(fips)) {
      if (missing(radius)) {radius <- 0}
      ftype <- fipstype(fips)
      if (!all(ftype %in% "blockgroup")) {
        warning("fips must be blockgroup fips currently - other types not yet implemented")
      }
      url_of_report <- paste0(
        baseurl,
        "fips=", fips, "&",
        "buffer=", radius
      )
      url_of_report[is.na(fips)] <- NA
      url_of_report[!(ftype %in% "blockgroup")] <- NA
    } else {
      ###################################### # sitepoints ?
      if (!is.null(sitepoints)) {
        if (!is.null(lat) | !is.null(lon)) {warning("should specify lat & lon, or only sitepoints, not both")}
        x <- latlon_from_anything(sitepoints)
        lat <- x$lat
        lon <- x$lon
      }
      if (!is.null(lat) && !is.null(lon)) {
        url_of_report <- paste0(
          baseurl,
          "lat=", lat, "&",
          "lon=", lon, "&",
          "buffer=", radius
        )
        url_of_report[is.na(lat) | is.na(lon)] <- NA
      } else {
        ###################################### # none of the above
        url_of_report <- ifna
      }
    }
  }
  ###################### #
  urlx <- url_of_report

  ok <- !(is.na(urlx))

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
