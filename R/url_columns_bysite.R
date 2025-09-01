
#' URL functions - Create URLs in columns, for EJAM
#'
#' Could start to use this in server and ejamit(), and already used in table_xls_format()
#' @details used in [table_xls_format()]
#'
#' @param sitepoints data.frame or data.table with lat and lon columns
#'   (and should have ejam_uniq_id column or assume 1 output row per input row, same order)
#' @param lat,lon if sitepoints NULL/missing, vectors of latitudes and longitudes
#'   (assumes ejam_uniq_id is not available and treats output as 1 per input same order)
#'
#' @param shapefile spatial data.frame, class sf, see ejamit() parameter of same name.
#'   (and should have ejam_uniq_id column or assume 1 output row per input row, same order)
#'
#' @param fips vector of FIPS codes if relevant  (instead of sitepoints or shapefile input)
#'   Passed to [url_ejscreen_report()] as areaid. Treated like ejam_uniq_id
#'   Note that nearly half of all county fips codes are impossible to distinguish from
#'   5-digit zipcodes because the same numbers are used for both purposes.
#'
#' @param wherestr optional because inferred from fips if provided.
#'   Passed to [url_ejscreenmap()] and can be name of city, county, state like
#'   from fips2name(201090), or "new rochelle, ny" or "AK"
#'   or even a zip code, but NOT a fips code! (for FIPS, use the fips parameter instead).
#'   Note that nearly half of all county fips codes are impossible to distinguish from
#'   5-digit zipcodes because the same numbers are used for both purposes.
#' @param namestr passed to [url_ejscreen_report()]
#'
#' @param regid optional vector of FRS registry IDs if available to use to create links
#'   to detailed ECHO facility reports
#'
#' @param radius vector of values for radius in miles
#'
#' @param sitetype optional "latlon" or "shp" or "fips" but can be inferred from other params
#'
#' @param as_html logical, optional.
#'   passed to [url_ejscreen_report()] and [url_ejscreenmap()]
#' @param sitetype one of c("latlon", "fips", "shp") - inferred if not provided.
#'
#' @param hyperlink_colnames vector of colnames of URLs to create for results_bysite.
#'  Options: "EJScreen Report", "EJScreen Map",
#'  "ECHO Report", "FRS Report", "Enviromapper Report", "Countyhealth Report"
#' @param hyperlink_text vector of labels to use in the links, one per report column
#'
#' @param validate_regids if set TRUE, returns NA where a regid is not found in the FRS dataset that is
#'   currently being used by this package (which might not be the very latest from EPA).
#'   If set FALSE, faster since avoids checking but some links might not work and not warn about bad regid values.
#'
#' @param ... passed to [url_ejscreen_report()] such as areaid="0201090" for a city fips
#'
#' @seealso  [url_ejscreen_report()] [url_ejscreenmap()] [url_echo_facility_webpage()] [url_ejscreenapi_clusters_and_sort_cols()]
#' @return list of data.frames to append to the list of data.frames created by
#'   [ejamit()] or [doaggregate()],
#'
#'  `list(results_bysite = results_bysite, `
#'  `    results_overall = results_overall,`
#'  `      newcolnames=newcolnames)`
#'
#' @keywords internal
#'
url_columns_bysite <- function(sitepoints = NULL, lat = NULL, lon = NULL,
                               shapefile = NULL,
                               fips = NULL, wherestr = "", namestr = NULL,
                               regid = NULL,
                               radius = NULL,
                               sitetype = NULL,
                               as_html = TRUE,
                               validate_regids = FALSE,

                               ...) {
  {
    # Note that various settings passed to ejamit() are so far ignored by EJAM-API (and old ejscreen api), so results of API will often differ from actual results!!
    # message("API providing links to EJSCREEN/EJAM reports so far ignore most parameters that ejamit() does allow (and allow only blockgroup fips), so the link-based html report may differ from the actual ejamit() results_bysite table info!!")

    # Check for which API is available within each url_xyz function.
    #    e.g., in url_ejamapi(), check if (EJAM:::global_or_param("ejscreen_is_down") && EJAM:::global_or_param("ejamapi_is_down")) etc.

    ### *** draft work to refactor this to make it flexible to assemble a list of report names, report text labels, and url-generating functions
    # stopifnot(length(hyperlink_colnames) == length(hyperlink_text))
    ######################################################################################### #
  }
  # clean/check inputs and sitetype

  if (missing(lat) && missing(lon) && missing(shapefile) && missing(fips) && !missing(wherestr)) {
    fips <- fips_from_name(wherestr) # old ejscreenapi used wherestr not fips so this is in case that is the only thing provided here
   }
  if (is.null(sitepoints)) {
    if (!is.null(lat) && !is.null(lon)) {
      sitepoints = data.frame(lat = lat, lon = lon, ejam_uniq_id = seq_along(lat))
    }
  }
  if (is.null(sitetype)) {
    # infer if it is points, fips, or polygons
    sitetype <- EJAM:::ejamit_sitetype_from_input(
      sitepoints = sitepoints,
      fips = fips,
      shapefile = shapefile
    )
  }
  if (is.null(sitetype) || length(sitetype) != 1 || !(sitetype %in% c("latlon", "fips", "shp"))) {
    # also uses regid in some functions
    warning("cannot infer sitetype, must be one of 'latlon', 'fips', or 'shp'")
    # return all NA values?   but unclear how many rows worth.  # return(NA) # ???? ***
    return(NULL)
  }

  ######################################################################################### #
  # handle any list of functions that provide report URLs from this set of input params
  ######################################################################################### #

  if ("fips" %in% sitetype)   {rowcount = NROW(fips)}
  if ("shp" %in% sitetype)    {rowcount = NROW(shapefile)}
  if ("latlon" %in% sitetype) {rowcount = NROW(sitepoints)}

  reports <- EJAM:::global_or_param("default_reports") # list of reports, each a named lists of info like header, text, & FUN.
  if (is.null(reports)) {
    reports =  list(
      list(header = "EJAM Report",     text = "Report",   FUN = url_ejamapi)      # EJAM summary report (HTML via API)
      , list(header = "EJSCREEN Map",  text =  "EJSCREEN", FUN = url_ejscreenmap) # EJSCREEN site, zoomed to the location
      # , list(header = "ECHO Report",         text = "ECHO",         FUN = url_echo_facility_webpage) # if regid provided
      # , list(header = "FRS Report",          text =  "FRS",         FUN = url_frs_report)            # if regid provided
      # , list(header = "Enviromapper Report", text = "Enviromapper", FUN = url_enviromapper)          # if lat,lon provided
      # , list(header = "County Report",       text = "County",       FUN = url_countyhealthrankings)  # if fips provided
    )
  }
  links <- data.frame(matrix(NA, nrow = rowcount, ncol = length(reports)))
  url_functions = lapply(reports, function(x) (x$FUN))
  ## The input parameter defaults of url_columns_bysite() ensure all possible parameter names listed below exist and can be passed from here, and are NULL if not specified by the analysis calling url_columns_bysite().
  ## The ... parameter(s) in every report-generating FUN ensures any params not needed by a given FUN can be passed to it from here and just get ignored for that type of report.

  for (i in seq_along(reports)) {
    links[, i] <- url_functions[[i]](

      sitetype   = sitetype,
      radius = radius,
      sitepoints = sitepoints, # and if lat,lon had been specified above, they already got turned into sitepoints.
      shapefile = shapefile,
      fips = fips,
      wherestr = wherestr, # maybe not needed
      regid = regid,
      as_html = as_html, linktext = reports[[i]]$text,
      validate_regids = validate_regids)
  }
  colnames(links) <- sapply(reports, function(x) (x$header))

  results_overall = links[1, , drop=FALSE]
  if (NROW(links) == 1) {
     # ok
  } else {
    results_overall[1, ] <- NA # could create links to summary report overall, at least for that column ***
  }

  return(list(
    results_bysite  = links,
    results_overall = results_overall
  ))

  return(links)


  # ejscreen
  # map
  # acs
  # ECHO Report = url_echo_facility_webpage(regids = regid, as_html = as_html, validate_regids = validate_regids)
  # FRS Report = url_frs_report(regid = regid, as_html = as_html, validate_regids = validate_regids)
  # Enviromapper Report = url_enviromapper(lat = sitepoints$lat, lon = sitepoints$lon, as_html = as_html)
  # Countyhealth Report = url_countyhealthrankings(fips = fips2countyfips(fips), year = 2025)

  ################################################ #  ################################################ #


#
#
#   ## older approach hard-coded each report type and each sitetype:
#
#   ###################################### #
#   # EJAM/EJSCREEN SUMMARY REPORT
#   ###################################### #
#   echolink <- NA
#
#   ####### # get url_of_report - in fips case
#
#   if ("fips" %in% sitetype) {
#
#     if (!EJAM:::global_or_param("ejamapi_is_down")) {
#       url_of_report <- try({url_ejamapi(fips = fips, radius = radius, as_html = T)})
#       if (inherits(url_of_report, "try-error")) {
#         url_of_report <- rep(NA, length(fips))
#       }
#     } else {
#       if (!EJAM:::global_or_param("ejscreen_is_down")) {
#         areatype <- fipstype(fips)
#         if (!(all(areatype %in% c("blockgroup", "tract", "city", "county", 'state')))) {warning("FIPS must be one of 'blockgroup', 'tract', 'city', 'county' 'state' for the EJScreen API")}
#         url_of_report <- url_ejscreen_report(areaid   = fips, areatype = areatype, as_html = T) # use wherestr ?
#       } else {
#         url_of_report <- NA
#       }
#     }
#   }
#   ####### # get url_of_report - in latlon case
#
#   if ("latlon" %in% sitetype) {
#
#     pts = sitepoints
#
#     url_of_report <- url_ejamapi(sitepoints = pts,
#                                  radius = radius)
#   }
#   ####### # get url_of_report - in shp case
#
#   if ("shp" %in% sitetype) {
#     # shp_valid has only the valid ones and has ejam_uniq_id column to use to join this into final results_bysite
#
#     url_of_report <- url_ejamapi(shapefile = shapefile,
#                                  radius = radius) # not linktext = hyperlink_text[1]
#   }
#   ################################################ #
#
#   if (as_html) {
#     url_of_report = URLencode(url_of_report)
#     linktext <- hyperlink_text[1]
#     url_of_report <- url_linkify(url_of_report, text = linktext)
#   }
#   ################################################ #  ################################################ #
#
#   ###################################### #
#   ## EJSCREEN MAP LINK
#   ###################################### #
#
#   # get url_of_ejscreen_map, ideally focused on that location
#   # ignore radius, but then cannot map it as buffered within ejscreen app - if possible, at least just go to it without drawing it
#
#   if ("fips" %in% sitetype) {
#     if (!EJAM:::global_or_param("ejamapi_is_down") || !EJAM:::global_or_param("ejscreen_is_down")) {
#       url_of_ejscreen_map <- try({url_ejscreenmap(wherestr = fips2name(fips), as_html = T, linktext = hyperlink_text[2])})
#       if (inherits(url_of_ejscreen_map, "try-error")) {
#         url_of_ejscreen_map <- rep(NA, length(fips))
#       }
#     } else {
#       url_of_ejscreen_map <- rep(NA, length(fips))
#       ejam_uniq_id <- fips
#     }
#   }
#   if ("latlon" %in% sitetype) {
#     url_of_ejscreen_map = url_ejscreenmap(lat = pts$lat, lon = pts$lon, as_html = as_html, linktext = hyperlink_text[2])
#     ejam_uniq_id <- NA
#   }
#   if ("shp" %in% sitetype) {
#     url_of_ejscreen_map = url_ejscreenmap(shapefile = shapefile, as_html = as_html, linktext = hyperlink_text[2]) # includes invalid polygons. has ejam_uniq_id. already buffered if radius>0.
#   }
#   ################################################ #  ################################################ #
#
#   ###################################### #
#   ## ECHO REPORT
#   ###################################### #
#   # ECHO Report = url_echo_facility_webpage(regids = regid, as_html = as_html, validate_regids = validate_regids)
#
#   if ("fips" %in% sitetype) {
#     echolink = rep(NA, NROW(fips))
#   }
#   if ("latlon" %in% sitetype) {
#     if (!is.null(regid)) {
#       echolink <- url_echo_facility_webpage(regid = regid, as_html = as_html, validate_regids = validate_regids, linktext = hyperlink_text[3])
#     } else {
#       echolink <- rep(NA, NROW(pts)) # server used 'N/A' instead of NA -- which do we want to use?
#     }
#   }
#   if ("shp" %in% sitetype) {
#     echolink = rep(NA, NROW(shapefile))
#   }
#   ###################################### #
#   ## FRS Report
#   ###################################### #
#   # FRS Report = url_frs_report(regid = regid, as_html = as_html, validate_regids = validate_regids)
#   if ("fips" %in% sitetype) {
#
#   }
#   if ("latlon" %in% sitetype) {
#
#   }
#   if ("shp" %in% sitetype) {
#
#   }
#   ###################################### #
#   ## Enviromapper Report
#   ###################################### #
#   # Enviromapper Report = url_enviromapper(lat = sitepoints$lat, lon = sitepoints$lon, as_html = as_html)
#   if ("fips" %in% sitetype) {
#
#   }
#   if ("latlon" %in% sitetype) {
#
#   }
#   if ("shp" %in% sitetype) {
#
#   }
#   ###################################### #
#   ## Countyhealth Report
#   ###################################### #
#   # Countyhealth Report = url_countyhealthrankings(fips = fips2countyfips(fips), year = 2025)
#   if ("fips" %in% sitetype) {
#
#   }
#   if ("latlon" %in% sitetype) {
#
#   }
#   if ("shp" %in% sitetype) {
#
#   }
#
#   ################################################ #  ################################################ #
#   ################################################ #  ################################################ #
#
#   results_bysite[ , `:=`(
#     `EJScreen Report` = url_of_report,
#     `EJScreen Map`    = url_of_ejscreen_map,
#
#     `ECHO Report` = echolink,
#     # `ECHO Report` = url_echo_facility_webpage(regid = regid, as_html = as_html, validate_regids = validate_regids, linktext = hyperlink_text[3]),
#     `FRS Report`             = url_frs_report(regid = regid, as_html = as_html, validate_regids = validate_regids, linktext = hyperlink_text[4]),
#     `Enviromapper Report`  = url_enviromapper(lat = sitepoints$lat, lon = sitepoints$lon, as_html = as_html, linktext = hyperlink_text[5]),
#     `Countyhealth Report` = url_countyhealthrankings(fips = fips2countyfips(fips),  year = 2025) # no linktext param
#   )]
#   ################################################ #  ################################################ #
#   # URL columns for results_overall etc.
#
#   if (NROW(results_bysite) == 1) {
#     # If we analyzed only 1 place then overall is same as 1 site per row!
#     results_overall[ , `:=`(
#       `EJScreen Report` = results_bysite$`EJScreen Report`,
#       `EJScreen Map`    = results_bysite$`EJScreen Map`,
#
#       `ECHO Report`     = results_bysite$`ECHO Report`,
#       `FRS Report`      =  results_bysite$ `FRS Report`,
#       `Enviromapper Report`  = results_bysite$`Enviromapper Report`,
#       `Countyhealth Report` = results_bysite$`Countyhealth Report`
#     )]
#   } else {
#     results_overall[ , `:=`(
#       `EJScreen Report` = NA,
#       `EJScreen Map`    = NA,
#
#       `ECHO Report`     = NA,
#       `FRS Report`      =  NA,
#       `Enviromapper Report`  = NA,
#       `Countyhealth Report` = NA
#     )]
#   }
#
#   return(list(
#     results_bysite  = results_bysite,
#     results_overall = results_overall
#   ))
}
################################################ #  ################################################ #
################################################ #  ################################################ #
