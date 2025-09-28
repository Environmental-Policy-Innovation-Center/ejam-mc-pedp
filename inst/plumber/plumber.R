####################################################### #
#
# Set up API for access to EJAM functionality, using the plumber package.
#
# also see    EJAM file "plumber/test_the_api.R"
############################# #
#* @apiTitle EJAM API
#*
#* @apiDescription Provides EJAM / EJSCREEN batch analysis summary results.
#* See the EJAM package for technical documentation on functions powering the API, at <https://ejanalysis.org/ejamdocs>

# future::plan("multisession")  # did not seem to work

############################# #

library(EJAM)

# helpers to convert API input format to R function parameter formats
# note these do not handle a vector parameter, only convert a single "" or "true" or "false" value
NULL_if_empty = function(x) {
  if ("" %in% x) {return(NULL)} else {return(x)}
}
TRUEFALSE_if_truefalse = function(x) {
  if (length(x) == 1 && ("true" %in% x || "TRUE" %in% x)) {return(TRUE)}
  if (length(x) == 1 && ("false" %in% x || "FALSE" %in% x)) {return(FALSE)}
  return(x)
}
api2r = function(x) {
  NULL_if_empty(
    TRUEFALSE_if_truefalse(x)
  )
}

############################# #

# MULTIPLE POINTS or Shapefile WILL NEED TO BE PASSED USING POST AND REQUEST BODY
#  SO IT IS A BIT MORE COMPLICATED - NOT DONE YET

############################# #


####################################################### #
#  DEFINE API ENDPOINTS ####
####################################################### #
# . ####
####################################################################################################### #

# report ####


## JUST A DRAFT - NOT TESTED AT ALL


##  This endpoint is essentially doing  ejam2report(ejamit(  ))
##  so inputs are point(s) or polygon(s) or fip(s), and output is html summary report.

#* Get EJAM analysis results report as HTML (on one site or the aggregate of multiple sites overall)
#* See ejanalysis.org/docs for more information about the ejamit() and ejam2report() functions
#*
#* @param lat if provided, a vector of latitudes in decimal degrees (comma-separated values)
#* @param lon if provided, a vector of longitudes in decimal degrees (comma-separated values)
#* @param sitepoints optional way to provide lat,lon: a data.table with columns lat, lon giving point locations of sites or facilities around which are circular buffers
#*
#* #* @param fips optional FIPS code vector (comma-separated values) to provide if using FIPS instead of sitepoints to specify places to analyze,
#*  such as a list of US Counties or tracts. Passed to [getblocksnearby_from_fips()]
#*
#* @param shapefile optional. A sf shapefile object or path to .zip, .gdb, .json, .kml, etc., or folder that has a shapefiles, to analyze polygons.
#*  e.g., `out = ejamit(shapefile = testdata("portland.json", quiet = T), radius = 0)`
#*  If in RStudio you want it to interactively prompt you to pick a file,
#*  use shapefile=1 (otherwise it assumes you want to pick a latlon file).
#*
#* @param sitenumber if provided, reports on specified row in results table of sites,
#*   instead of on overall aggregate of all sites analyzed (default)
#*
#* @param radius in miles, defining circular buffer around a site point, or buffer to add to polygon
#* @param radius_donut_lower_edge radius of lower edge of donut ring if analyzing a ring not circle
#* @param maxradius  do not use
#* @param avoidorphans do not use
#* @param quadtree do not use
#*
#* @param countcols character vector of names of variables to aggregate within a buffer using a sum of counts,
#*  like, for example, the number of people for whom a poverty ratio is known,
#*  the count of which is the exact denominator needed to correctly calculate percent low income.
#* @param wtdmeancols character vector of names of variables to aggregate within a buffer using population-weighted or other-weighted mean.
#* @param calculatedcols character vector of names of variables to aggregate within a buffer using formulas that have to be specified.
#* @param calctype_maxbg character vector of names of variables to aggregate within a buffer
#*  using max() of all blockgroup-level values.
#* @param calctype_minbg character vector of names of variables to aggregate within a buffer
#*  using min() of all blockgroup-level values.
#* @param subgroups_type Optional (uses default). Set this to "nh" for non-hispanic race subgroups
#*  as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh;
#*  "alone" for race subgroups like White Alone, wa and others in names_d_subgroups_alone;
#*  "both" for both versions. Possibly another option is "original" or "default"
#*  Alone means single race.
#* @param include_ejindexes whether to try to include EJ Indexes (assuming dataset is available) - passed to [doaggregate()]
#* @param calculate_ratios whether to calculate and return ratio of each indicator to US and State overall averages - passed to [doaggregate()]
#* @param extra_demog if should include more indicators from v2.2 report on language etc.
#* @param need_proximityscore whether to calculate proximity scores
#* @param infer_sitepoints set to TRUE to try to infer the lat,lon of each site around which the blocks in sites2blocks were found.
#*  lat,lon of each site will be approximated as average of nearby blocks,
#*  although a more accurate slower way would be to use reported distance of each of 3 of the furthest block points and triangulate
#* @param need_blockwt if fips parameter is used, passed to [getblocksnearby_from_fips()]
#* @param thresholds list of percentiles like list(80,90) passed to
#*  batch.summarize(), to be
#*  counted to report how many of each set of indicators exceed thresholds
#*  at each site. (see default)
#* @param threshnames list of groups of variable names (see default)
#* @param threshgroups list of text names of the groups (see default)
#* @param progress_all progress bar from app in R shiny to run
#* @param updateProgress progress bar function passed to [doaggregate()] in shiny app
#* @param updateProgress_getblocks progress bar function passed to [getblocksnearby()] in shiny app
#* @param in_shiny if fips parameter is used, passed to [getblocksnearby_from_fips()]
#* @param quiet Optional. passed to [getblocksnearby()] and [batch.summarize()]. set to TRUE to avoid message about using [getblocks_diagnostics()],
#*  which is relevant only if a user saved the output of this function.
#* @param silentinteractive to prevent long output showing in console in RStudio when in interactive mode,
#*  passed to [doaggregate()] also. app server sets this to TRUE when calling [doaggregate()] but
#*  [ejamit()] default is to set this to FALSE when calling [doaggregate()].
#* @param called_by_ejamit Set to TRUE by [ejamit()] to suppress some outputs even if ejamit(silentinteractive=F)
#* @param testing used while testing this function, passed to [doaggregate()]
#* @param showdrinkingwater T/F whether to include drinking water indicator values or display as NA. Defaults to TRUE.
#* @param showpctowned T/f whether to include percent owner-occupied units indicator values or display as NA. Defaults to TRUE.
#* @param download_city_fips_bounds passed to [area_sqmi()]
#* @param download_noncity_fips_bounds passed to [area_sqmi()]
#*
#* @param ... passed to ejam2report() but cannot change these:
#*   launch_browser, fileextension, return_html, filename = "EJAM_results.html"
#*
#* @param attachment "true" means return html file as attachment
#*
#* @post /report
#* @serializer html
function(
    # mosty the same arguments as ejamit()

  sitepoints = "",  lat = "",  lon = "",
  radius = 3,
  fips = "",
  shapefile = "",

  sitenumber = "",

  radius_donut_lower_edge = 0,
  maxradius = 31.07,
  avoidorphans = "false",
  # quadtree = "",
  countcols = "",
  wtdmeancols = "",
  calculatedcols = "",
  calctype_maxbg = "",
  calctype_minbg = "",
  subgroups_type = "nh",
  include_ejindexes = "true",
  calculate_ratios = "true",
  extra_demog = "true",
  need_proximityscore = "false",
  infer_sitepoints = "false",
  need_blockwt = "true",
  thresholds = list(80, 80),
  threshnames = list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
  threshgroups = list("EJ-US-or-ST", "Supp-US-or-ST"),
  updateProgress = "",
  updateProgress_getblocks = "",
  progress_all = "",
  in_shiny = "false",
  quiet = "true",
  silentinteractive = "false",
  called_by_ejamit = "true",
  testing = "false",
  showdrinkingwater = "true",
  showpctowned = "true",
  download_city_fips_bounds = "true",
  download_noncity_fips_bounds = "false",

  ...,

  attachment = "true"
) {



  filename = "EJAM_results.html"

  crs <- 4326

  ## we could avoid running analysis of all sites if many are submitted but
  ## only one is going to be reported on (ie sitenumber was specified)
  ## BUT we would need to NOT provide sitenumber param to ejam2report() if this is done
  # if (!is.null(sitenumber)) {
  #   sitenumber <- as.numeric(sitenumber)
  #   if (!missing(shapefile) && !is.null(shapefile)) {
  #     shapefile = shapefile[sitenumber, ]
  #   } else {
  #     if (!missing(fips) && !is.null(fips)) {
  #       fips = fips[sitenumber]
  #     } else {
  #       if (!missing(sitepoints) && !is.null(sitepoints)) {
  #         sitepoints <- sitepoints[sitenumber, ]
  #       }
  #     }
  #   }
  # }

  # sites <- EJAM:::sites_from_input(sitepoints = sitepoints, lat = lat, lon = lon,
  #                                  fips = fips,
  #                                  shapefile = shapefile)

  ejamitout <- tryCatch(
    ejamit(
      sitepoints = api2r(sitepoints),
      lat = api2r(lat), lon = api2r(lon),
      radius = api2r(radius),
      fips = api2r(fips),
      shapefile = api2r(shapefile),

      radius_donut_lower_edge = api2r(radius_donut_lower_edge),
      maxradius = api2r(maxradius),
      avoidorphans = api2r(avoidorphans),
      # quadtree = quadtree,
      countcols = api2r(countcols),
      wtdmeancols = api2r(wtdmeancols),
      calculatedcols = api2r(calculatedcols),
      calctype_maxbg = api2r(calctype_maxbg),
      calctype_minbg = api2r(calctype_minbg),
      subgroups_type = api2r(subgroups_type),
      include_ejindexes = api2r(include_ejindexes),
      calculate_ratios = api2r(calculate_ratios),
      extra_demog = api2r(extra_demog),
      need_proximityscore = api2r(need_proximityscore),
      infer_sitepoints = api2r(infer_sitepoints),
      need_blockwt = api2r(need_blockwt),
      thresholds = api2r(thresholds),
      threshnames = api2r(threshnames),
      threshgroups = api2r(threshgroups),
      updateProgress = api2r(updateProgress),
      updateProgress_getblocks = api2r(updateProgress_getblocks),
      progress_all = api2r(progress_all),
      in_shiny = api2r(in_shiny),
      quiet = api2r(quiet),
      silentinteractive = api2r(silentinteractive),
      called_by_ejamit = api2r(called_by_ejamit),
      testing = api2r(testing),
      showdrinkingwater = api2r(showdrinkingwater),
      showpctowned = api2r(showpctowned),

      maxradius = api2r(maxradius),
      avoidorphans = api2r(avoidorphans),
      quadtree = api2r(quadtree),
      countcols = api2r(countcols),
      wtdmeancols = api2r(wtdmeancols),
      calculatedcols = api2r(calculatedcols),
      calctype_maxbg = api2r(calctype_maxbg),
      calctype_minbg = api2r(calctype_minbg),
      subgroups_type = api2r(subgroups_type),
      include_ejindexes = api2r(include_ejindexes),
      calculate_ratios = api2r(calculate_ratios),
      extra_demog = api2r(extra_demog),
      need_proximityscore = api2r(need_proximityscore),
      infer_sitepoints = api2r(infer_sitepoints),
      need_blockwt = api2r(need_blockwt),
      thresholds = api2r(thresholds),
      threshnames = api2r(threshnames),
      threshgroups = api2r(threshgroups),
      updateProgress = api2r(updateProgress),
      updateProgress_getblocks = api2r(updateProgress_getblocks),
      progress_all = api2r(progress_all),
      in_shiny = api2r(in_shiny),
      quiet = api2r(quiet),
      silentinteractive = api2r(silentinteractive),
      called_by_ejamit = api2r(called_by_ejamit),
      testing = api2r(testing),
      download_city_fips_bounds = api2r(download_city_fips_bounds),
      download_noncity_fips_bounds = api2r(download_noncity_fips_bounds)
    ),
    error = function(e) {
      res$status <- 400
      handle_error(e$message)
    }
  )

  # If an error was returned from the interface, return it.
  if ("error" %in% names(ejamitout)) {
    return(ejamitout)
  }

  # Prepare the final JSON output.
  # Generate and return the HTML report.
  reportout <- ejam2report(ejamitout = ejamitout,

                           # shp = api2r(shp), ### WHERE TO GET shp as done in server ?? ***

                           sitenumber = api2r(sitenumber), ############ #
                           return_html = TRUE,
                           launch_browser = FALSE)

  if (attachment == "true") {
    plumber::as_attachment(
      value = reportout,
      filename = filename
    )
  } else {
    reportout
  }

}
####################################################################################################### #

# reportpost ####

## JUST A DRAFT - NOT TESTED AT ALL

##  This endpoint is essentially doing  ejam2report(ejamit(  ))
##  so inputs are point(s) or polygon(s) or fip(s), and output is html summary report.

#* inputs are like those to ejamit(), outputs like those from ejam2report(), returns html EJAM summary report, analysis results
#*
#* @param lat Latitude decimal degrees (single point only, or vector of comma-separated values like lat=34,35,32)
#* @param lon Longitude decimal degrees (single point only, for now)
#* @param sitepoints NOTE USED HERE - optional way to provide lat,lon: a data.table with columns lat, lon giving point locations of sites or facilities around which are circular buffers
#* @param radius Radius in miles]
#*
#* @param fips Census fips code for Census unit(s) of
#*   type(s) blockgroup, tract, city (7-digit), county (5-digit), or state (2-digit)
#*
#* @param shapefile spatial data.frame?
#*
#* @param ... parameters passed to ejam2report(), but these are preset and cannot be changed:
#*   launch_browser, fileextension, return_html, filename = "EJAM_results.html"
#*
#* @param attachment "true" means return html file as attachment
#*
#* @serializer html
#* @post /reportpost
#*
function(lat = '', lon = '', sitepoints = "", radius = '', shapefile = '', fips = '', ..., attachment = "true") {

  filename = "EJAM_results.html"

  lat = api2r(lat)
  lon = api2r(lon)
  radius = api2r(radius)
  shapefile = api2r(shapefile)
  fips = api2r(fips)
  sitepoints = api2r(sitepoints)

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  # if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  # if (length(radius) != 1) {radius <- 1}

  ejamitout <- tryCatch(
    ejamit(
      lat = lat, lon = lon, radius = radius, shapefile = shapefile, fips = fips
    ),
    error = function(e) {
      res$status <- 400
      handle_error(e$message)
    }
  )

  # If an error was returned from the interface, return it.
  if ("error" %in% names(ejamitout)) {
    return(ejamitout)
  }

  # Prepare the final JSON output.
  # Generate and return the HTML report.

  reportout <- ejam2report(ejamitout = ejamitout,
                           sitenumber = sitenumber, ############ #
                           return_html = TRUE,
                           launch_browser = FALSE,
                           ...)

  if (attachment == "true") {
    plumber::as_attachment(
      value = reportout,
      filename = filename
    )
  } else {
    reportout
  }

}

####################################################################################################### #

# ejam2report ####

## JUST A DRAFT - NOT TESTED AT ALL

#* like ejam2report(), returns html EJAM summary report, analysis results, given the list that is the output of ejamit()
#*
#* @param ejamitout the output of ejamit(), and if omitted, a sample report is returned
#* @param ... other parameters passed to ejam2report(), but these are preset and cannot be changed:
#*   launch_browser, fileextension, return_html, filename = "EJAM_results.html"
#*
#* Like EJAM::ejam2report()
#*
#* @serializer html
#* @post /ejam2report
#*
function(ejamitout = testoutput_ejamit_10pts_1miles, ...) {

  # ejamitout = testoutput_ejamit_10pts_1miles,
  # sitenumber = NULL,
  # analysis_title = 'Summary of Analysis',
  # submitted_upload_method = c("latlon", "SHP", "FIPS")[1],
  # shp = NULL,
  # return_html = FALSE,
  # fileextension = c("html", "pdf")[1],
  # filename = NULL,
  # launch_browser = TRUE,
  # show_ratios_in_report = TRUE,
  # extratable_show_ratios_in_report = TRUE,
  # extratable_title = '', #'Additional Information',
  # extratable_title_top_row = 'ADDITIONAL INFORMATION',
  # extratable_list_of_sections = list(
  #   # see build_community_report defaults and see global_defaults_*.R
  #   `Breakdown by Population Group` = names_d_subgroups,
  #   `Language Spoken at Home` = names_d_language,
  #   `Language in Limited English Speaking Households` = names_d_languageli,
  #   `Breakdown by Sex` = c('pctmale','pctfemale'),
  #   `Health` = names_health,
  #   `Age` = c('pctunder5', 'pctunder18', 'pctover64'),
  #   `Community` = names_community[!(names_community %in% c( 'pctmale', 'pctfemale', 'pctownedunits_dupe'))],
  #   `Poverty` = names_d_extra,
  #   `Features and Location Information` = c(
  #     names_e_other,
  #     names_sitesinarea,
  #     names_featuresinarea,
  #     names_flag
  #   ),
  #   `Climate` = names_climate,
  #   `Critical Services` = names_criticalservice,
  #   `Other` = names_d_other_count
  #   # , `Count above threshold` = names_countabove  # need to fix map_headernames longname and calctype and weight and drop 2 of the 6
  # ),
  # ## all the indicators that are in extratable_list_of_sections:
  # extratable_hide_missing_rows_for = as.vector(unlist(extratable_list_of_sections))

  filename = "EJAM_results.html"

  reportout <- ejam2report(ejamitout = ejamitout, launch_browser = FALSE, fileextension = "html",
                           return_html = FALSE, ## ???
                           filename = filename, ## or NULL ?
                           ...)

  if (attachment == "true") {
    plumber::as_attachment(
      value = reportout,
      filename = filename
    )
  } else {
    reportout
  }
}
####################################################################################################### #

# ejam2excel ####

## JUST A DRAFT - NOT TESTED AT ALL

#* like ejam2excel(), returns xlsx file of EJAM analysis results for all residents within X miles of a single point defined by latitude and longitude.
#*
#* @param lat Latitude decimal degrees (single point only, for now)
#* @param lon Longitude decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#* @param ... other parameters passed to ejam2excel()
#*
#* Like EJAM::ejam2excel()
#*
#* @serializer excel
#* @get /ejam2excel
#*
function(lat = 40.81417, lon = -96.69963, radius = 1, test = "false", ...) {

  fname = "EJAM_results.xlsx"

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}

  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {
    # promises::future_promise({ # did not seem to work
    out <- ejamit(
      sitepoints = data.frame(lat = lat, lon = lon),
      radius = radius
    )$results_overall
    # }) # did not seem to work
  }

  out <- ejam2excel(out, ...)

  # if (attachment == "true") {
  plumber::as_attachment(
    value = out,
    filename = fname
  )
  # } else {
  #   out
  #   }
}
####################################################################################################### #
####################################################################################################### #

# ejamit ####

## JUST A DRAFT - NOT TESTED AT ALL

#* json table of EJAM analysis summary results for all residents within X miles of a single point or in a polygon
#*
#* @param lat Latitude decimal degrees  (single point only, for now)
#* @param lon Longitude decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param shapefile shapefile (ignores lat,lon,radius if this is provided). NOT YET IMPLEMENTED.
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#*
#* Like EJAM::ejamit()$results_overall (but with friendlier column names for indicators).
#*
#* Calling from R for example:
#* url2 <- "https://urlgoeshere/ejamit?lon=-101&lat=36&radius=1&test=true";
#* results_overall <- httr2::request(url2) |> httr2::req_perform() |>
#* httr2::resp_body_json() |> jsonlite::toJSON() |> jsonlite::fromJSON()
#*
#* @get /ejamit
#*
function(lat = 40.81417, lon = -96.69963, radius = 1, shapefile = 0, names = "long", test = "false") {

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}

  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {

    # promises::future_promise({  # did not seem to work

    if (!all(0 == shapefile)) {

      return("not working yet for shapefile inputs")

      out <- ejamit(
        shapefile = shapefile,
        radius = radius
      )$results_overall

    } else {

      out <- ejamit(
        sitepoints = data.frame(lat = lat, lon = lon),
        radius = radius
      )$results_overall

    }
    # })

  }

  if (names == "long") {
    names(out) <- fixcolnames(names(out), 'r', 'long')
  }

  # if (attachment == "true") {
  # plumber::as_attachment(
  #   value = as.data.frame(out),
  #   filename = "EJAM_results.csv"
  # )
  # } else {
  out
  # }
}
####################################################################################################### #

# ejamit_csv ####

## JUST A DRAFT - NOT TESTED AT ALL

#* csv table of EJAM analysis summary results for all residents within X miles of a single point defined by latitude and longitude.
#*
#* @param lat Latitude decimal degrees (single point only, for now)
#* @param lon Longitude decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#*
#* Like EJAM::ejamit()$results_overall (but with friendlier column names for indicators).
#*
#* @serializer csv
#* @get /ejamit_csv
#*
function(lat = 40.81417, lon = -96.69963, radius = 1, names = "long", test = "false") {

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}

  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {
    # promises::future_promise({ # did not seem to work
    out <- ejamit(
      sitepoints = data.frame(lat = lat, lon = lon),
      radius = radius
    )$results_overall
    # }) # did not seem to work
  }

  if (names == "long") {
    names(out) <- fixcolnames(names(out), 'r', 'long')
  }

  # if (attachment == "true") {
  plumber::as_attachment(
    value = as.data.frame(out),
    filename = "EJAM_results.csv"
  )
  # } else {
  #   out
  #   }
}
####################################################################################################### #

# getblocksnearby ####

## JUST A DRAFT - NOT TESTED AT ALL

#* json table of distances to all Census blocks near given point.
#*
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius Radius of circular area in miles.
#*
#* Finds all Census blocks whose internal point is within radius of site point.
#*
#* @get /getblocksnearby
#*
function(lat, lon, radius) {

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}

  # require(EJAM)
  # if (!exists("blockwts")) {dataload_dynamic('blockwts)}
  # if (!exists("localtree")) indexblocks()

  # promises::future_promise({  #

  out <- EJAM::getblocksnearby(
    data.frame(
      lat = lat,
      lon = lon
    ),
    radius = as.numeric(radius)  # , quadtree = localtree
  )
  # })
  out
}
####################################################### #

# get_blockpoints_in_shape ####

## JUST A DRAFT - NOT TESTED AT ALL

#* json table of Census blocks in each polygon
#*
#* @param polys Spatial data that is polygons as from sf::st_as_sf()
#* @param addedbuffermiles width of optional buffering to add to the points (or edges), in miles
#* @param dissolved If TRUE, use sf::st_union(polys) to find unique blocks inside any one or more of polys
#* @param safety_margin_ratio  multiplied by addedbuffermiles, how far to search for blocks nearby using EJAM::getblocksnearby(), before using those found to do the intersection
#* @param crs coordinate reference system used in st_as_sf() and st_transform() and shape_buffered_from_shapefile_points(), crs = 4269 or Geodetic CRS NAD83
#* @get /get_blockpoints_in_shape
#*
function(polys,
         addedbuffermiles = 0,
         dissolved = FALSE,
         safety_margin_ratio = 1.10,
         crs = 4269
) {

  return("not working yet for shapefile inputs")

  # require(EJAM)
  # if (!exists("blockwts"))  {dataload_dynamic('blockwts)}
  # if (!exists("localtree")) indexblocks()

  # promises::future_promise({  # })

  out <- EJAM::get_blockpoints_in_shape(
    polys = polys,
    addedbuffermiles = addedbuffermiles,
    dissolved = dissolved,
    safety_margin_ratio = safety_margin_ratio,
    crs = crs
  )
  # })
  out
}
####################################################### #

# doaggregate ####

## JUST A DRAFT - NOT TESTED AT ALL

#* List of tables and other info summarizing demog and envt based on sites2blocks table
#*
#* @param sites2blocks see [doaggregate()]
#* @param sites2states_or_latlon see [doaggregate()]
#* @param countcols see [doaggregate()]
#* @param popmeancols see [doaggregate()]
#* @param calculatedcols see [doaggregate()]
#* @param ... passed to [doaggregate()]
#* @get /doaggregate
#*
function(sites2blocks, sites2states_or_latlon, countcols, popmeancols, calculatedcols, ...) {
  # promises::future_promise({
  if (!exists("blockgroupstats")) {library(EJAM)} # to use installed version only if not already attached
  # library(EJAM)
  if (!exists("blockwts"))  dataload_dynamic("blockwts")
  if (!exists("localtree")) indexblocks()
  EJAM::doaggregate(sites2blocks = sites2blocks,
                    sites2states_or_latlon = sites2states_or_latlon,
                    countcols = countcols, popmeancols = popmeancols, calculatedcols = calculatedcols, ... )
  # })
}
# ####################################################### #

# echo ####
#
#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
#*
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}
####################################################### #

# if (format == "excel") {
#   # NOT WORKING YET - THIS WOULD NOT RETURN A SPREADSHEET IF save_now=FALSE... IT JUST WOULD CREATE A WORKBOOK IN openxlsx::  format.
# promises::future_promise({  # })
#   # out <- table_xls_from_ejam(ejamit(sitepoints = sitepoints, radius = radius), launchexcel = F, save_now = FALSE)
# })

# ##promises::future_promise({  # })
#   out <- as.data.frame(as.data.frame(EJAM::ejamit(sitepoints = sitepoints, radius = radius)[["results_overall"]]))
# ##})
# }
#

####################################################### #
####################################################### #
