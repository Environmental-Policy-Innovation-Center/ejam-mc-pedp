
#' helps other functions have flexible input parameters - can provide points in a table, points as lat,lon vectors, polygons in a spatial data.frame, or Census units as a fips code vector
#' figures out which type of inputs were provided, and returns them
#' @param sitepoints optional data.frame with colnames lat,lon
#' @param lat,lon optional vectors of latitudes and longitudes
#' @param shapefile  optional polygons in a spatial data.frame
#' @param fips  optional Census units as a fips code vector
#'
#' @returns a list with names sitetype, sitepoints, fips, and shapefile.
#'   sitetype is "latlon" or "fips" or "shp" or NULL
#'   others are NULL except the one corresponding to the sitetype
#'   sitepoints would be a data.frame of points in columns lat,lon or NULL
#'   shapefile would be a spatial data.frame "sf" class or NULL
#'   fips would be a vector of Census FIPS codes or NULL
#'
#' @keywords internal
#'
sites_from_input <- function(sitepoints = NULL, lat = NULL, lon = NULL,
                             shapefile = NULL,
                             fips = NULL) {

  # reconcile latlon vs sitepoints as input method for points
  sitepoints <- sitepoints_from_latlon_or_sitepoints(sitepoints = sitepoints, lat = lat, lon = lon)
  ######################## #
  sitetype = NULL # not NA
  have_latlon = (!is.null(sitepoints) & NROW(sitepoints) > 0)
  have_shp  = (!is.null(shapefile) & NROW(shapefile) > 0)
  have_fips = (!is.null(fips) & NROW(fips) > 0)

  # warn and NULL if 0 provided
  if (sum(have_latlon, have_shp, have_fips) == 0
      # && (is.null(regid) || length(regid) == 0)
  ) {
    # warning("no sites were provided") # and sitetype is NULL
    sitepoints = NULL; shapefile = NULL; fips <- NULL
  }
  # warn and pick if >1 provided
  if (sum(have_latlon, have_shp, have_fips) > 1) {
    warning("should provide only one of these: sitepoints or shapefile or fips")
    if (have_latlon) {warning("using sitepoints and ignoring shp or fips")} else {
      if (have_shp)  {warning("using shapefile and ignoring fips")}
    }
  }
  # make others NULL if only 1 provided, or use in this priority order if >1 provided:
  # and report which 1 type
  if (have_latlon) {sitetype = "latlon"; fips <- NULL;                  shapefile <- NULL} else {
    if (have_shp)    {sitetype = "shp"; fips <- NULL; sitepoints <- NULL}                    else {
      if (have_fips)   {sitetype = "fips";            sitepoints <- NULL; shapefile <- NULL}
    }}
  ####################### #
  ### DO ANY ACTUAL VALIDATION HERE OR LATER? ***
  # But should NOT drop them here, yet, so we can keep track of all inputs including invalid ones.
  #    i.e. flag? warn? on invalid sites, such as bad latlon, empty polygons?
  ## no validation done so far but could do:
  # sitepoints <- latlon_from_anything(sitepoints, set_invalid_to_na = TRUE) # also lets input be a filepath
  # shapefile <- shapefile_from_any(shapefile, cleanit=F) # also maybe make the empty/invalid rows NA? cleanit=T drops them?
  # fips[!fips_valid(fips)] <- NA #  fips_from_any is not a function
  ####################### #
  return(list(
    sitepoints = sitepoints,
    shapefile = shapefile,
    fips = fips,

    sitetype = sitetype # latlon, shp, fips, or NULL, not NA
  ))
}
