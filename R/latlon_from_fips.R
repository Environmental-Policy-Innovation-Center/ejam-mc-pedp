
# DRAFT

# get approx centroid of each fips census unit


latlon_from_fips <- function(fips) {

  ftype = fipstype(fips)

  dtf = data.table::data.table(ftype=ftype, fips=fips) #, lat = NA, lon = NA) latlon_join_on_ does nothing if lat,lon cols already there

  ####################################### #
  # blocks: fips to blockid to lat,lon

  if (("block" %in% ftype)) {
    if (!exists("blockid2fips")) {
      dataload_dynamic("blockid2fips")
    }
    # get blockid via fips
    setnames(dtf, 'fips', 'blockfips')
    dtf[blockid2fips, blockid := blockid, on = "blockfips"]
    setnames(dtf, 'blockfips', 'fips')
    # get latlon via blockid
    # if (!exists("blockid2fips")) {
    #   dataload_dynamic("blockid2fips")
    # } # done by:
    latlon_join_on_blockid(dtf)
    dtf[, blockid := NULL]
  }
  ####################################### #
  # blockgroups:  just use bgpts table from this pkg

  if ("blockgroup" %in% ftype) {

    dtf_bg = dtf[ftype %in% "blockgroup", .(ftype, fips)]
    setnames(dtf_bg, 'fips', 'bgfips')
    dtf_bg[bgpts, `:=`(lat = lat, lon = lon), on = "bgfips"]
    setnames(dtf_bg, 'bgfips', 'fips')

    if (!("lat" %in% names(dtf)) || !("lat" %in% names(dtf))) {
      dtf$lat = 0
      dtf$lon = 0
    }
    dtf[ftype %in% "blockgroup", ] <- dtf_bg[, .(ftype,fips,lat,lon)]

    # latlon_from_bgfips = function(fips) {
    #
    #
    # return(dtf)
    # }
    #
    # x <-  latlon_from_bgfips(fips[ftype %in% "blockgroup"])


  }
  ####################################### #
  # tracts:
  if ("tract" %in% ftype) {

    # maybe just take average of lat and average of lon of the blockgroups in a tract?



  }
  ####################################### #
  # cities
  if ("city" %in% ftype) {

    require(AOI)
    placename <-  fips_place2placename(fips[ftype %in% "city"])
    if (NROW(placename) > 0 && !is.null(placename) && "lat" %in% names(placename)) {
      newrows <- data.table(ftype = ftype[ftype %in% "city"], fips = fips[ftype %in% "city"], lat = placename$lat, lon = placename$lon)
      dtf[ftype %in% "city", ] <- newrows
    }

  }

  ####################################### #
  # counties ?
  if ("county" %in% ftype) {
    # use tidycensus:: to get this info??  or shapes_from_fips() ?
    # or just the average lat and lon among blocks in the county

    cfips = fips[ftype %in% "county"]
    # get all blocks in county
    x = counties_as_sites(cfips)
    # get latlon pts
    latlon_join_on_bgid(x)
    # take averages within each county
    x <- x[, .(ftype = "county", lat = mean(lat), lon = mean(lon)), by = "countyfips"]
    setnames(x, "countyfips", "fips")
    # for the subset of the original fips that were county fips,
    y = dtf["county" == ftype, .(  fips)]
    # add latlon columns via merge
    y = merge(x, y, by = "fips", all.x = TRUE)

    if (!("lat" %in% names(dtf)) || !("lat" %in% names(dtf))) {
      dtf$lat = 0
      dtf$lon = 0
    }
    # same sort order?
    dtf["county" == ftype, ] <- y[, .(ftype,fips,lat,lon)]
  }

  ####################################### #
  # states ?
  if ("state" %in% ftype) {
    # for the subset of the original fips that were state fips,
    y = dtf["state" == ftype, .(ftype, fips)]
    x = data.table(stateinfo2[!is.na(stateinfo2$FIPS.ST), c("FIPS.ST", "lat", "lon")])
    setnames(x, "FIPS.ST", 'fips')
    # add latlon columns via merge
    y = merge(x, y, by = "fips", all.y = TRUE)
    if (!("lat" %in% names(dtf)) || !("lat" %in% names(dtf))) {
      dtf$lat = 0
      dtf$lon = 0
    }
    dtf["state" == ftype, ] <- y[, .(ftype, fips, lat, lon)]
  }

  return(dtf[])

}
