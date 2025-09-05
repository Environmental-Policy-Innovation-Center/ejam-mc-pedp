
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

    dtf_bg = dtf[ftype == "blockgroup", .(ftype, fips)]
    setnames(dtf_bg, 'fips', 'bgfips')
    dtf_bg[bgpts, `:=`(lat = lat, lon = lon), on = "bgfips"]
    setnames(dtf_bg, 'bgfips', 'fips')
    dtf[ftype == "blockgroup", ] <- dtf_bg

    # latlon_from_bgfips = function(fips) {
    #
    #
    # return(dtf)
    # }
    #
    # x <-  latlon_from_bgfips(fips["blockgroup" %in% ftype])


  }
  ####################################### #
  # tracts:
  if ("tract" %in% ftype) {
    # maybe just take average of lat and average of lon of the blockgroups in a tract?



  }
  ####################################### #
  # cities ?
  if ("city" %in% ftype) {


  }

  ####################################### #
  # counties ?
  if ("county" %in% ftype) {
    # use tidycensus:: to get this info??  or shapes_from_fips() ?
    # or just the average lat and lon among blocks in the county

    cfips = fips["county" %in% ftype]
    # get all blocks in county
    x = counties_as_sites(cfips)
    # get latlon pts
    latlon_join_on_bgid(x)
    # take averages within each county
    x <- x[, .(lat = mean(lat), lon = mean(lon)), by = "countyfips"]
    setnames(x, "countyfips", "fips")
    # for the subset of the original fips that were county fips,
    y = dtf["county" == ftype, .(ftype, fips)]
    # add latlon columns via merge
    y = merge(x, y, by = "fips", all.x = TRUE)
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

    dtf["state" == ftype, ] <- y[, .(ftype, fips, lat, lon)]
  }

return(dtf[])

}
