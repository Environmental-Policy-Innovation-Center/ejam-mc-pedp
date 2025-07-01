
#' Find all blocks within each of the FIPS codes provided
#'
#' Allows EJAM to analyze and compare Counties, for example
#'
#' @param fips vector of FIPS codes identifying blockgroups, tracts, counties, or states.
#'   This is useful if -- instead of getting stats on and comparing circular buffers or polygons --
#'    one will be getting stats on one or more tracts,
#'   or analyzing and comparing blockgroups in a county,
#'   or comparing whole counties to each other, within a State.
#' @param inshiny used by shiny app server code to handle errors via validate() instead of stop()
#' @param need_blockwt set to FALSE to speed it up if you do not need blockwt
#' @param return_shp set to TRUE to get a named list, pts and polys, that are sites2blocks table and spatial data.frame,
#'   or FALSE to get the pts data.table much like output of [getblocksnearby()] for latlon points.
#' @return same as for [getblocksnearby()] but one row per FIPS, and the distance column is irrelevant
#'
#' @examples
#'   x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"))
#'   y <- doaggregate(x)
#'   z <- ejamit(fips = fips_counties_from_statename("Delaware"))
#'
#'   # x2 <- getblocksnearby_from_fips("482011000011") # one blockgroup only
#'   # y2 <- doaggregate(x2)
#' @seealso [getblocksnearby()] [fips_bgs_in_fips()] [fips_lead_zero()] [getblocksnearby_from_fips()] [fips_from_table()]
#'
#' @export
#'
getblocksnearby_from_fips <- function(fips, inshiny = FALSE, need_blockwt = TRUE, return_shp = FALSE) {

  # If some fips are city and others not, use approp method for each, then combine and re-sort
  original_order <- data.table(n = 1:length(fips), ejam_uniq_id = fips)
  ftype <- fipstype(fips)
  ftype_city <- ftype %in% "city"

  if (any(ftype_city)) {
    # these need shapefile download
    output_city <- getblocksnearby_from_fips_cityshape(fips = fips[ftype_city],
                                                       return_shp = return_shp)
  } else {
    output_city <- NULL
  }
  if (any(!ftype_city)) {
    # these do not need shapefile download, just using FIPS code since blockgroups aggregate into tracts then counties then states
    output_noncity <- getblocksnearby_from_fips_noncity(fips[!ftype_city],
                                                        return_shp = return_shp,
                                                        inshiny = inshiny,
                                                        need_blockwt = need_blockwt)
  } else {
    output_noncity <- NULL
  }

  # Combine city/noncity, and SORT AGAIN in same order as original input fips, in case it was a mix of city and noncity fips

  if (return_shp) {

    output$pts <- rbind(output_city$pts, output_noncity$pts)
    output$shp <- rbind(output_city$shp, output_noncity$shp)

    output$shp <- output$shp[match(fips, output$shp$ejam_uniq_id), ]

    output$pts[original_order, n := n, on = "ejam_uniq_id"]
    setorder(output$pts, n)
    output$pts[, n := NULL]

  } else {

    output <- rbind(output_city, output_noncity)
    output[original_order, n := n, on = "ejam_uniq_id"]
    setorder(output, n)
    output[, n := NULL]
  }
  return(output)
}
######################################## #  ######################################## #


# helper used by getblocksnearby_from_fips()


getblocksnearby_from_fips_cityshape <- function(fips, return_shp = FALSE) {

  polys <- shapes_places_from_placefips(fips)
  polys$ejam_uniq_id <- fips
  # polys[, c("FIPS", "ejam_uniq_id")][] # shows the order of fips was preserved, with NA row if no bounds avail.
  s2b_pts_polys <- get_blockpoints_in_shape(polys = polys)
  if (return_shp) {
    return(s2b_pts_polys) # named list with pts and polys tables
  } else {
    return(s2b_pts_polys$pts) # multiple rows per fips, but the order of fips was preserved, with no row for fips with no bounds avail.
  }

  ## example/test
  # fips = c(4975360, 4262056, 4958070) # 1 of those 3 has no bounds avail.
  # mapview::mapview( shapes_from_fips(fips))
  #
  ## handles ejam_unique_id aka fips ####
  ##   polys$FIPS is in same sort order as fips input, but some are NA if cannot find boundaries shapefile for some cities/cdps !
  ##   get_blockpoints_in_shape() was ignoring FIPS and assigning ejam_uniq_id as 1:NROW(), if ejam_uniq_id column not found,
  ##   but may want to recode this to pass the FIPS codes as ejam_uniq_id ... except some FIPS are NA (if bounds not found)
  ##   so better to add original fips vector to polys. Since NROW is same and sort order is same other than the NA rows, can just add polys$ejam_uniq_id <- fips
  ##
  #  polys[, c("FIPS", "ejam_uniq_id")][]
  ## Simple feature collection with 5 features and 2 fields (with 2 geometries empty)
  ## Geometry type: MULTIPOLYGON
  ## Dimension:     XY
  ## Bounding box:  xmin: -111.9915 ymin: 34.45588 xmax: -82.71834 ymax: 40.80277
  ## Geodetic CRS:  NAD83
  ##         FIPS ejam_uniq_id                       geometry
  ## 144  4975360      4975360 MULTIPOLYGON (((-111.9915 4...
  ## NA      <NA>      4262056             MULTIPOLYGON EMPTY
  ## 2636 3919330      3919330 MULTIPOLYGON (((-82.78998 4...
  ## 3831 1304980      1304980 MULTIPOLYGON (((-83.59302 3...
  ## NA.1    <NA>      5082300             MULTIPOLYGON EMPTY
}
######################################## #  ######################################## #


# helper used by getblocksnearby_from_fips()


getblocksnearby_from_fips_noncity <- function(fips, return_shp = FALSE, inshiny = FALSE, need_blockwt = TRUE) {

  if (!exists('blockid2fips')) {
    dataload_dynamic(varnames = 'blockid2fips')
  }
  if (!exists('bgid2fips')) {
    dataload_dynamic(varnames = 'bgid2fips')
  }

  fips.char <- fips_lead_zero( fips)  # adds leading zeroes and returns as character, 5 characters if seems like countyfips, etc.
  original_order <- data.table(n = seq_along(fips), ejam_uniq_id = fips.char)
  fipslengths <- nchar(fips.char)
  if (!(length(unique(fipslengths)) == 1)) {    # might recode to allow that but it is complicated
    if (inshiny) {
      validate('fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
    } else {
      stop('fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
    }}
  #  see   fipstype() function.
  #
  # > length(unique(substr(blockid2fips$blockfips,1,12)))
  # [1] 242335
  # > length(unique(substr(blockid2fips$blockfips,1,11)))
  # [1] 85395
  # > length(unique(substr(blockid2fips$blockfips,1,5)))
  # [1] 3221

  fips_vec <- fips
  names(fips_vec) <- fips.char # as.character(fips_vec)
  if (length(fips_vec) == 1) {
    fips_vec <- c(fips_vec, na = NA) # quick workaround since code below failed if only 1 FIPS provided, like 1 state or only 1 county
  }
  suppressWarnings({ # because if length was 1 and added NA at end, this reports irrelevant warning
    ## create two-column dataframe with bgs (values) and original fips (ind)
    # fips_bgs_in_fips1() returns all blockgroup fips codes contained within each fips provided
    # fips_bgs_in_fips() replaces fips_bgs_in_fips1()
    # all_bgs <- stack(sapply(fips_vec, fips_bgs_in_fips)) # newer - fast alone but slow in sapply?
    all_bgs <- stack(sapply(fips_vec, fips_bgs_in_fips1)) # Slow:  1.4 seconds for all counties in region 6, e.g.
  })
  names(all_bgs) <- c('bgfips', 'ejam_uniq_id')

  # *** It actually could be more efficient to replace the above fips_bgs_in_fips1()
  # or make a new func to provide bgid_from_anyfips()
  # instead of 1st getting bgfips and then needing to look up bgid by bgfips -
  #
  #    Can we just change to this?...
  #      use fips_bgs_in_fips() to get all bg fips values
  #      use join to blockgroupstats on bgfips, to get all bgid values
  #      use join to blockwts on bgid, to get all the blockid values.
  #
  # Get bgid:
  all_bgs$bgid <- bgid2fips[match(all_bgs$bgfips, bgfips), bgid]

  #### IS THIS RIGHT OR DID IT GET MESSED UP :  ???  bgfips vs site id vs ejam_uniq_id might have gotten mixed up: ***
  all_bgs$ejam_uniq_id <- as.character(all_bgs$ejam_uniq_id) # because stack() always creates a factor column. data.table might have a faster reshaping approach? ***
  # Note that site id in this case actually is the fips provided, like a state fips or county fips vector

  ## only process blockgroups exist for uploaded data
  if (nrow(all_bgs) > 0) {
    # WOULD data.table join or merge be faster than dplyr here? ***
    fips_blockpoints <- dplyr::left_join(all_bgs,
                                         ## create 12-digit column inline (original table not altered)  ## do not actually need blockfips here except to join on its first 12 chars
                                         blockid2fips[, .(blockid, blockfips, blockfips12 = substr(blockfips,1,12))],
                                         by = c('bgfips' = 'blockfips12'), multiple = 'all') |>
      dplyr::left_join(blockpoints) |>
      dplyr::mutate(distance = 0) |>      # or do I want distance to be null, or missing or NA or 0.001, or what? note approximated block_radius_miles is sometimes zero, in blockwts
      data.table::as.data.table()

    if (need_blockwt) {
      # provide blockwt to be consistent with getblocksnearby() and doaggregate() understands it if you want to use it after this.
      #fips_blockpoints[,blockwt := 1] # since doaggregate() uses blockwt even though we know the resulting bgwt will be 1 in every case if used FIPS codes bigger than blocks (blockgroups, tracts, counties, states, whatever)
      fips_blockpoints <- merge(fips_blockpoints, blockwts[,.(blockid, blockwt)], by = "blockid")
    }

    ## remove any invalid  values
    fips_blockpoints <- na.omit(fips_blockpoints)

    # Emulate the normal output of  getblocksnearby() which is a data.table with
    #  ejam_uniq_id, blockid, distance, blockwt, bgid
    # but do not really need to return bgfips, blockfips, lat, lon here.
    setcolorder(fips_blockpoints, c('ejam_uniq_id', 'blockid', 'distance', 'blockwt', 'bgid'))
    fips_blockpoints[ , bgfips := NULL]
    fips_blockpoints[ , blockfips := NULL]
    fips_blockpoints[ , lat := NULL]
    fips_blockpoints[ , lon := NULL]

    # return results sorted in same order as the original input fips
    fips_blockpoints[original_order, n := n, on = "ejam_uniq_id"]
    setorder(fips_blockpoints, n)
    fips_blockpoints[, n := NULL]

    return(fips_blockpoints[])

  } else {
    if (inshiny) {
      ## if not matched, return this message
      shiny::validate('No blockgroups found for these FIP codes.') # A list of tests. Each test should equal NULL for success, FALSE for silent failure, or a string for failure with an error message.
    } else {
      stop('No blockgroups found for these FIP codes.')
    }

  }
}
######################################## #

