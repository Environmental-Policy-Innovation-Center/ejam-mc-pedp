
#' Find all blocks within each of the FIPS codes provided
#'
#' Allows EJAM to analyze and compare Counties, for example
#'
#' @param fips vector of FIPS codes identifying blockgroups, tracts, counties, or states.
#'   This is useful if -- instead of getting stats on and comparing circular buffers or polygons --
#'    one will be getting stats on one or more tracts,
#'   or analyzing and comparing blockgroups in a county,
#'   or comparing whole counties to each other, within a State.
#' @param in_shiny used by shiny app server code to handle errors via validate() instead of stop()
#' @param need_blockwt set to FALSE to speed it up if you do not need blockwt
#' @param return_shp set to TRUE to get a named list, pts and polys, that are sites2blocks table and spatial data.frame,
#'   or FALSE to get the pts data.table much like output of [getblocksnearby()] for latlon points.
#' @param allow_multiple_fips_types if enabled, set TRUE to allow mix of blockgroup, tract, city, county, state fips
#' @return data.table with colnames ejam_uniq_id, blockid, distance, blockwt, bgid, fips.
#'
#'   [getblocksnearby()] output is similar but
#'   `getblocksnearby(testpoints_10, radius = 1)`
#'   also returns extra columns from input, like lat,lon.
#'
#'   [get_blockpoints_in_shape()] output is similar but
#'   `get_blockpoints_in_shape(testinput_shapes_2)`
#'   returns a list of pts table and polys table, where the pts data.table has
#'   colnames ejam_uniq_id, blockid, distance, blockwt, bgid, lat, lon.
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
getblocksnearby_from_fips <- function(fips, in_shiny = FALSE, need_blockwt = TRUE,
                                      return_shp = FALSE, allow_multiple_fips_types = TRUE) {

  ## NOTE       getblocksnearby_from_fips()           was using fips as the output ejam_uniq_id but now will use 1:NROW() like other getblock... functions do
  ## AND NOW,   getblocksnearby_from_fips_noncity()   same
  ## AND NOW,   getblocksnearby_from_fips_cityshape() same

  suppressWarnings({
    fips <- fips_lead_zero(fips)  # adds leading zeroes and returns as character, 5 characters if seems like countyfips, etc.
  })
  # create an overall ejam id, and note the ejam_uniq_id in each of the helpers will be a different count of 1:A and 1:B for the cityshape and noncity cases!
  original_order <- data.table(ejam_uniq_id = seq_along(fips), fips = fips)

  # If some fips are city and others not, use approp method for each, then combine and re-sort
  suppressWarnings({
    ftype <- fipstype(fips)
  })
  ftype_city <- ftype %in% "city"
  ftype_city[is.na(ftype)] <- NA

  if (any(na.omit(ftype_city))) {
    # these need shapefile download
    output_city <- getblocksnearby_from_fips_cityshape(fips = fips[ftype_city],
                                                       return_shp = return_shp)
  } else {
    output_city <- NULL
  }
  if (any(!na.omit(ftype_city))) {
    # these do not need shapefile download, just using FIPS code since blockgroups aggregate into tracts then counties then states
    output_noncity <- getblocksnearby_from_fips_noncity(fips[!ftype_city],
                                                        return_shp = return_shp,
                                                        in_shiny = in_shiny,
                                                        need_blockwt = need_blockwt,
                                                        allow_multiple_fips_types = allow_multiple_fips_types)
  } else {
    output_noncity <- NULL
  }

  # Combine city/noncity, and SORT AGAIN in same order as original input fips, in case it was a mix of city and noncity fips
  ## each part of s2b table (city and noncity) is already in correct sort order at this point, but need to sort overall now,
  # using  fips[ftype_city] and fips[!ftype_city] and original_order

  original_order[            , ftype_city := ftype_city]
  original_order[  ftype_city, id_among_subset := .I] # 1:N just among the city subset
  original_order[ !ftype_city, id_among_subset := .I] # 1:N just among the non-city subset
  original_order[            , id_overall := ejam_uniq_id]

  if (return_shp) {
    # return_shp case ####

    ## 1a. pull in id_overall as the new ejam_uniq_id value ####
    # in output_city$pts, and then in output_noncity$pts
    # by joining each to original_order on = "id_among_subset"

    if (!is.null(output_city)) {
      output_city$pts[, id_among_subset := ejam_uniq_id]
      output_city$pts[original_order[ ftype_city, ], ejam_uniq_id := id_overall, on = "id_among_subset"]
      output_city$pts[, id_among_subset := NULL]
    }
    if (!is.null(output_noncity)) {
      output_noncity$pts[, id_among_subset := ejam_uniq_id]
      output_noncity$pts[original_order[!ftype_city, ], ejam_uniq_id := id_overall, on = "id_among_subset"]
      output_noncity$pts[, id_among_subset := NULL]
    }

    # 1b. pull in id_overall as the new ejam_uniq_id value
    # in output_city$polys, output_noncity$polys
    output_city$polys$ejam_uniq_id    <- original_order$ejam_uniq_id[ ftype_city]
    output_noncity$polys$ejam_uniq_id <- original_order$ejam_uniq_id[!ftype_city]

    ## 2. combine city/non ####
    output <- list()
    ## a way to combine spatial data.frames that do not all have the same columns:
    output$polys <- rbindlist(list(output_city$polys, output_noncity$polys), fill = TRUE)
    output$pts   <- rbind(         output_city$pts,   output_noncity$pts)

    ## 3. sort s2b ####
    ## sort pts data.table using data.table syntax, in same order as original inputs were:
    # now that overall ejam_uniq_id is here, sort on that, since it was just 1:N
    setorder(output$pts, ejam_uniq_id, blockid)

    ## 4. sort spatial data.frame ####
    # sort  ejam_uniq_id which now is original overall  1:NROW
    output$polys <- output$polys[order(output$polys$ejam_uniq_id), ]

  } else {
    # NOT return_shp case ####
    # each is just a data.table of s2b like pts

    ## 1a. pull in id_overall as the new ejam_uniq_id value ####
    # in output_city, and then in output_noncity
    # by joining each to original_order on = "id_among_subset"

    if (!is.null(output_city)) {
      output_city[, id_among_subset := ejam_uniq_id]
      output_city[original_order[ ftype_city, ], ejam_uniq_id := id_overall, on = "id_among_subset"]
      output_city[, id_among_subset := NULL]
    }
    if (!is.null(output_noncity)) {
      output_noncity[, id_among_subset := ejam_uniq_id]
      output_noncity[original_order[!ftype_city, ], ejam_uniq_id := id_overall, on = "id_among_subset"]
      output_noncity[, id_among_subset := NULL]
    }

    ## 2. combine city/non ####
    ## a way to combine spatial data.frames that do not all have the same columns:
    output <- rbindlist(list(output_city, output_noncity), fill = TRUE)

    ## 3. sort s2b ####
    # sort data.table using data.table syntax, in same order as original inputs were:
    # now that overall ejam_uniq_id is here, sort on that, since it was just 1:N
    setorder(output, ejam_uniq_id, blockid)
  }
  return(output)
}
######################################## #  ######################################## #


# helper used by getblocksnearby_from_fips()


getblocksnearby_from_fips_cityshape <- function(fips, return_shp = FALSE) {

  suppressWarnings({
    fips <- fips_lead_zero(fips)  # adds leading zeroes and returns as character, 5 characters if seems like countyfips, etc.
  })

  polys <- shapes_places_from_placefips(fips) # preserves exact order, and includes NAs in output if NAs in input. prints info to console.
  polys <- polys[match(fips, polys$FIPS), ] # adds back in NA rows where fips was NA if missing (but was already handled by shapes_places_from_placefips() )
  s2b_pts_polys <- get_blockpoints_in_shape(polys = polys) # had NA row in output for each NA input. Sorted by 1:N ejam_uniq_id, with multiple rows each

  ## s2b_pts_polys$polys is a spatial df with FIPS character like fips, and ejam_uniq_id is 1:nrow integer class (since the input is polygons not fips codes)
  ## s2b_pts_polys$pts is a data.table with no fips field, and   ejam_uniq_id is integer class 1:nrow but check sort order of it.

  # SORT
  # table of block points is already sorted, and has many rows (blocks) per fips
  # table of polygons is also already sorted, and has 1 row per fips
  s2b_pts_polys$pts[s2b_pts_polys$polys, fips := FIPS, on = "ejam_uniq_id"]
  s2b_pts_polys$pts[, lat := NULL]
  s2b_pts_polys$pts[, lon := NULL]

  if (return_shp) {
    return(s2b_pts_polys) # named list with pts and polys tables
  } else {
    return(s2b_pts_polys$pts) # multiple rows per fips, but the order of fips was preserved, with no row for fips with no bounds avail.
  }

  ## example/test
  # fips = c(4975360, 4262056, 4958070) # 1 of those 3 has no bounds avail.
  # mapview::mapview( shapes_from_fips(fips))
}
######################################## #  ######################################## #


# helper used by getblocksnearby_from_fips()


getblocksnearby_from_fips_noncity <- function(fips, return_shp = FALSE, in_shiny = FALSE, need_blockwt = TRUE, allow_multiple_fips_types = TRUE) {

  if (!exists('blockid2fips')) {
    dataload_dynamic(varnames = 'blockid2fips')
  }
  if (!exists('bgid2fips')) {
    dataload_dynamic(varnames = 'bgid2fips')
  }
  suppressWarnings({
    fips <- fips_lead_zero( fips)  # adds leading zeroes and returns as character, 5 characters if seems like countyfips, etc.
  })
  ######################################## #
  # unlike getblocksnearby_from_fips(), for now this function needs all fips to be the same type, like all "county", not a mix of county and city
  #
  if (!allow_multiple_fips_types) {
    suppressWarnings({ftypes <- fipstype(fips)})
    if (length(unique(ftypes)) != 1) {
      if (in_shiny) {
        validate('noncity fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
      } else {
        stop('noncity fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
      }}
    #  see   fipstype() function.
    # > length(unique(substr(blockid2fips$blockfips,1,12)))  # [1] 242335
    # > length(unique(substr(blockid2fips$blockfips,1,11)))  # [1] 85395
    # > length(unique(substr(blockid2fips$blockfips,1,5)))  # [1] 3221
  }

  fips_vec <- fips
  names(fips_vec) <- fips
  if (length(fips_vec) == 1) {
    fips_vec <- c(fips_vec, na = NA) # quick workaround since code below failed if only 1 FIPS provided, like 1 state or only 1 county
  }
  ######################################## #

  ## Get all BLOCKGROUPS in each fips ####

  ### Get bgfips:
  suppressWarnings({ # because if length was 1 and added NA at end, this reports irrelevant warning
    ## create two-column dataframe with bgs (values) and original fips (ind)
    # fips_bgs_in_fips1() returns all blockgroup fips codes contained within each fips provided
    # fips_bgs_in_fips() replaces fips_bgs_in_fips1()
    # all_bgs <- stack(sapply(fips_vec, fips_bgs_in_fips)) # newer - fast alone but slow in sapply?
    # *** Note this stack() drops all NA fips, including any originally in the inputs passed to getblocks, and we now may want to retain those, or can add back in later

    all_bgs <- stack(sapply(fips_vec, fips_bgs_in_fips1)) # Slow:  1.4 seconds for all counties in region 6, e.g.
  })
  names(all_bgs) <- c('bgfips', 'fips')
  all_bgs$ejam_uniq_id <- as.integer(all_bgs$fips) # creates 1:N but multiple copies so it is 1 id for each fips
  all_bgs$fips <- as.character(all_bgs$fips)# because stack() always creates a factor column. data.table might have a faster reshaping approach? ***

  ### Get bgid:
  all_bgs$bgid <- bgid2fips[match(all_bgs$bgfips, bgfips), bgid]
  ######################################## #

  ######################################## #
  # *** It actually could be more efficient to replace the above fips_bgs_in_fips1()
  # or make a new func to provide bgid_from_anyfips()
  # instead of 1st getting bgfips and then needing to look up bgid by bgfips -
  #
  #    Can we just change to this?...
  #      use fips_bgs_in_fips() to get all bg fips values
  #      use join to blockgroupstats on bgfips, to get all bgid values
  #      use join to blockwts on bgid, to get all the blockid values.
  ######################################## #

  if (NROW(all_bgs) == 0) {
    if (in_shiny) {
      shiny::validate('No blockgroups found for these FIP codes.')
    } else {
      stop('No blockgroups found for these FIP codes.') # or just give a warning? ***
    }
  } else {

    ## Get all BLOCKS in each blockgroup ####

    # WOULD data.table join or merge be faster than dplyr here? ***
    suppressMessages({
    fips_blockpoints <- dplyr::left_join(all_bgs,
                                         ## create 12-digit column inline (original table not altered)
                                         ## do not actually need blockfips here except to join on its first 12 chars
                                         blockid2fips[, .(blockid, blockfips, blockfips12 = substr(blockfips,1,12))],
                                         by = c('bgfips' = 'blockfips12'), multiple = 'all') |>
      dplyr::left_join(blockpoints) |>
      dplyr::mutate(distance = 0) |>      # or do I want distance to be null, or missing or NA or 0.001, or what? note approximated block_radius_miles is sometimes zero, in blockwts
      data.table::as.data.table() # makes it a data.table
    })
    if (need_blockwt) {
      # provide blockwt to be consistent with getblocksnearby() and doaggregate() understands it if you want to use it after this.
      #fips_blockpoints[,blockwt := 1] # since doaggregate() uses blockwt even though we know the resulting bgwt will be 1 in every case if used FIPS codes bigger than blocks (blockgroups, tracts, counties, states, whatever)
      fips_blockpoints <- merge(fips_blockpoints, blockwts[,.(blockid, blockwt)], by = "blockid")
    }

    # Emulate the normal output of  getblocksnearby() which is a data.table with
    #  ejam_uniq_id, blockid, distance, blockwt, bgid
    # but do not really need to return bgfips, blockfips, lat, lon here.
    setcolorder(fips_blockpoints, c('ejam_uniq_id', 'blockid', 'distance', 'blockwt', 'bgid'))
    fips_blockpoints[ , bgfips := NULL]
    fips_blockpoints[ , blockfips := NULL]
    fips_blockpoints[ , lat := NULL]
    fips_blockpoints[ , lon := NULL]
    ######################################## #

    ## handle NAs? ####
    ## remove any invalid  values? but it is easier to ensure output matches input if NA invalid fips result in NA rows in output of getblocksnearby as is done by getblocksnearby() in the latlon case
    # fips_blockpoints <- na.omit(fips_blockpoints)

    ## SORT output again just in case (and include NA rows if any were in inputs?) ####
    setorder(fips_blockpoints, ejam_uniq_id)

    ## get boundaries if return_shp ####
    if (return_shp) {
      # do not need to do what getblocksnearby_from_fips_cityshape() since the pts part we can get from FIPS. just need the shapefile polygons part.
      s2b_pts_polys <- list()
      s2b_pts_polys$pts <- fips_blockpoints
      polys <- shapes_from_fips(fips, allow_multiple_fips_types = allow_multiple_fips_types) # preserves exact order, and includes NAs in output if NAs in input
      s2b_pts_polys$polys <- polys
      return(s2b_pts_polys)
    } else {
      return(fips_blockpoints)
    }
  }
}
######################################## #
