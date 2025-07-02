
testthat::test_that("getblocksnearby_from_fips works at all", {

  testthat::capture_output({

    testthat::expect_no_error({
      x <- getblocksnearby_from_fips("482011000011") # one blockgroup only
      # y=doaggregate(x)
    }
    )
    testthat::expect_no_error({
      x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"), inshiny = F, need_blockwt = TRUE)
    })
    testthat::expect_setequal(
      names(x),
      c("ejam_uniq_id" ,"blockid"  ,    "distance"  ,   "blockwt"  ,    "bgid")
    )
    # counties_ej <- doaggregate(x)
    #cannot use mapfast(counties_ej$results_bysite) since no lat lon.  mapfastej_counties() should work...

  })

})
################# #  ################# #  ################# #

test_that("getblocksnearby_from_fips() noncity outputs NA rows as needed, sorted as input", {

  # getblocksnearby_from_fips_noncity() is used

  inputfips = c("061090011001" ,"530530723132" ,"340230083002" ,"240338052021", "390490095901")
  # ONE INVALID ROW:
  inputfips[3  ] <- NA
  suppressWarnings({
    s2b <- getblocksnearby_from_fips(inputfips)
  })
  outputfips <- unique(s2b$ejam_uniq_id)
  # cbind(inputfips, outputfips)
  expect_equal(inputfips, outputfips)
  expect_equal(unique(s2b$distance), c(0,NA))
})
################# #  ################# #  ################# #

test_that("getblocksnearby_from_fips() city outputs NA rows as needed, sorted as input", {

  # getblocksnearby_from_fips_cityshape() is used

  # ONE SORT OF VALID ROW- FIPS IS REAL BUT CANNOT GET POLYGONS:
  ## cannot obtain shapefile for "4273072" so shape_from_fips() returns NA row for that one, empty polygon
  x <- c("4273072", "1332412", "3920212", "2966134", "4272168")
  # AND ONE INVALID ROW:
  x[5] <- NA
  inputfips <- x

# undebug(getblocksnearby_from_fips)
#   undebug(getblocksnearby_from_fips_cityshape)
#   undebug(shapes_places_from_placefips)
#   undebug(get_blockpoints_in_shape)
#   rm(outputfips)

  suppressWarnings({
    s2b <- getblocksnearby_from_fips(inputfips)
  })
  # confirms each input is reflected in outputs even if fips was NA or bounds not downloaded
  expect_true(length(unique(s2b$ejam_uniq_id)) == length(inputfips))
  # the invalid FIPS and valid fips with no polygons available appear as NA in outputs
  expect_true(is.na(s2b$distance[s2b$ejam_uniq_id == 1]))
  expect_true(is.na(s2b$distance[s2b$ejam_uniq_id == 5]))
  expect_true(all(s2b$distance == 0, na.rm = T))

})
################# #  ################# #  ################# #
