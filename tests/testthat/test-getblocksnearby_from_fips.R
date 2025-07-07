################# #  ################# #  ################# ################## #  ################# #  ################# #
################# #  ################# #  ################# ################## #  ################# #  ################# #
if (FALSE) {
  # large set of test cases

  f1  <- rev(testinput_fips_blockgroups)
  f2  <- rev(testinput_fips_tracts)
  f3  <- rev(testinput_fips_cities)
  f4  <- rev(testinput_fips_counties)
  f5  <- rev(testinput_fips_states)

  testinput_fips_sets <- list(
    # possibly missing boundaries for some?
    `one fipstype, no NA fips` = list(
      bgs     = f1,
      tracts  = f2,
      cities  = f3,
      counties= f4,
      states  = f5
    ),
    `1 has MISSING BOUNDARIES DESPITE VALID FIPS CODE` = list(
      cities = c(f3, "4273072")
    ),
    `mix of just 1 noncity type and city type` = list(
      cities_counties = c(f3, f4)
    ),
    `mix of noncity types, but no city type` = list(
      all_but_cities = c(f1, f2, f4, f5)
    ),
    `mix of all types` = list(
      all = c(f1, f2, f3, f4, f5)
    ),
      # `some fips are 99, some NA` = list(   # DONT WORK
    #   bgs     = c(NA, f1, 99),
    #   tracts  = c(NA, f2, 99),
    #   cities  = c(NA, f3, 99),
    #   counties= c(NA, f4, 99),
    #   states  = c(NA, f5, 99)
    # ),
    # `same fips duplicated in inputs` = list(    # DONT WORK
    #   bgs     = c(f1[1], f1, f1[1]),
    #   tracts  = c(f2[1], f2, f2[1]),
    #   cities  = c(f3[1], f3, f3[1]),
    #   counties= c(f4[1], f4, f4[1]),
    #   states  = c(f5[1], f5, f5[1])
    # ),
    `some fips are NA` = list(
      bgs     = c(NA, f1, NA),
      tracts  = c(NA, f2, NA),
      cities  = c(NA, f3, NA),   # ??
      counties= c(NA, f4, NA),
      states  = c(NA, f5, NA)
    )
  )

  rm(f1, f2, f3, f4, f5)




  for (allow_multiple_fips_types in c(TRUE, FALSE)) {
    cat("\n\n----------------- allow_multiple_fips_types =", allow_multiple_fips_types, ' ---------------------')
    for (return_shp in c(FALSE, TRUE)) {
      cat("\n\n         ----------------- return_shp =", return_shp, ' ---------------------\n\n')

      for (i in seq_along(testinput_fips_sets)) {

        for (ii in seq_along(testinput_fips_sets[[i]])) {

          test_that(paste0(names(testinput_fips_sets)[i],
                           paste0(" (fipstype: ", names(testinput_fips_sets[[i]][ii]), ")"),
                           " (allow_multiple_fips_types=", substr(allow_multiple_fips_types,1,1), ", return_shp=", substr(return_shp,1,1), ")"), {

                             cat(paste0("return_shp=", return_shp, ", allow_multiple_fips_types=", allow_multiple_fips_types, "  --"))
                             cat("  test set name:", names(testinput_fips_sets)[i],
                                 paste0("(fipstype: ", names(testinput_fips_sets[[i]][ii]), ")"),
                                 # "\n   fips: ", paste0(testinput_fips_sets[[i]][ii], collapse = ", "),
                                 "\n")

                             originalfips <- as.character(as.vector(unlist(testinput_fips_sets[[i]][ii])))
                             originalfips_nona = originalfips[!is.na(originalfips)]

                             if (allow_multiple_fips_types == FALSE & grepl("mix", names(testinput_fips_sets)[i])) {
                               # ("allow_multiple_fips_types=FALSE but this test set has mixed fips types")

                               expect_error({
                                 junk = capture_output({ suppressMessages({
                                   x <- getblocksnearby_from_fips(
                                     originalfips,
                                     return_shp = return_shp,
                                     allow_multiple_fips_types = allow_multiple_fips_types
                                   )
                                 })})
                               })
                             } else {

                               expect_no_error({
                                 junk = capture_output({ suppressMessages({
                                   x <- getblocksnearby_from_fips(
                                     originalfips,
                                     return_shp = return_shp,
                                     allow_multiple_fips_types = allow_multiple_fips_types
                                   )
                                 })})
                               })

                               if (return_shp) {
                                 # s2b table lacks the NAs but otherwise same ordering
                                 expect_equal(unique(x$pts$fips),                   originalfips_nona)
                                 expect_equal(unique(x$pts$ejam_uniq_id), seq_along(originalfips_nona))
                                 expect_equal(x$polys$FIPS,                   originalfips)
                                 expect_equal(x$polys$ejam_uniq_id, seq_along(originalfips))
                               } else {
                                 # s2b table lacks the NAs but otherwise same ordering
                                 expect_equal(unique(x$fips),                   originalfips_nona)  # not working
                                 # expect_equal(unique(x$ejam_uniq_id), seq_along(originalfips_nona)) ########## #  seems to be one extra/one missing for some fips are NA bgs
                               }
                             } # end no mixed types case
                           }) # end test_that
        }

      }
    }
  }
  cat('\n\n')

}
################# #  ################# #  ################# ################## #  ################# #  ################# #
################# #  ################# #  ################# ################## #  ################# #  ################# #

testthat::test_that("getblocksnearby_from_fips works at all", {

  testthat::capture_output({

    testthat::expect_no_error({
      x <- getblocksnearby_from_fips("482011000011") # one blockgroup only
      # y=doaggregate(x)
    }
    )
    testthat::expect_no_error({
      x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"), in_shiny = F, need_blockwt = TRUE)
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

testthat::test_that("getblocksnearby_from_fips works return_shp=T", {

  testthat::capture_output({

    testthat::expect_no_error({
      x <- getblocksnearby_from_fips("482011000011", return_shp=T) # one blockgroup only
      # y=doaggregate(x)
    }
    )
    testthat::expect_no_error({
      x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"), in_shiny = F, need_blockwt = TRUE, return_shp=T)
    })
    expect_setequal(names(x), c("polys", "pts"))
    expect_true("sf" %in% class(x$polys))
    testthat::expect_setequal(
      names(x$pts),
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
  suppressMessages({
    suppressWarnings({
      s2b <- getblocksnearby_from_fips(inputfips)
    })
  })
  outputfips <- unique(s2b$ejam_uniq_id, )
  # cbind(inputfips, outputfips)
  expect_equal(outputfips, inputfips)
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
  suppressMessages({  # messages about downloading bounds
    suppressWarnings({
      s2b <- getblocksnearby_from_fips(inputfips)
    })
  })
  # confirms each input is reflected in outputs even if fips was NA or bounds not downloaded
  expect_true(length(unique(s2b$ejam_uniq_id)) == length(inputfips))
  # the invalid FIPS and valid fips with no polygons available appear as NA in outputs
  expect_true(is.na(s2b$distance[s2b$ejam_uniq_id == 1]))
  expect_true(is.na(s2b$distance[s2b$ejam_uniq_id == 5]))
  expect_true(all(s2b$distance == 0, na.rm = T))

})
################# #  ################# #  ################# #


test_that("getblocksnearby_from_fips() handles mix of city & noncity FIPS", {

  inputfips = c("061090011001", # bg
                "4273072", "1332412", "3920212", # cities where 1 lacks bounds available
                "530530723132" , NA, NA, "240338052021", "390490095901", # bgs AND 2 fips are given as NA
                "99") # Totally INVALID FIPS code but not NA value
  suppressMessages({
    suppressWarnings({
      s2b <- getblocksnearby_from_fips(inputfips)
    })
  })
  inputfips_county  <- unique(substr(inputfips,  1, 5)) # unique() since NA values would mess up the comparison
  outputfips_county <- unique(substr(s2b$bgfips, 1, 5)) # unique() since NA values would mess up the comparison
  # cbind(outputfips_county, inputfips_county)
  # expect_equal(substr(s2b$bgfips[1], 1, 2), "06") # is bgfips in s2b ?
  expect_equal(NROW(s2b) > 3 * length(inputfips)) # at least 3 blocks per fips on avg

  expect_equal(length(unique(outputfips_county)), length(unique(inputfips_county))) #
  # NOTE  NA VALUES FOR 2 FIPS ARE NOT COUNTED AS 2 UNIQUE OUTPUTS
  expect_equal(outputfips, inputfips)
  expect_equal(unique(s2b$distance), c(0,NA))

})
################# #  ################# #  ################# #

