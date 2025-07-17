################# #  ################# #  ################# ################## #  ################# #  ################# #
################# #  ################# #  ################# ################## #  ################# #  ################# #
# if (FALSE) {
  #   x = function() {
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
      cities  = c(NA, f3, NA),   # ??  f3 is "2743000" "2743306"
      counties= c(NA, f4, NA),
      states  = c(NA, f5, NA)
    )
  )

  rm(f1, f2, f3, f4, f5)



  for (allow_multiple_fips_types in TRUE) {
#  for (allow_multiple_fips_types in c(TRUE, FALSE)) {   # nonessential to allow FALSE here
    cat("\n\n----------------- allow_multiple_fips_types =", allow_multiple_fips_types, ' ---------------------')

    for (return_shp in c(FALSE, TRUE)) {
      cat("\n\n         ----------------- return_shp =", return_shp, ' ---------------------\n\n')

      for (i in seq_along(testinput_fips_sets)) {

        for (ii in seq_along(testinput_fips_sets[[i]])) {

     try({     test_that(paste0(names(testinput_fips_sets)[i],
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

                               expect_error({            # or may just warn now
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



                                 # ***    fix tests for return_shp case:     failed if any NA values



                                 # s2b table lacks the NAs but otherwise same ordering?
                                 expect_equal(unique(x$pts$ejam_uniq_id), seq_along(originalfips_nona))
                                 expect_equal(unique(x$pts$fips),                   originalfips_nona) # Error in `x$pts`: object of type 'closure' is not subsettable

                                 expect_true("sf" %in% class(x$polys))
                                 expect_equal(x$polys$ejam_uniq_id, seq_along(originalfips))

                                 expect_setequal(unique(x$polys$FIPS), unique(originalfips))
                                 expect_equal(x$polys$FIPS,                   originalfips)  # even NAs?

                               } else {




                                 ## ***    add appropriate tests here



                                 expect_equal(unique(x$ejam_uniq_id), seq_along(originalfips_nona)) ########## #  seems to be one extra/one missing for some fips are NA bgs

                                 # s2b table lacks the NAs but otherwise same ordering?
                                 expect_equal(unique(x$fips),                   originalfips_nona)  # not working for city case

                               }
                             } # end no mixed types case
                           }) # end test_that
     }) # end try
        }

      }
    }
  }
  cat('\n\n')

# }

################# #  ################# #  ################# ################## #  ################# #  ################# #
################# #  ################# #  ################# ################## #  ################# #  ################# #

testthat::test_that("getblocksnearby_from_fips works 1 bg", {
  testthat::capture_output({
    testthat::expect_no_error({
      x <- getblocksnearby_from_fips("482011000011") # one blockgroup only
    })
    testthat::expect_setequal(
      names(x),
      c("ejam_uniq_id", "blockid", "distance", "blockwt", "bgid", "fips")
    )
  })
})
################# #  ################# #  ################# ################## #  ################# #  ################# #

testfips_list <- list(testinput_fips_blockgroups, testinput_fips_tracts, testinput_fips_cities, testinput_fips_counties, testinput_fips_states)

for (testfips in testfips_list) {
  testfips <- testfips[1:2]
  ftype <- fipstype(testfips)[1]
  testthat::test_that(paste0("getblocksnearby_from_fips colnames,fips,id ok for", ftype), {
    testthat::capture_output({
      testthat::expect_no_error({
        suppressMessages({
          x <- getblocksnearby_from_fips(
            testfips
            # , return_shp = FALSE, allow_multiple_fips_types = FALSE
          )
        })
      })
    })
    testthat::expect_setequal(
      names(x),
      c("ejam_uniq_id", "blockid", "distance", "blockwt", "bgid", "fips")
    )
    testthat::expect_equal(length(unique(x$ejam_uniq_id)), length(testfips))
    testthat::expect_equal(unique(x$fips), testfips)
    testthat::expect_equal(state_from_nearest_block_bysite(x)$ST, fips2state_abbrev(testfips))
  })
}
rm(testfips_list, testfips, ftype)
################# #  ################# #  ################# ################## #  ################# #  ################# #

testfips_list <- list(testinput_fips_blockgroups, testinput_fips_tracts, testinput_fips_cities, testinput_fips_counties, testinput_fips_states)
# one_of_each
testfips <- sapply(testfips_list, function(x) x[1])

testthat::test_that(paste0("getblocksnearby_from_fips colnames,fips,id  ok for mix of types"), {
  testthat::capture_output({
    testthat::expect_no_error({
      x <- getblocksnearby_from_fips(
        testfips
        # , return_shp = FALSE, allow_multiple_fips_types = FALSE
      )
    })
  })
  testthat::expect_setequal(
    names(x),
    c("ejam_uniq_id", "blockid", "distance", "blockwt", "bgid", "fips")
  )
  testthat::expect_equal(length(unique(x$ejam_uniq_id)), length(testfips))
  testthat::expect_equal(unique(x$fips), testfips)
  testthat::expect_equal(state_from_nearest_block_bysite(x)$ST, fips2state_abbrev(testfips))
})

rm(testfips_list, testfips)
################# #  ################# #  ################# ################## #  ################# #  ################# #


################# #  ################# #  ################# #

testthat::test_that("getblocksnearby_from_fips works return_shp=T", {

  testthat::capture_output({

    testthat::expect_no_error({
      suppressMessages({
        x <- getblocksnearby_from_fips("482011000011", return_shp=T) # one blockgroup only
        # y=doaggregate(x)
      })
    })

    testthat::expect_no_error({
      suppressMessages({
        x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"), in_shiny = F, need_blockwt = TRUE, return_shp=T)
      })
    })

    expect_setequal(names(x), c("polys", "pts"))
    expect_true("sf" %in% class(x$polys))
    testthat::expect_setequal(
      names(x$pts),
      c("ejam_uniq_id", "blockid", "distance", "blockwt", "bgid", "fips")
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
  outputfips <- unique(s2b$fips)
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


test_that("getblocksnearby_from_fips() returns NA, handles mix of city & noncity FIPS", {

  inputfips = c("061090011001", # bg
                "4273072", "1332412", "3920212", # cities where 1 lacks bounds available
                "530530723132" , NA, NA, "240338052021", "390490095901", # bgs AND 2 fips are given as NA
                "99") # Totally INVALID FIPS code but not NA value
  suppressMessages({
    suppressWarnings({
      s2b <- getblocksnearby_from_fips(inputfips)
    })
  })

  ### NOTE HOW NA AND INVALID FIPS ARE HANDLED ! ******************************************   same ejam_uniq_id in s2b appears for NA row as valid rows !

  # > unique(s2b[,.(ejam_uniq_id, fips)])
  #    ejam_uniq_id         fips
  #           <int>       <char>
  # 1:            1 061090011001
  # 2:            2      4273072
  # 3:            3      1332412
  # 4:            4         <NA>     6 & 7 were supposed to be the NA rows, but merging city and noncity fails to keep tract across types?
  # 5:            4      3920212
  # 6:            5         <NA>
  # 7:            5 530530723132
  # 8:            8 240338052021
  # 9:            9 390490095901

  expect_false(99 %in% unique(s2b$fips))
  expect_true(NA %in% unique(s2b$fips))

  # > 1:length(inputfips)
  # [1]  1  2  3  4  5  6  7  8  9 10
  # > unique(s2b$ejam_uniq_id)
  # [1] 1 2 3 4 5 8 9            # drops the fips that is invalid but not NA (99). keeps only 1 of the 2 NA values?

  # > inputfips
  # [1] "061090011001" "4273072"      "1332412"            "3920212"      "530530723132"   NA   NA   "240338052021" "390490095901"   "99"
  # >  unique(s2b$fips)
  # [1] "061090011001" "4273072"      "1332412"      NA    "3920212"      "530530723132"             "240338052021" "390490095901"  # has   NAs but not shown here as unique, and in the wrong place?, and lacks the fips that is 99 invalid but not NA

  ### NA values retained but out of order?
  # >   which(is.na(inputfips))
  # [1] 6 7
  # > s2b$ejam_uniq_id[is.na(s2b$fips)]
  # [1] 4 5

  # all.equal(length(unique(s2b$ejam_uniq_id)), length(inputfips[fips_valid(inputfips)]))

  ## outputs will include NA but not 99 as row with fips in s2b
  outputfips <- unique(s2b$fips)
  ## THIS IS ONLY TRUE IF REMOVING INVALID fips including NA values and 99, or other bad numbers!!
  # cbind(inputfips[fips_valid(inputfips)], outputfips[!is.na(outputfips)])
  expect_equal(inputfips[fips_valid(inputfips)], outputfips[!is.na(outputfips)])

  expect_true(NROW(s2b) > 3 * length(inputfips)) # at least 3 blocks per fips on avg

  expect_equal(unique(s2b$distance), c(0,NA))

})
################# #  ################# #  ################# #

