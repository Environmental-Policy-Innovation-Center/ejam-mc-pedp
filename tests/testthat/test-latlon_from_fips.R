


test_that("latlon_from_fips works on bg", {
  expect_no_error({
    latlon_from_fips(testinput_fips_blockgroups[1])
    latlon_from_fips(testinput_fips_blockgroups[1:2])
  })
})

test_that("latlon_from_fips works on tract", {
  expect_no_error({
    latlon_from_fips(testinput_fips_tracts[1])
    latlon_from_fips(testinput_fips_tracts[1:2])
  })
})

test_that("latlon_from_fips works on city", {
  expect_no_error({
    latlon_from_fips(testinput_fips_cities[1])
    latlon_from_fips(testinput_fips_cities[1:2])
  })
})

test_that("latlon_from_fips works on county", {
  expect_no_error({
    latlon_from_fips(testinput_fips_counties[1])
    latlon_from_fips(testinput_fips_counties[1:2])
  })
})

test_that("latlon_from_fips works on states", {
  expect_no_error({
    latlon_from_fips(testinput_fips_states[1])
    latlon_from_fips(testinput_fips_states[1:2])
  })
})

test_that("latlon_from_fips works on fipsmix", {
  expect_no_error({
    suppressWarnings({
      x <-    latlon_from_fips(testinput_fips_mix )
    })
  })

})

############################################# #

#
# ## test they are correct latlon in sense of right county?
#
# for ( i in 1:length(  testinput_fips_mix[fipstype(testinput_fips_mix) != "state"])) {
#
#   fips <-  testinput_fips_mix[i]
#
#   suppressWarnings({
#     x <-    latlon_from_fips(fips[i] )
#   })
#
#   test_that(paste0("latlon are in same county as input fips was, for fips is ", fipstype(fips)), {
#
#
#     ## stuck on latlon_from_anything in here:
#
#     s2b = getblocksnearby(sitepoints = data.frame(lat = x$lat, lon = x$lon), radius = 0.5)
#
#
#     cat("fips = ",fips[i], "\n")
#     print(paste0("lat,lon", x$lat, ", ", x$lon, "\n"))
#
#     # get fips from s2b$bgid
#     s2b[bgpts,  fips := bgfips, on = "bgid"]
#     # setnames(s2b, "bgfips", "fips")
#     countyfips_per_latlon  <-  unique(fips2countyfips(s2b$fips))
#
#     expect_true({
#       n = length(intersect(
#         fips2countyfips(fips), countyfips_per_latlon
#       ))
#       if (n == 0) {cat("not the same county!?  \n")}
#       n > 0
#     })
#   })
# }
#

    ############################################# #
