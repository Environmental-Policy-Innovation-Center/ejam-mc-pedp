
# test-url_ejamapi.R

############### test all the other url_xyz functions in a loop:

do_url_tests = function(funcname, FUN) {

  ## e.g.,
  #   funcname <- "url_ejamapi"; FUN <- NULL

  if (is.null(FUN)) {FUN <- get(funcname)}

  test_that("Site responds with 200", {
    expect_true(url_online(FUN(sitepoints = testpoints_10[1,])))
  })

  # fipsmix = testinput_fips_mix
  fipsmix =  c(
    "091701844002024", # block
    testinput_fips_blockgroups[1],
    testinput_fips_tracts[2],
    "4748000", ## Memphis  # testinput_fips_cities[1],
    testinput_fips_counties[1],
    testinput_fips_states[2]
  )

  test_that(paste0(funcname, " sitepoints works"), {
    expect_no_error({x = FUN(sitepoints = testpoints_10[1,])})
    expect_no_error({x = FUN(sitepoints = testpoints_10, radius = 1)})
  })
  test_that(paste0(funcname, " fips works"), {
    expect_no_error({x = FUN(fips = blockgroupstats$bgfips[1])})
    expect_no_error({x = FUN(fips = blockgroupstats$bgfips[1:2] )})
    expect_no_error({x = FUN(fips = fipsmix)})
  })
  test_that(paste0(funcname, " shapefile works"), {
    expect_no_error({x = FUN(shapefile = testinput_shapes_2[1, ])})
    expect_no_error({x = FUN(shapefile = testinput_shapes_2, radius = 1)})
  })
  test_that(paste0(funcname, " regid works"), {
    expect_no_error({x = FUN(regid = testinput_regid[1])})
    expect_no_error({x = FUN(regid = testinput_regid, radius = 1)})
  })

  test_that(paste0(funcname, " 1 url per sitepoint or regid"), {
    expect_no_error({x = FUN(sitepoints = testpoints_10[1:6, ], radius = 1,
                             # fips = fipsmix[1:6],
                             # shapefile = rbind(testinput_shapes_2,testinput_shapes_2,testinput_shapes_2),
                             regid = testinput_regid[1:6])})
    expect_equal(length(x), 6)
    expect_true(substr(x[1], 1, 5) == "https")
  })
  test_that(paste0(funcname, " 1 url per fips or regid"), {
    expect_no_error({x = FUN( # sitepoints = testpoints_10[1:6, ], radius = 1,
                             fips = fipsmix[1:6],
                             # shapefile = rbind(testinput_shapes_2,testinput_shapes_2,testinput_shapes_2),
                             regid = testinput_regid[1:6])})
    expect_equal(length(x), 6)
    expect_true(substr(x[1], 1, 5) == "https")
  })
  test_that(paste0(funcname, " 1 url per polygon of shapefile or regid"), {
    expect_no_error({x = FUN( # sitepoints = testpoints_10[1:6, ], radius = 1,
                             # fips = fipsmix[1:6],
                             shapefile = rbind(testinput_shapes_2,testinput_shapes_2,testinput_shapes_2),
                             regid = testinput_regid[1:6])})
    expect_equal(length(x), 6)
    expect_true(substr(x[1], 1, 5) == "https")
  })

}
###############

do_url_tests("url_ejamapi", url_ejamapi)   # fips must be blockgroup fips currently - other types not yet implemented 9/2025
