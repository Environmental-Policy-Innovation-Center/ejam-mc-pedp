## test-url_ejamapi

test_that("url_ejamapi sitepoints works", {
  expect_no_error({x = url_ejamapi(sitepoints = testpoints_10[1,])})
  expect_no_error({x = url_ejamapi(sitepoints = testpoints_10, radius = 1)})
})
test_that("url_ejamapi fips works", {
  expect_no_error({x = url_ejamapi(fips = blockgroupstats$bgfips[1])})
  expect_no_error({x = url_ejamapi(fips = blockgroupstats$bgfips[1:2], radius = 1)})
})
test_that("url_ejamapi shapefile works", {
  expect_no_error({x = url_ejamapi(shapefile = testinput_shapes_2[1, ])})
  expect_no_error({x = url_ejamapi(shapefile = testinput_shapes_2, radius = 1)})
})
test_that("1 url per sitepoint", {
  expect_no_error({x = url_ejamapi(sitepoints = testpoints_10, radius = 1)})
  expect_equal(length(x), 10)
  expect_true(substr(x[1], 1, 5) == "https")
})


############### test all the other url_xyz functions in a loop:

do_url_tests = function(funcname, FUN) {

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
do_url_tests("url_ejamapi", url_ejamapi)   # fips must be blockgroup fips currently - other types not yet implemented
do_url_tests("url_ejscreenmap", url_ejscreenmap)
do_url_tests("url_echo_facility", url_echo_facility)
do_url_tests("url_frs_facility", url_frs_facility)
do_url_tests("url_enviromapper", url_enviromapper)
do_url_tests("url_countyhealth", url_countyhealth)
do_url_tests("url_statehealth", url_statehealth)

