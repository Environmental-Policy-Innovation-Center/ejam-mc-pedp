
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
