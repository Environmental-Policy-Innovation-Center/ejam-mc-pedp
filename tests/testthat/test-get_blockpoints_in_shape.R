
# get_blockpoints_in_shape


test_that("get_blockpoints_in_shape() outputs NA rows as needed, sorted as input", {

  shp = testinput_shapes_2

  # ONE INVALID ROW? not sure that could happen
  shp[3, ] <- NA

  suppressWarnings({
    s2b <- get_blockpoints_in_shape(polys = shp)
  })

  # cbind(inputfips, outputfips)
  expect_equal(unique(s2b$polys$ejam_uniq_id), 1:NROW(shp))
  expect_equal(unique(s2b$pts$ejam_uniq_id), 1:NROW(shp))
  expect_equal(unique(s2b$pts$distance), c(0, NA))
})
################# #  ################# #  ################# #

# larger shapefile

test_that("get_blockpoints_in_shape() larger shapefile", {

  shp = shapefile_from_any(testdata("gdb", quiet = T)[1])

  suppressWarnings({
    s2b <- get_blockpoints_in_shape(polys = shp)
  })
  expect_equal(s2b$polys, shp)
  expect_equal(unique(s2b$polys$ejam_uniq_id), 1:NROW(shp))
  expect_equal(unique(s2b$pts$ejam_uniq_id), 1:NROW(shp))

})
################# #  ################# #  ################# #
