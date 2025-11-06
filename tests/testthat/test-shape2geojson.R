
test_that("converts 1 polygon to text", {
  shp <- testinput_shapes_2[1, ]
  expect_no_error({
    junk = capture_output({
      x = shape2geojson(shp)
    })
    expect_true(class(x) == "character")
    expect_equal(length(x), 1)
  })
})

test_that("converts 2 polygons to 2 strings", {
  expect_no_error({
    junk = capture_output({
      x = shape2geojson(testinput_shapes_2, txt = T, combine_in_one_string = F, combine_in_one_file = T) # ignore combine_in_one_file since txt=T
    })
    expect_true(class(x) == "character")
    expect_equal(length(x), 2)
  })
})

test_that("converts 2 polygons to 2 files", {
  expect_no_error({
    junk = capture_output({
      x = shape2geojson(testinput_shapes_2, txt = F, combine_in_one_file = F, combine_in_one_string = T) # ignore combine_in_one_string since txt=F
    })
  })
  expect_true(class(x) == "character")
  expect_equal(length(x), 2)
  expect_true(x[1] != x[2])
  expect_true(all(file.exists(x)))

  y1 = sf::read_sf(x[1])
  y2 = sf::read_sf(x[2])
  expect_true("sf" %in% class(y1))
  expect_equal(y1$GEOID, testinput_shapes_2[1,]$GEOID)
})

test_that("converts 2 polygons to 1 string if combine_in_one_string=T", {
  expect_no_error({
    junk = capture_output({
      x = shape2geojson(testinput_shapes_2, txt = T, combine_in_one_string = T, combine_in_one_file = F) # ignore combine_in_one_file since txt=T
    })
    expect_true(class(x) == "character")
    expect_equal(length(x), 1)
  })
})

test_that("converts 2 polygons to 1 file of 2 polygons if combine_in_one_file=T", {
  expect_no_error({
    junk = capture_output({
      x = shape2geojson(testinput_shapes_2, txt = F, combine_in_one_file = T, combine_in_one_string = F) # ignore combine_in_one_string since txt=F
    })
    expect_true(class(x) == "character")
    expect_equal(length(x), 1)
    expect_true( (file.exists(x)))

    expect_equal(basename(x), "shp.geojson")
    y = sf::read_sf(x)
    expect_true("sf" %in% class(y))
    expect_equal(NROW(y), 2)
    # expect_equal(y$GEOID, testinput_shapes_2[1,]$GEOID)  # not relevant if dissolved
  })
})

test_that("saves to 1 readable file by default", {
  expect_no_error({
    junk = capture_output({
      x = shape2geojson(testinput_shapes_2[1:2, ], txt = FALSE)
    })
  })
  expect_true(file.exists(x))
  expect_equal(basename(x), "shp.geojson")
  y = sf::read_sf(x)
  expect_true("sf" %in% class(y))
  expect_equal(y$GEOID[2], testinput_shapes_2[2,]$GEOID)
})
