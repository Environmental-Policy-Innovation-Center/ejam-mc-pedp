
test_that("ejam2histogram default ok", {
  expect_no_error({
    x = ejam2histogram(testoutput_ejamit_1000pts_1miles$results_bysite)
  })
  expect_true("ggplot" %in% class(x))
})

test_that("ejam2histogram SHP ok", {
  expect_no_error({
    x = ejam2histogram(testoutput_ejamit_1000pts_1miles$results_bysite, sitetype = "SHP")
  })
  expect_true("ggplot" %in% class(x))
})


test_that("ejam2histogram sites pctile ok", {
  expect_no_error({
    x = ejam2histogram(testoutput_ejamit_1000pts_1miles, distn_type = "Sites", data_type = "pctile",
                       ylab_sites = "test ylab_sites",
                       title_sites_pctile = 1,
                       title_sites_raw = 2,
                       title_people_pctile = 3,
                       title_people_raw = 4)
  })
  expect_true("ggplot" %in% class(x))
  expect_equal(x$labels$title, 1)
  expect_equal(x$labels$y, "test ylab_sites")
})

test_that("ejam2histogram sites raw ok", {
  expect_no_error({
    x = ejam2histogram(testoutput_ejamit_1000pts_1miles, distn_type = "Sites", data_type = "raw",
                       title_sites_pctile = 1,
                       title_sites_raw = 2,
                       title_people_pctile = 3,
                       title_people_raw = 4)
  })
  expect_true("ggplot" %in% class(x))
  expect_equal(x$labels$title, 2)
})

test_that("ejam2histogram people pctile ok", {
  expect_no_error({
    x = ejam2histogram(testoutput_ejamit_1000pts_1miles, distn_type = "People", data_type = "pctile",
                       ylab_people = "test ylab_people",
                       title_sites_pctile = 1,
                       title_sites_raw = 2,
                       title_people_pctile = 3,
                       title_people_raw = 4)
  })
  expect_true("ggplot" %in% class(x))
  expect_equal(x$labels$title, 3)
  expect_equal(x$labels$y, "test ylab_people")
})

test_that("ejam2histogram people raw ok", {
  expect_no_error({
    x = ejam2histogram(testoutput_ejamit_1000pts_1miles, distn_type = "People", data_type = "raw",
                       title_sites_pctile = 1,
                       title_sites_raw = 2,
                       title_people_pctile = 3,
                       title_people_raw = 4)
  })
  expect_true("ggplot" %in% class(x))
  expect_equal(x$labels$title, 4)
})
