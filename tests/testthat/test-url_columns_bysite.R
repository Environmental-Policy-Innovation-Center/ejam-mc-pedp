
test_that("url_columns_bysite ok", {

  expect_no_error({
    x = url_columns_bysite(sitepoints = testpoints_10[1:8,],
                   regid = testinput_registry_id[1:8],
                   as_html = T)
  })
  expect_equal(NROW(x), 8)
  expect_true(!any(is.na(x[,1])))
  expect_true(all(is.character(x[,1])))

})

