
test_that("regid_from_input gets regid", {
  expect_no_error({
    x = regid_from_input(regid = testinput_regid)
  })
  expect_equal(x, testinput_regid)
})

test_that("regid_from_input gets regid from sitepoints", {
  expect_no_error({
    x = regid_from_input(sitepoints = data.frame(regid = testinput_regid))
  })
  expect_equal(x, testinput_regid)
})



x = regid_from_input(sitepoints = data.frame(REGISTRY_ID = testinput_regid))
expect_equal(x, testinput_regid)


