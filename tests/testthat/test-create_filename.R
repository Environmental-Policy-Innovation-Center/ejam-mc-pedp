test_that("ok", {
  arglist = list(
    filename_base = 'EJAM',
    buffer_dist = c(0, 3.2),
    site_method = c("", "latlon"),
    ext = c(NULL, ".html"),
    file_desc=c("", "FILE DESCRIPTION"),
    title = c("", "My Title")
  )
  func = EJAM:::create_filename
  expect_no_error(
    argument_combos(FUN = func, arglist, quiet = TRUE)
  )

x =  create_filename(file_desc = "FILE DESCRIPTION", buffer_dist = 3.2, site_method = "latlon" , ext = ".html")
without_timedate = gsub(" 20.*\\.html", ".html", x)
expect_equal(without_timedate,
             "EJAM FILE DESCRIPTION within 3.2 Miles for places by latlon.html"
)

})
