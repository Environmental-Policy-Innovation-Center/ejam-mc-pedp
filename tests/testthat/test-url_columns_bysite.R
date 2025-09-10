
test_that("url_columns_bysite default ok", {

  expect_no_error({
    x = url_columns_bysite(
      sitepoints = testpoints_10[1:8,],
      regid = testinput_registry_id[1:8],
      as_html = T)
  })


  expect_no_error({
    x = url_columns_bysite(
      sitepoints = testpoints_10[1:8,],
      regid = testinput_registry_id[1:8],
      as_html = T,
      reports = list(
        list(header = "EJAM Report",     text = "EJAM Site Report",   FUN = url_ejamapi)   # EJAM summary report (HTML via API)
        , list(header = "EJSCREEN Map",  text =  "EJSCREEN", FUN = url_ejscreenmap)        # EJSCREEN site, zoomed to the location
      )
    )
  })
  expect_equal(NROW(x), 8)
  expect_true(!any(is.na(x[,1])))
  expect_true(all(is.character(x[,1])))
})

############# #


test_that("url_columns_bysite all url types", {

  # > str(EJAM:::global_or_param("default_reports"))
  # List of 2
  # $ :List of 3
  # ..$ header: chr "EJAM Report"
  # ..$ text  : chr "EJAM Site Report"
  # ..$ FUN   :function (sitepoints = NULL, lat = NULL, lon = NULL, radius = 3, fips = NULL, shapefile = NULL, linktext = "Report", as_html = FALSE, ifna = "https://ejanalysis.com",
  #                      baseurl = "https://ejamapi-84652557241.us-central1.run.app/report?", ...)
  #   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 54 15 221 1 15 1 54 221
  # .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ff3570d8278>
  #   $ :List of 3
  # ..$ header: chr "EJSCREEN Map"
  # ..$ text  : chr "EJSCREEN"
  # ..$ FUN   :function (sitepoints = NULL, lat = NULL, lon = NULL, shapefile = NULL, fips = NULL, wherestr = NULL, as_html = FALSE, linktext = "EJSCREEN", ifna = "https://pedp-ejscreen.azurewebsites.net/index.html",
  #                      baseurl = "https://pedp-ejscreen.azurewebsites.net/index.html", ...)
  #   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 261 20 356 1 20 1 261 356
  # .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7ff31d36a8f8>
  #   >

  allreports = list(

    list(header = "EJAM Report",     text = "EJAM Site Report",   FUN = url_ejamapi)   # EJAM summary report (HTML via API)

    , list(header = "EJSCREEN Map",  text =  "EJSCREEN", FUN = url_ejscreenmap)        # EJSCREEN site, zoomed to the location

    , list(header = "ECHO Report",          text = "ECHO",          FUN = url_echo_facility) # if regid provided # e.g., browseURL(url_echo_facility(110070874073))
    , list(header = "FRS Report",           text =  "FRS",          FUN = url_frs_facility)  # if regid provided # e.g., browseURL(url_frs_facility(testinput_registry_id[1]))

    , list(header = "Enviromapper Report",  text = "Enviromapper", FUN = url_enviromapper)   # if lat,lon provided or can be approximated # e.g., browseURL(url_enviromapper(lat = 38.895237, lon = -77.029145, zoom = 17))

    , list(header = "County Health Report", text = "County",       FUN = url_county_health)  # if fips provided
    , list(header = "State Health Report",  text = "State",        FUN = url_state_health)   # if fips provided

    , list(header = "County Equity Atlas Report", text = "County (Equity Atlas)", FUN = url_county_equityatlas)
    , list(header = "State Equity Atlas Report",  text = "State (Equity Atlas)", FUN = url_state_equityatlas)

  )



  expect_no_error({
    x = url_columns_bysite(sitepoints = testpoints_10[1:8,],
                           regid = testinput_registry_id[1:8],
                           as_html = T,
                           reports = allreports

    )
  })
  expect_equal(NROW(x), 8)
  expect_true(!any(is.na(x[,1])))
  expect_true(all(is.character(x[,1])))

})

