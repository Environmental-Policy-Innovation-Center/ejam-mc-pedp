
#  test_ejam() had been called test_interactively() in prior versions of EJAM

#' run group(s) of unit tests for EJAM package
#' run tests of local source pkg EJAM, by group of functions, quietly, interactively or not, with compact summary of test results
#'
#' @details
#'  Note these require installing the package [testthat](https://testthat.r-lib.org) first:
#'
#'     [EJAM:::test_ejam()]         to test this local source pkg, by group of functions, quietly, summarized.
#'
#'     [devtools::test()]           is just a shortcut for [testthat::test_dir()], to run all tests in package.
#'
#'     [testthat::test_local()]     to test any local source pkg
#'
#'     [testthat::test_package()]   to test the installed version of a package
#'
#'     [testthat::test_check()]     to test the installed version of a package, in the way used by R CMD check or [utils::check()]
#'
#' @param ask logical, whether it should ask in RStudio what parameter values to use
#' @param noquestions logical, whether to avoid questions later on about where to save shapefiles
#' @param useloadall logical, TRUE means use [load_all()], FALSE means use [library()].
#'   But useloadall=T is essential actually, for unexported functions to be found when they are tested!
#' @param y_skipbasic logical, if FALSE, runs some basic [ejamit()] functions, but NOT any unit tests.
#' @param y_latlon logical, if y_skipbasic=F, whether to run the basic [ejamit()] using points
#' @param y_shp logical, if y_skipbasic=F, whether to run the basic [ejamit()] using shapefile
#' @param y_fips logical, if y_skipbasic=F, whether to run the basic [ejamit()] using FIPS
#' @param y_coverage_check logical, whether to show simple lists of
#'   which functions might not have unit tests, just based on matching source file and test file names.
#' @param y_runall logical, whether to run all tests instead of only some groups
#'   (so y_runsome is FALSE)
#' @param y_runsome logical, whether to run only some groups of tests (so y_runall is FALSE)
#' @param run_these if y_runsome = T, a vector of group names to test, like 'fips', 'naics', etc.
#'   see source code for list
#' @param skip_these if y_runall = T, a vector of group names to skip, like 'fips', 'naics', etc.
#' @param y_seeresults logical, whether to show results in console
#' @param y_save logical, whether to save files of results
#' @param y_tempdir logical, whether to save in tempdir
#' @param mydir optional folder
#' @examples
#' \dontrun{
#' biglist <- EJAM:::test_ejam()
#'
#' biglist <- EJAM:::test_ejam(ask=F, mydir = rstudioapi::selectDirectory())
# uses defaults, except it asks you what folder to save in

#' biglist <- EJAM:::test_ejam(ask = F,
#'       y_runsome = T, run_these = c('test', 'maps'),
#'       mydir = "~/../Downloads/unit testing") # for example
#'
#'   }
#'
#' @return a named list of objects like data.tables, e.g., named
#'   'bytest', 'byfile', 'bygroup', 'params', 'passcount' and other summary stats, etc.
#'
#' @keywords internal
#'
test_ejam <- function(ask = TRUE,
                      noquestions = TRUE, # just for shapefile folder selections
                      useloadall = TRUE, # essential actually, for unexported functions to be found when they are tested!

                      y_skipbasic = TRUE, y_latlon=TRUE, y_shp=TRUE, y_fips = TRUE,

                      y_coverage_check = FALSE,

                      y_runall  = TRUE,
                      y_runsome = FALSE, # if T, need to also create partial_testlist
                      run_these = NULL,  ## or...
                      # run_these = c("test_fips", "test_naics", "test_frs", "test_latlon", "test_maps",
                      #   "test_shape", "test_getblocks", "test_fixcolnames", "test_doag",
                      #   "test_ejamit", "test_misc", "test_ejscreenapi", "test_mod", "test_app",
                      #   "test_test", "test_golem"),
                      skip_these = c("ejscreenapi", "app"),

                      y_stopif = FALSE,
                      y_seeresults = TRUE,
                      y_save = TRUE,
                      y_tempdir = TRUE,
                      mydir = NULL
) {

  x <- offline_cat(); if (x) {stop("cannot use test_ejam() if offline")}

  if (ask) {
    # how to use test_ejam() ####
    cat('\n
################################### #  ################################### #
\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
\n  # examples of using this function: ####

# Examples of using it ####

?EJAM:::test_ejam

x <- EJAM:::test_ejam()   # it will ask about each parameter, by default

x <- EJAM:::test_ejam(ask=F, mydir = rstudioapi::selectDirectory())
# uses defaults, except it asks you what folder to save in

x <- EJAM:::test_ejam(F)  # no questions, just defaults, i.e. these:

x <- EJAM:::test_ejam(
  ask = TRUE,
  noquestions = TRUE, # just for shapefile folder selections

  useloadall  = TRUE, # might be essential actually

  y_skipbasic = TRUE,   y_latlon=TRUE, y_shp=TRUE, y_fips=TRUE,

  y_coverage_check = FALSE,

  y_runall     = TRUE,
  y_runsome    = FALSE, # if T, need to also create partial_testlist
  run_these = NULL,  # or some of these:
  # run_these = c("test_fips", "test_naics", "test_frs", "test_latlon", "test_maps",
  #   "test_shape", "test_getblocks", "test_fixcolnames", "test_doag",
  #   "test_ejamit", "test_misc", "test_ejscreenapi", "test_mod", "test_app",
  #   "test_test", "test_golem"),

  y_stopif     = FALSE, # stop as soon as problem is hit?
  y_seeresults = TRUE,
  y_save       = TRUE,
  y_tempdir    = TRUE,
  mydir = NULL
)

')
  }
  ########################################## # ########################################## #
  if (missing(y_skipbasic) & ask) {
    if (missing(y_skipbasic)) {
      y_skipbasic = askYesNo("Skip basic quick checks (which are not unit tests) ?", default = y_skipbasic)
    }}
  if (is.na(y_skipbasic)) {stop("canceled")}
  if (!y_skipbasic) {
    if (missing(y_latlon) & ask) {y_latlon = askYesNo("quick tests for latlon?", default = y_latlon)}
    if (is.na(y_latlon)) {stop("canceled")}
    if (missing(y_shp)    & ask) {y_shp    = askYesNo("quick tests for shp?",    default = y_shp)}
    if (is.na(y_shp))    {stop("canceled")}
    if (missing(y_fips)   & ask) {y_fips   = askYesNo("quick tests for fips?",   default = y_fips)}
    if (is.na(y_fips))   {stop("canceled")}
  }
  # if only doing basic non-unit-testing then do not ask about other details and do not find groups of test files, etc. -
  #  just skip way ahead to load/library and do those quick checks

  ########################################## # ########################################## #
  # . -------------------------------------------------- ####

  # Setup ####

  logfilename_only = paste0("testresults-",
                            gsub(" ", "_", gsub("\\.[0-9]{6}$", "", gsub(":", ".", as.character(Sys.time())))),
                            ".txt")
  if (y_skipbasic) {

    # consoleclear <- function() {if (interactive() & rstudioapi::isAvailable()) {rstudioapi::executeCommand("consoleClear")}}
    # consoleclear() is an undocumented internal function in the pkg now
    # !diagnostics off ## to disable diagnostics in this document
    #        thisfile = "./R/test_ejam.R"

    # require(data.table) # used in functions here

    # Note testthat package is in Suggests not Imports, in DESCRIPTION file
    try({suppressWarnings(suppressMessages({testthat_available <- require(testthat)}))}, silent = TRUE)
    if (!testthat_available) {stop("this requires installing the package testthat first, e.g., \n  install.packages('testthat')")}

    # Note beepr is in suggests not imports, in DESCRIPTION file
    # to make a sound when an error is hit and when it finishes - using beepr::beep(10) since utils::alarm() may not work.
    if (interactive()) {
      try({suppressWarnings(suppressMessages({beepr_available <- require(beepr)}))}, silent = TRUE)
      if (!beepr_available) {
        cat("install the beepr package if you want to have this function make a noise when it hits an error and when it is finished with all testing\n")
      }
    } else {
      beepr_available <- FALSE # it does not get used below when !intera
    }
    ########################################## #

    ## FIND test files ####

    sdir <- getwd()
    test_files_found <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
    ########################################## #

    # GROUP tests ####

    testlist = list(

      test_fips = c(
        "test-fips_bgs_in_fips.R",  # supports getblocksnearby_from_fips() that is tested in "test-getblocksnearby_from_fips.R"
        "test-FIPS_FUNCTIONS.R",
        "test-state_from_fips_bybg.R",
        "test-state_from_latlon.R",
        "test-is.numeric.text.R",
        "test-fips2countyfips.R",
        "test-fips_bg_from_latlon.R"
      ),
      test_naics = c(
        "test-naics_categories.R",
        "test-naics_findwebscrape.R",
        "test-naics_from_any.R",
        "test-naics_from_code.R",
        "test-naics_from_name.R",
        "test-naics_subcodes_from_code.R",
        "test-naics_validation.R",
        "test-naics2children.R"
      ),
      test_frs = c(
        "test-regid_from_input.R",
        "test-regid_from_naics.R",
        "test-frs_from_naics.R",
        "test-frs_from_programid.R",
        "test-frs_from_regid.R",
        "test-frs_from_sic.R",
        "test-frs_is_valid.R",
        "test-latlon_from_fips.R"
      ),
      test_latlon = c(
        "test-latlon_infer.R",
        "test-latlon_as.numeric.R",
        "test-latlon_df_clean.R",
        "test-latlon_is.valid.R",
        "test-latlon_from_anything.R",
        "test-latlon_from_sic.R",
        "test-address_xyz.R",
        "test-latlon_from_address.R",
        "test-latlon_from_vectorofcsvpairs.R",
        "test-state_from_sitetable.R"
      ),
      test_maps = c(
        "test-MAP_FUNCTIONS.R",
        "test-ejam2map.R"
      ),
      test_shape = c(
        "test-shapefile_xyz.R",
        "test-shapes_from_fips.R",
        "test-ejam2shapefile.R",
        "test-shape2zip.R",
        "test-shape2geojson.R"
      ),
      test_getblocks = c(
        "test-radius_inferred.R",              # this is SLOW THOUGH
        "test-getblocks_summarize_blocks_per_site.R",
        "test-getblocksnearby.R",
        "test-getblocksnearby_from_fips.R",
        "test-getblocksnearbyviaQuadTree.R",
        "test-report_residents_within_xyz.R",
        "test-proxistat.R",
        "test-utils_indexpoints.R",
        "test-get_blockpoints_in_shape.R"
      ),
      test_fixcolnames = c(
        "test-fixcolnames.R",
        "test-fixnames.R",
        "test-fixnames_to_type.R",
        "test-fixcolnames_infer.R",
        "test-varinfo.R",
        "test-utils_metadata_add.R"
      ),
      test_doag = c(
        "test-pctile_from_raw_lookup.R",
        "test-doaggregate.R",
        "test-area_sqmi.R",
        "test-batch.summarize.R",
        "test-utils_flagged_FUNCTIONS.R"
      ),
      test_ejamit = c(
        "test-ejamit.R",
        "test-ejamit_compare_distances.R",
        "test-ejamit_compare_types_of_places.R",
        "test-ejamit_sitetype_from_input.R",
        "test-ejamit_sitetype_from_output.R",

        "test-ejam2barplot_sites.R",
        "test-ejam2histogram.R"
      ),
      test_misc = c(
        "test-sites_from_input.R",
        "test-acs_bybg.R",
        "test-url_ejamapi.R",
        "test-URL_FUNCTIONS_part1.R",
        "test-URL_FUNCTIONS_part2.R",
        "test-url_columns_bysite.R",
        "test-is.numericish.R"
      ),
      ### skip ejscreenapi tests - do not work / get skipped WHILE EJSCREEN API IS DOWN MID 2025  ####
      test_ejscreenapi = c(
        "test-ejscreenapi.R",
        "test-ejscreenapi_plus.R",
        "test-ejscreenapi1.R",
        "test-ejscreenit.R",
        "test-ejscreenRESTbroker-functions.R"
      ),
      test_mod = c(
        "test-mod_save_report.R",
        "test-mod_specify_sites.R",
        "test-mod_view_results.R"
      ),
      test_app = c( # not to be confused with shinytest2::test_app() !
        #"test-report_residents_within_xyz.R",  # maybe belongs in a separate group about reports/tables?
        "test-ui_and_server.R",
        "test-FIPS-functionality.R",
        "test-latlon-functionality.R",
        "test-NAICS-functionality.R",
        "test-shp-gdb-zip-functionality.R",
        "test-shp-json-functionality.R",
        "test-shp-unzip-functionality.R",
        "test-shp-zip-functionality.R"
      ),
      test_test = c(
        # "test-test.R", #   fast way to check this script via  biglist <- EJAM:::test_ejam(ask = FALSE, y_runsome = T, run_these = 'test')
        "test-test2.R",  #   fast way to check this script
        "test-test1.R"
      ),
      test_golem = c(
        "test-golem_utils_server.R", # not used
        "test-golem_utils_ui.R"      # not used
      )
    )
    # c("test_fips", "test_naics", "test_frs", "test_latlon", "test_maps",
    #   "test_shape", "test_getblocks", "test_fixcolnames", "test_doag",
    #   "test_ejamit", "test_misc", "test_ejscreenapi", "test_mod", "test_app",
    #   "test_test", "test_golem")

    ########################################## #
    # groupnames <- names(testlist)
    test_all <- as.vector(unlist(testlist))
    ########################################## #
    ### check we grouped all tests ####
    # ensure the testlist includes all test files found
    {
      if (!all(TRUE == all.equal(sort(test_all), sort(test_files_found)))) {
        if (interactive() && beepr_available) {beepr::beep(10)}
        cat("\n\n ** Test files found in folder does not match test_files_found list ** \n\n")
      }

      if (length(setdiff(test_all, test_files_found)) > 0) {
        cat("These are in list of groups above but not in test folder as files: \n\n")
        print(setdiff(test_all, test_files_found))
        cat("\n")
      }

      if (length(setdiff(test_files_found, test_all)) > 0) {
        cat("These are in test folder as files but not in list of groups above: \n\n")
        print(setdiff(test_files_found, test_all))
        cat("\n")
        if (interactive() && ask) {
          # setdiff(test_files_found, test_all)
          stopfix <- askYesNo("Stop now to fix list of files in test_ejam() source code?", default = TRUE)
        } else {
          stopfix <- TRUE
        }
        if (is.na(stopfix) || stopfix == TRUE) { # if ESC or asked and yes
          cat("
You need to fix `testlist`, the list of files in the test_ejam() source code, to
ensure all existing `./test/test-xyz.R` files are listed in `testlist`
and all filenames listed there actually exist as in that folder called `test`.\n\n")
          stop("exiting to fix list of test files")
        } else {
          cat("Continuing anyway \n")
        }
      }

      if (length(setdiff(test_all, test_files_found)) > 0) {
        stop("fix list of test files")
      }

      if (any(duplicated(test_all))) {
        cat("some are listed >1 group\n")
        stop("some are listed >1 group")
      }

      cat("\n\n")
      ########################################## #
    }
    ########################### #  ########################################## #

    ########################### #  ########################################## #
    # cat("\n\nAVAILABLE UNIT TEST FILES, IN GROUPS:\n\n")

    ### count tests per group ####

    count_available_files_bygroup = data.frame(groupnames = names(testlist),
                                               shortgroupnames = gsub("^test_(.*)","\\1", names((testlist))),
                                               filecount = sapply(testlist, length)
                                               #, `filenames as test-___.R` = as.vector(unlist(lapply(testlist, function(z) paste0(gsub("^test-|.R$", "", unlist(z)), collapse = ", "))))
    )
    rownames(count_available_files_bygroup) = NULL
    # print(testlist) # long list of vectors

    cat("\n   COUNTS OF AVAILABLE FILES IN EACH GROUP OF TESTS\n\n")
    print(count_available_files_bygroup)
    cat("\n")
    { #          groupnames shortgroupnames filecount
      # 1         test_fips            fips         3 or 4?
      # 2        test_naics           naics         8
      # 3          test_frs             frs         6
      # 4       test_latlon          latlon        10
      # 5         test_maps            maps         1
      # 6        test_shape           shape         3
      # 7    test_getblocks       getblocks         5
      # 8  test_fixcolnames     fixcolnames         6
      # 9         test_doag            doag         2
      # 10      test_ejamit          ejamit         6
      # 11 test_ejscreenapi     ejscreenapi         5
      # 12         test_mod             mod         3
      # 13         test_app             app         5
      # 14        test_test            test         1
      # 15       test_golem           golem         2
      # fnames = unlist(testlist)
    }

    shortgroupnames = gsub("^test_(.*)","\\1", names((testlist)))
    ########################### #  ########################################## #

    # TIME the tests, predict ETA ####

    timebyfile <- structure(list(
      file = c("test-ejamit.R", "test-ejamit_compare_distances.R",
               "test-ejam2barplot_sites.R", "test-ejamit_compare_types_of_places.R",
               "test-ejamit_sitetype_from_input.R", "test-ejamit_sitetype_from_output.R",
               "test-getblocksnearbyviaQuadTree.R", "test-getblocksnearby.R",
               "test-proxistat.R", "test-get_blockpoints_in_shape.R", "test-getblocks_summarize_blocks_per_site.R",
               "test-getblocksnearby_from_fips.R", "test-radius_inferred.R",
               "test-report_residents_within_xyz.R", "test-utils_indexpoints.R",
               "test-MAP_FUNCTIONS.R", "test-doaggregate.R", "test-area_sqmi.R",
               "test-batch.summarize.R", "test-utils_flagged_FUNCTIONS.R", "test-pctile_from_raw_lookup.R",
               "test-shapes_from_fips.R", "test-ejam2shapefile.R", "test-shape2zip.R",
               "test-shapefile_xyz.R", "test-latlon_from_anything.R", "test-address_xyz.R",
               "test-latlon_as.numeric.R", "test-latlon_df_clean.R", "test-latlon_from_address.R",
               "test-latlon_from_sic.R", "test-latlon_from_vectorofcsvpairs.R",
               "test-latlon_infer.R", "test-latlon_is.valid.R", "test-state_from_sitetable.R",
               # add    "test-fips_bgs_in_fips.R"
               "test-FIPS_FUNCTIONS.R",
               "test-is.numeric.text.R", "test-state_from_fips_bybg.R",
               "test-state_from_latlon.R", "test-fixcolnames.R", "test-fixcolnames_infer.R",
               "test-fixnames.R", "test-fixnames_to_type.R", "test-utils_metadata_add.R",
               "test-varinfo.R", "test-frs_from_naics.R", "test-frs_from_programid.R",
               "test-frs_from_regid.R", "test-frs_from_sic.R", "test-frs_is_valid.R",
               "test-regid_from_naics.R", "test-golem_utils_server.R", "test-acs_bybg.R",
               "test-mod_save_report.R", "test-mod_specify_sites.R", "test-mod_view_results.R",
               "test-naics2children.R", "test-naics_categories.R", "test-naics_findwebscrape.R",
               "test-naics_from_any.R", "test-naics_from_code.R", "test-naics_from_name.R",
               "test-naics_subcodes_from_code.R", "test-naics_validation.R",
               "test-test1.R", "test-test2.R"),
      seconds_byfile = c(74.3, 58.3, 27.3, 11.7, 2, 10.3, 6.2, 10, 3, 4.8, 4.1, 7.2,
                         15.1, 2, 1.9, 40.5, 31.1, 6.5, 6.7, 6.4, 2.1, 6, 3.2, 2, 6, 2.6,
                         6.9, 1.9, 3.2, 5.5, 2.1, 2, 2.1, 2, 6.4,
                         # add    "test-fips_bgs_in_fips.R"
                         30.6, # subtract some for removed    "test-fips_bgs_in_fips.R"
                         2, 3.8, 10.5,
                         4.2, 2.9, 2, 2, 3.7, 2.1, 9, 2.2, 2.1, 2.2, 2.1, 5.8, 1.8, 4.6,
                         1.8, 1.8, 1.8, 2.2, 1.9, 3.8, 2.5, 1.9, 1.9, 1.9, 1.9, 1.7, 1.8
      )), class = "data.frame", row.names = c(NA, -66L
      ))

    #     # other names for tests that did not get run when dput used?
    timebyfile <- rbind(timebyfile,
                        data.frame(file =  c("test-latlon-functionality.R", "test-shp-gdb-zip-functionality.R",
                                             "test-shp-json-functionality.R", "test-shp-unzip-functionality.R",
                                             "test-shp-zip-functionality.R", "test-FIPS-functionality.R",
                                             "test-NAICS-functionality.R",
                                             "test-ui_and_server.R", "test-golem_utils_server.R",
                                             c("test-ejscreenRESTbroker-functions.R",
                                               "test-ejscreenapi.R", "test-ejscreenapi1.R", "test-ejscreenapi_plus.R",
                                               "test-ejscreenit.R")
                        ),
                        seconds_byfile = c(120, 157, 156, 160, 163,
                                           134, 115,
                                           2.7, 2.4,
                                           c(67, 7,
                                             7.8, 14 , 13)
                        )
                        ))
    ## test_ejscreenapi timing was removed

    {
      #                                          file seconds_byfile
      # 1                               test-ejamit.R         74.258
      # 2             test-ejamit_compare_distances.R         58.289
      # 3                   test-ejam2barplot_sites.R         27.297
      # 4       test-ejamit_compare_types_of_places.R         11.744
      # 5           test-ejamit_sitetype_from_input.R          1.986
      # 6          test-ejamit_sitetype_from_output.R         10.279
      # 7           test-getblocksnearbyviaQuadTree.R          6.232
      # 8                      test-getblocksnearby.R          9.954
      # 9                            test-proxistat.R          3.035
      # 10            test-get_blockpoints_in_shape.R          4.806
      # 11 test-getblocks_summarize_blocks_per_site.R          4.069
      # 12           test-getblocksnearby_from_fips.R          7.155
      # 13                     test-radius_inferred.R         15.127
      # 14         test-report_residents_within_xyz.R          1.956
      # 15                   test-utils_indexpoints.R          1.926
      # 16                       test-MAP_FUNCTIONS.R         40.495
      # 17                         test-doaggregate.R         31.119
      # 18                           test-area_sqmi.R          6.532
      # 19                     test-batch.summarize.R          6.711
      # 20             test-utils_flagged_FUNCTIONS.R          6.416
      # 21              test-pctile_from_raw_lookup.R          2.066
      # 22                    test-shapes_from_fips.R          5.964
      # 23                      test-ejam2shapefile.R          3.161
      # 24                           test-shape2zip.R          1.957
      # 25                       test-shapefile_xyz.R          6.001
      # 26                test-latlon_from_anything.R          2.570
      # 27                         test-address_xyz.R          6.859
      # 28                   test-latlon_as.numeric.R          1.903
      # 29                     test-latlon_df_clean.R          3.196
      # 30                 test-latlon_from_address.R          5.490
      # 31                     test-latlon_from_sic.R          2.076
      # 32        test-latlon_from_vectorofcsvpairs.R          2.003
      # 33                        test-latlon_infer.R          2.089
      # 34                     test-latlon_is.valid.R          1.953
      # 35                test-state_from_sitetable.R          6.378
      # 36                      test-FIPS_FUNCTIONS.R         30.649
      # 37                     test-is.numeric.text.R          2.017
      # 38                test-state_from_fips_bybg.R          3.771
      # 39                   test-state_from_latlon.R         10.485
      # 40                         test-fixcolnames.R          4.212
      # 41                   test-fixcolnames_infer.R          2.852
      # 42                            test-fixnames.R          2.014
      # 43                    test-fixnames_to_type.R          1.968
      # 44                  test-utils_metadata_add.R          3.673
      # 45                             test-varinfo.R          2.106
      # 46                      test-frs_from_naics.R          8.987
      # 47                  test-frs_from_programid.R          2.156
      # 48                      test-frs_from_regid.R          2.072
      # 49                        test-frs_from_sic.R          2.208
      # 50                        test-frs_is_valid.R          2.143
      # 51                    test-regid_from_naics.R          5.812
      # 52                  test-golem_utils_server.R          1.818
      # 53                            test-acs_bybg.R          4.570
      # 54                     test-mod_save_report.R          1.837
      # 55                   test-mod_specify_sites.R          1.802
      # 56                    test-mod_view_results.R          1.802
      # 57                      test-naics2children.R          2.239
      # 58                    test-naics_categories.R          1.893
      # 59                 test-naics_findwebscrape.R          3.778
      # 60                      test-naics_from_any.R          2.520
      # 61                     test-naics_from_code.R          1.894
      # 62                     test-naics_from_name.R          1.946
      # 63            test-naics_subcodes_from_code.R          1.910
      # 64                    test-naics_validation.R          1.888
      # 65                               test-test1.R          1.734
      # 66                               test-test2.R          1.789

      # 67                test-latlon-functionality.R        119.793
      # 68           test-shp-gdb-zip-functionality.R        157.021
      # 69              test-shp-json-functionality.R        156.421
      # 70             test-shp-unzip-functionality.R        160.492
      # 71               test-shp-zip-functionality.R        163.264
      # 72                  test-FIPS-functionality.R        133.808
      # 73                 test-NAICS-functionality.R        114.904
      ################# #
    }
    # timebygroup

    # dput(x$bygroup[, .(testgroup, seconds_bygroup)])

    timebygroup <- structure(list(
      testgroup = c("test_ejamit", "test_getblocks",
                    "test_maps", "test_doag", "test_shape", "test_latlon", "test_fips",
                    "test_fixcolnames", "test_frs", "test_golem", "test_misc", "test_mod",
                    "test_naics", "test_test"),
      seconds_bygroup = c(204, 81, 43, 67, 29, 66, 61, 34, 42, 9, 8, 14, 43, 9)),
      class = "data.frame", row.names = c(NA, -14L))
    # missing  "test_ejscreenapi" and "test_app"
    #sum(timebyfile[timebyfile$file %in% testlist[['test_app']], ]$seconds_byfile)
    # 1006
    timebygroup = rbind(timebygroup, cbind(testgroup = 'test_app', seconds_bygroup = 1006))
    timebygroup = rbind(timebygroup, cbind(testgroup = 'test_ejscreenapi', seconds_bygroup = 0))
    timebygroup$seconds_bygroup = as.numeric(timebygroup$seconds_bygroup)
    timebygroup$minutes_bygroup = round(as.numeric(timebygroup$seconds_bygroup) / 60, 1)
    data.table::setDT(timebygroup)
    # timebygroup
    #
    #           testgroup seconds_bygroup minutes_bygroup
    #              <char>           <num>           <num>
    # 1:      test_ejamit             204             3.4
    # 2:   test_getblocks              81             1.4
    # 3:        test_maps              43             0.7
    # 4:        test_doag              67             1.1
    # 5:       test_shape              29             0.5
    # 6:      test_latlon              66             1.1
    # 7:        test_fips              61             1.0
    # 8: test_fixcolnames              34             0.6
    # 9:         test_frs              42             0.7
    #10:       test_golem               9             0.1
    #11:        test_misc               8             0.1
    #12:         test_mod              14             0.2
    #13:       test_naics              43             0.7
    #14:        test_test               9             0.1
    #15:         test_app            1006            16.8
    #16: test_ejscreenapi               0             0.0

    ########################### #  ########################################## #

    ## check time est. avail. for each test ####
    # confirm we have the time estimate for each group and test

    if (y_runsome || y_runall) {
      timing_needed <- FALSE
      missingtime_tests <- setdiff(as.vector(unlist(testlist)), timebyfile$file)
      if (length(missingtime_tests) > 0) {
        cat("Missing time estimates for these test FILES:", paste0(missingtime_tests, collapse = ","), '\n')
      }
      missingtime_groups <- setdiff(names(testlist), timebygroup$testgroup)
      if (length(missingtime_groups) > 0) {
        cat("Missing time estimates for these GROUPS:", paste0(missingtime_groups, collapse = ","), '\n')
      }
      if (length(missingtime_tests) >0 || length(missingtime_groups > 0)) {
        timing_needed <- TRUE

        cat("Need to update the timing info on unit tests after running them again \n")
        }
      cat('\n')
    }
    ########################### #  ########################################## #

    # FUNCTIONS that will run tests by group ####
    ########################### #      ########################### #
    {
      ##     TO TEST 1 GROUP  (WITH SUCCINCT SUMMARY)

      ## examples
      # x1 = test1group(c("test-test1.R", "test-test2.R"), groupname = 'test', print4group = F   )
      # x2 = test1group(c("test-test1.R", "test-test2.R"), groupname = 'test', print4group = TRUE)
      # print(x1)
      # print(x2)

      test1group <- function(fnames = test_all, groupname = "",
                             reporter = "minimal", # some of the code below now only works if using this setting
                             load_helpers = TRUE,
                             print4eachfile = FALSE, # useless - keep it FALSE
                             print4group = TRUE,
                             add_seconds_bygroup = TRUE,
                             stop_on_failure = FALSE
      ) {

        xtable <- list()
        # tfile <- tempfile("junk", fileext = "txt")
        # timing = system.time({
        for (i in 1:length(fnames)) {
          seconds_byfile = system.time({
            cat(paste0("#", i, ' '))
            # cat(".") ## like a counter, one dot per file

            suppressWarnings(suppressMessages({
              junk <- testthat::capture_output_lines({
                x <- try(
                  testthat::test_file(
                    file.path("./tests/testthat/", fnames[i]),
                    load_helpers = load_helpers,
                    load_package = 'none',
                    # or else  Helper, setup, and teardown files located in the same directory as the test will also be run. See vignette("special-files") for details.
                    reporter = reporter,
                    stop_on_failure = stop_on_failure
                  )
                )
                if (inherits(x, "try-error")) {cat("Stopped on failure in ", fnames[i], "\n")}
              }
              , print = print4eachfile) # here it is a useless param of capture_output_lines()
            }))

            x <- as.data.frame(x)
            if (NROW(x) == 0) {
              # at one point it was having trouble around here
              # browser()
              cat("\n\n ********** FAILED TO GET ANY RESULTS TRYING TO RUN TESTS IN", file.path("./tests/testthat/", fnames[i]), '\n\n')
              xtable[[i]] <- NULL
              next
              }
            x$tests <- x$nb
            x$nb <- NULL
            x$flag <- x$tests - x$passed
            x$err  <- x$tests - x$passed - x$warning
            x$error_cant_test <- ifelse(x$error > 0, 1, 0)  ## a problem with counting this?
            x$error <- NULL
            x$skipped <- ifelse(x$skipped, 1, 0)

            x$err = NULL
            x$untested_skipped <- x$skipped; x$skipped = NULL
            x$untested_cant <- x$error_cant_test;  x$error_cant_test = NULL
            x$tested = x$tests - x$untested_skipped; x$tests = NULL
            x$total = x$untested_skipped + x$untested_cant + x$tested
            x$warned = x$warning; x$warning = NULL
            x$failed = x$tested - x$passed - x$warned
            x$flagged = x$untested_skipped + x$untested_cant + x$warned + x$failed; x$flag = NULL
            if (sum(x$total) != sum(x$passed + x$flagged)) {stop('math error in counts!')}

            x <- x[, c('file',  'test',
                       'total', 'passed', 'flagged',
                       'untested_cant', 'untested_skipped', 'warned', 'failed'
            )]

            # x <- x[, c('file',  'test',
            #            'tests', 'passed', 'failed',  'err',
            #            'warning', 'flag', 'skipped', 'error_cant_test'
            # )]

            x$test <- substr(x$test, 1, 50) # some are long
            xtable[[i]] <- data.table::data.table(x)
          })
          xtable[[i]]$seconds_byfile <- seconds_byfile['elapsed']
        }
        # })
        xtable <- data.table::rbindlist(xtable)

        seconds_bygroup <- round(sum(xtable[ , seconds_byfile[1], by = 'file'][,V1]), 0)
        ## can add this shorter time estimate to the results instead of relying on
        ## the slightly longer time estimate that can be done in testbygroup()
        if (add_seconds_bygroup) {
          xtable[ , seconds_bygroup := seconds_bygroup]
        }
        cat('done. ')
        cat(' Finished test group', groupname, 'in', seconds_bygroup, 'seconds.\n')
        if (print4group) {
          # print a table of counts
          print(c(
            colSums(xtable[, .(total, passed, flagged,
                               untested_cant, untested_skipped, warned, failed)]),
            seconds_bygroup = seconds_bygroup
          ))
        }

        return(xtable)
      }
      ########################### #      ########################### #

      ##     TO LOOP THROUGH GROUPS of tests

      ## examples
      #
      # y1 <- testbygroup( list(
      # test_test  = c("test-test1.R", "test-test2.R"),
      # test_golem = c("test-golem_utils_server.R", "test-golem_utils_ui.R")),
      # testing = TRUE
      # )
      # y2 <- testbygroup( list(
      #   test_test  = c("test-test1.R", "test-test2.R"),
      #   test_golem = c("test-golem_utils_server.R", "test-golem_utils_ui.R")),
      #   testing = FALSE,
      #   print4group = FALSE
      # )
      # y3 <- testbygroup( list(
      #   test_test  = c("test-test1.R", "test-test2.R"),
      #   test_golem = c("test-golem_utils_server.R", "test-golem_utils_ui.R")),
      #   testing = FALSE,
      #   print4group = TRUE # probably repeating printouts if  do this
      # )
      # print(y1)
      # print(y2)
      # print(y3)


      testbygroup <- function(testlist,
                              print4group = FALSE,
                              testing = FALSE,
                              stop_on_failure = FALSE,
                              reporter = "minimal" # this may be the only option that works now
      ) {
        # probably cannot now, but used to be able to use  reporter=default_compact_reporter()

        xtable <- list()

        i <- 0
        for (tgroupname in names(testlist)) {
          seconds_bygroup_viasystemtime = system.time({
            i <- i + 1
            if (i == 1) {load_helpers <- TRUE} else {load_helpers <- FALSE}
            fnames = unlist(testlist[[tgroupname]])
            cat("", tgroupname, "group has", length(fnames), "test files. Starting ")

            xtable[[i]] <- data.table::data.table(

              testgroup = tgroupname,

              test1group(testlist[[tgroupname]],
                         groupname = tgroupname,
                         load_helpers = load_helpers,
                         print4group = print4group,
                         stop_on_failure = stop_on_failure,
                         add_seconds_bygroup = TRUE, #   can be done here by testbygroup() not by test1group()
                         reporter = reporter)
            )
          })

          ## time elapsed
          ##
          ## This is the total time including overhead of looping, using test1group() for each group, and compiling.
          secs1 <- round(seconds_bygroup_viasystemtime['elapsed'], 0)
          if (testing) {
            cat('Seconds elapsed based on testbygroup() using system.time() is', secs1, '\n')
            # other ways fail if no test happened in a file like for group golem:
            ## This is a slightly shorter timing estimate could be done in test1group() by using add_seconds_bygroup=T
            secs2 <- round(xtable[[i]]$seconds_bygroup[1], 0)
            cat('Seconds elapsed based on testbygroup() reporting total reported by test1group() is', secs2, '\n')
            ## or, a similar estimate could be done here, but just like it would be in test1group() :
            secs3 <- round(sum(xtable[[i]][ , seconds_byfile[1], by = 'file'][,V1]), 0)
            cat('Seconds elapsed based on testbygroup() summing seconds_byfile once per file is', secs3, '\n')
          }
          secs <- secs1
          xtable[[i]]$seconds_bygroup <- secs # replaces any estimate done by test1group()

          # cat(paste0( '', round(secs, 0), ' seconds elapsed.\n'))
          ## That appears on same line where test1group() had already said "Finished test group xyz"
          ## or, previously, complete phrase here: # cat(paste0(' ', tgroupname, ' group finished, in ', round(secs, 0), ' seconds.\n\n'))

          ## Show table of counts for this group of files of tests:
          print(c(
            colSums(xtable[[i]][, .(total, passed, flagged,
                                    untested_cant, untested_skipped, warned, failed)]),
            seconds = secs

          ))

          if (sum(xtable[[i]]$flagged) > 0) {
            # using beepr::beep() since utils::alarm() may not work
            # using :: might create a dependency but prefer that pkg be only in Suggests in DESCRIPTION
            if (interactive() && beepr_available) {beepr::beep(10)}
            if (sum(xtable[[i]]$failed) > 0) {
              cat(paste0("     ***      Some FAILED in ", tgroupname, ": ",
                         paste0(unique(xtable[[i]]$file[xtable[[i]]$failed]), collapse = ","), "\n"))
            } else {
            cat(paste0("     ***      Some UNTESTED or WARNED in ", tgroupname, ": ",
                       paste0(unique(xtable[[i]]$file[xtable[[i]]$flagged]), collapse = ","), "\n"))
            }
          }

        } # looped over groups of test files

        xtable <- data.table::rbindlist(xtable)
        time_minutes <-   round(sum(xtable[ , (seconds_bygroup[1]) / 60, by = "testgroup"][, V1]) , 1)
        cat(paste0('\n', time_minutes[1], ' minutes total for all groups\n\n'))

        xtable[ , flagged_byfile := sum(flagged), by = "file"]
        xtable[ , failed_byfile  := sum(failed),  by = "file"]
        xtable[ , flagged_bygroup := sum(flagged), by = "testgroup"]
        xtable[ , failed_bygroup  := sum(failed),  by = "testgroup"]
        setorder(xtable, -failed_bygroup, -flagged_bygroup, testgroup, -failed, -flagged, file)
        setcolorder(xtable, neworder = c('seconds_bygroup', 'seconds_byfile'), after = NCOL(xtable))

        return(xtable)
      }
    }   #   done defining functions
    ########################### #  ########################################## #
# . ###
    # >> ASK WHAT TO DO << ####

    # *** THIS SECTION ASKS ABOUT run_these SO IT USES THE LATEST LIST OF TESTS FOUND to ask which ones to use, to know what the options are,
    # WHICH IS WHY THESE QUESTIONS ARE ASKED ONLY AFTER FINDING AND GROUPING TESTS

    if (y_runsome) {y_runall =  FALSE} # in case you want to say y_runsome = T and not have to also remember to specify y_runall = F

    if (interactive() & ask) {

      if (missing(y_coverage_check)) {
        y_coverage_check <- askYesNo(
          msg = "See lists of functions without matching unit test file names?",
          default = FALSE)
      }
      if (is.na(y_coverage_check)) {stop("canceled")}

      ## seems to not work if useloadall = FALSE
      # if (missing(useloadall)) {
      #   useloadall <- askYesNo(msg = "Do you want to load and test the current source code files version of EJAM (via devtools::load_all() etc.,
      #                 rather than testing the installed version)? MUST BE YES/TRUE OR UNEXPORTED FUNCTIONS CANT BE FOUND", default = TRUE)
      # }
      if (missing(y_runsome)) {
        if (!missing(run_these)) {y_runsome <- TRUE}
        if ( missing(run_these)) {y_runsome = askYesNo("Specify a subset of test groups to run?", default = FALSE)}
      }
      if (is.na(y_runsome))  {stop("canceled")}
      if (y_runsome) {y_runall =  FALSE}
      if (y_runsome) {
        if (missing(run_these)) {
          run_these = rstudioapi::showPrompt(
            "WHICH TEST GROUPS TO RUN? Enter a comma-separated list like  maps,frs  (or Esc to specify none)",
            paste0(shortgroupnames, collapse = ","),
            #e.g., "fips,naics,frs,latlon,maps,shape,getblocks,fixcolnames,doag,ejamit,ejscreenapi,mod,app"
          )
        }

        y_runall <- FALSE
      } else {
        y_runall <- TRUE
        # if (missing(y_runall)) {
        #   y_runall = askYesNo("RUN ALL TESTS NOW?")}
        # if (is.na(y_runall)) {stop("canceled")}
      }

      if (y_runall) {
        if (missing(skip_these)) {
          askskip = askYesNo("Specify some groups to skip?", default = FALSE)
          if (is.na(askskip)) {stop("canceled")}
          if (askskip) {
            skip_these = rstudioapi::showPrompt(
              "WHICH TEST GROUPS TO SKIP? Enter a comma-separated list like  maps,frs  (or Esc to specify none)",
              paste0(shortgroupnames, collapse = ","),
              default = ifelse(length(skip_these) > 0,
                               paste0(skip_these, collapse = ","),
                               "")
              # e.g., "fips,naics,frs,latlon,maps,shape,getblocks,fixcolnames,doag,ejamit,ejscreenapi,mod,app"
            )
            if (is.na(skip_these)) {stop("canceled")}
          }}
      }

      if (missing(y_stopif)) {
        y_stopif = askYesNo("Halt when a test fails?")}
      if (is.na(y_stopif)) {stop("canceled")}

      if (missing(y_seeresults)) {
        y_seeresults = askYesNo("View results of unit testing?")}
      if (is.na(y_seeresults))  {stop("canceled")}
      if (missing(y_save)) {
        y_save = askYesNo("Save results of unit testing (and log file of printed summaries)?")}
      if (is.na(y_save)) {stop("canceled")}
      if (y_save) {
        if (missing(y_tempdir) & missing(mydir)) {
          y_tempdir = askYesNo("OK to save in a temporary folder you can see later? (say No if you want to specify a folder)")}
        if (is.na(y_tempdir)) {stop("canceled")}
        if (y_tempdir & missing(mydir)) {
          mydir <- tempdir()
        } else {
          if (missing(mydir)) {
            mydir <- rstudioapi::selectDirectory()}
        }
      }
    }

    if (!missing(run_these)) {y_runsome <- TRUE} # you specified some tests to run, so assume you meant to ignore the default y_runsome
    if (any(skip_these %in% run_these)) {cat("Note you are skipping some tests that you also asked to run:\n ", paste0(intersect(skip_these, run_these), collapse = ", "), "\n")}
    if (y_runsome) {y_runall =  FALSE}
    if (y_runsome) {
      run_these <- unlist(strsplit(gsub(" ", "", run_these), ","))
      run_these = paste0("test_", run_these)
      #    test_file("./tests/testthat/test-MAP_FUNCTIONS.R" )
      partial_testlist <-  testlist[names(testlist) %in% run_these]
    }

    if (y_runall) {
      skip_these <- unlist(strsplit(gsub(" ", "", skip_these), ","))
      skip_these = paste0("test_", skip_these)
      partial_testlist <-  testlist
      if (length(skip_these) > 0 && !is.null(skip_these)) {
        partial_testlist <-  testlist[!(names(testlist) %in% skip_these)]
      }}
    ################################### #  ################################### #
    if (y_runall == FALSE && y_runsome == FALSE) {
      stop('no tests run')
    } else {
      noquestions <- TRUE
      # if (interactive() & ask & (y_runall | ("test_shape" %in% names(testlist)))) {
      #   # ***  note if interactive it normally tries to prompt for shapefile folder in some cases
      #   if (missing(noquestions)) {
      #     if (askYesNo("run tests where you have to interactively specify folders for shapefiles?")) {
      #       noquestions <- FALSE
      #     }  else {
      #       noquestions <- TRUE
      #     }
      #   } else {
      #     # noquestions  was given as a parameter
      #   }}
    }
  } # end if not just basic
  # finished asking what to do and setting up

  # if  still have not defined valid mydir
  if (missing(mydir) || (!exists('mydir') || is.null(mydir)) || !dir.exists(mydir) ) {
    if (y_tempdir) {
      mydir <- tempdir()
    } else {
      mydir = '.'
    }
  }
  mydir <- normalizePath(mydir)
  logfilename = (  file.path(mydir, logfilename_only) )

  cat("Saving in ", logfilename, ' etc. \n')
  ########################### #  ########################################## #
  # ~ ## ##
  # . -------------------------------------------------- ####

  # Start  ####

  ########################### #  ########################################## #

  ## test_coverage_check() ####

  if (y_coverage_check) {
    cat("Also see the covr package at https://covr.r-lib.org/ \n")
    source("tests/test_coverage_check.R")
    test_coverage_info <- test_coverage_check()
    # test_coverage_info table is not used. the function prints info.
  }
  ########################### #  ########################################## #
  ## load_all() or library(EJAM) ####
  cat('\n')
  if (useloadall) {

    # Note devtools package is in Suggests not Imports, in DESCRIPTION file
    try({suppressWarnings(suppressMessages({devtools_available <- require(devtools)}))}, silent = TRUE)
    if (!devtools_available) {stop("this requires installing the package devtools first, e.g., \n  install.packages('devtools') \n")}
    junk <- capture.output({
      suppressPackageStartupMessages(    devtools::load_all()   )
    })
  } else {
    cat("useloadall=F WILL FAIL TO FIND THE UNEXPORTED FUNCTIONS WHEN IT TRIES TO TEST THEM !! \n")
    junk <- capture.output({
      suppressPackageStartupMessages({   library(EJAM)   })
    })
  }
  cat("Downloading all large datasets that might be needed...\n")
  dataload_dynamic("all")
  ## should happen later in the function test1group() via testbygroup
  # if (file.exists("./tests/testthat/setup.R")) {
  #   # rstudioapi::navigateToFile("./tests/testthat/setup.R")
  #   source("./tests/testthat/setup.R") #   asks if need load_all or library
  # } else {
  #   cat("Need to source the setup.R file first \n")
  # }
  ########################### #  ########################################## #

  ## DO BASIC QUICK CHECKS, NOT UNIT TESTS   ####
  # for easy/basic case, main functions, without actually running unit tests with testthat

  if (!y_skipbasic) {

    if (y_latlon) {
      # latlon
      x <- ejamit(testpoints_5[1:2,], radius = 1)
      # names(x)
      ejam2table_tall(x)
      ejam2barplot(x)
      ejam2barplot_sites(x)
      ejam2tableviewer(x)
      junk = ejam2excel(x, save_now = F, launchexcel = T)

      ejam2report(x, analysis_title = "2 point latlon example")
      ejam2report(x, analysis_title = "2 point latlon example but selecting 1 site", sitenumber = 2)

      ejam2map(x) # no sitenumber param available
      # convert to shapefile of circles at points
      fname = ejam2shapefile(x, folder = tempdir())
      shpin = shapefile_from_any(fname)
      ejam2map(x, shp = shpin) # if shp is provided
      map_shapes_leaflet(shpin) # does not use nice EJAM popups
      cat("\n\n DONE WITH latlon CHECKS \n\n")
      x1 = x
    }

    if (y_shp) {
      # shapefile

      shp <- shape_buffered_from_shapefile( shapefile_from_sitepoints(testpoints_5[1:2,]), radius.miles = 1)
      # or use test data  shp <- shapefile_from_any()
      shp <- shapefile_from_any(
        system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM")
      )[1:3, ]
      x3 <- ejamit( shapefile = shp, radius = 0 )
      names(x3)
      ejam2table_tall(x3)
      ejam2barplot(x3)
      ejam2barplot_sites(x3)
      ejam2tableviewer(x3 , fname = file.path(tempdir(), "ejam2tableviewer_3polygon_test.html")) # should be able to pick name
      junk = ejam2excel(x3, save_now = F, launchexcel = T)

      ejam2report(x3, analysis_title = "3 polygon portland example", shp = shp)
      ejam2report(x3, analysis_title = "3 polygon portland example, 1 site", shp = shp, sitenumber = 2)

      ejam2map(x3) # no latlon or geometry is in output of ejamit() here so just shows a point at each poly!!
      ejam2map(x3, shp = shp)  # if shp is provided, map works!

      # map_ejam_plus_shp(out = x3, shp = shp) # also works
      # tfile = ejam2shapefile(x3, folder = tempdir()) # no latlon or geometry is in output of ejamit() here
      shp3 = ejam2shapefile(x3, save = FALSE, shp = shp) # this also merges them but there are better ways above
      map_shapes_leaflet(shp3) # ugly popup but works
      cat("\n\n DONE WITH shp CHECKS \n\n")
      x1 = x3
    }

    if (y_fips) {
      # fips
      fipstest = fips_bgs_in_fips(fips_counties_from_state_abbrev("DE")[1])[1:2]
      x2 <- ejamit(fips = fipstest) # just 2 blockgroups
      names(x2)
      ejam2table_tall(x2)
      ejam2barplot(x2)
      ejam2barplot_sites(x2)
      ejam2tableviewer(x2)
      junk = ejam2excel(x2, save_now = F, launchexcel = T)

      ejam2report(x2)
      ejam2report(x2, sitenumber = 2)

      ejam2map(x2) # no latlon or geometry is in output of ejamit() but this does work!
      # ejam2map(x2, shp = shapes_from_fips(fipstest)) # not needed and replaces fips with id 1:N

      ejam2shapefile(x2, folder = tempdir()) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      x3b <- ejamit(fips = fips_counties_from_state_abbrev("DE"))  #   3 Counties
      mapfastej_counties(x3b$results_bysite) # not (x)
      cat("\n\n DONE WITH fips CHECKS \n\n")
      x1 = x3b
    }

    cat("Done with basic checks. Not doing any other testing. \n\n")
    invisible(x1)
  } # halts if this gets done - just basic checks get done if !y_skipbasic
  ########################### #  ########################################## #
  ########################### #  ########################################## #


  ########################### #  ########################################## #

  # try to do this once here and not in setup.R
  ## out_api (obsolete) ####
  if (exists("out_api" , envir = globalenv() )) {
    cat("Using the copy of out_api that already is in globalenv() so if that is outdated you should halt and do rm(out_api) now\n")
  } else {
    eee = ejscreenapi_online()
    if (is.na(eee) || !eee) {
      cat("offline or ejscreen API URL does not seem to be accessible according to EJAM:::ejscreenapi_online() \n\n")
    } else {
      cat("Creating out_api in the globalenv(), using ejscreenapi()\n\n")
      test2lat <- c(33.943883,    39.297209)
      test2lon <- c(-118.241073, -76.641674)
      pts <- data.frame(lat = test2lat, lon = test2lon)
      testradius = 1
      out_api       <- ejscreenapi(lon = test2lon, lat = test2lat, radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE)
      assign(x = "out_api", out_api, envir = globalenv())
    }
  }
  ########################### #  ########################################## #

  ## log file started ####

  # cat("\n\nStarted testing at", as.character(Sys.time()), '\n')

  junk = loggable(file = logfilename, x = {
    cat(logfilename_only, '\n ---------------------------------------------------------------- \n\n')
    cat("Started at", as.character(Sys.time()), '\n')

    if (is.null(run_these)) {run_theseprint = NA} else {
      run_theseprint = paste0(run_these, collapse = ',')
    }

    ## summary of input parameters ####
    # get current values
    paramslist <- list()
    for (i in 1:length(formalArgs(test_ejam))) {
      paramslist[[i]] <- get(formalArgs(test_ejam)[i])
    }
    names(paramslist) <- formalArgs(test_ejam)
    paramslist$run_these <- paste0(paramslist$run_these, collapse = ",") # easier to view
    params <- paramslist
    ## same as spelling them out:
    # params = list(ask =  ask,
    #               noquestions  =  noquestions,
    #               useloadall   =  useloadall,
    #               y_skipbasic      =  y_skipbasic,
    #               y_latlon     =  y_latlon,
    #               y_shp        =  y_shp,
    #               y_fips       =  y_fips,
    #               y_runsome    =  y_runsome,
    #               run_these        =  paste0(run_these, collapse = ","),
    #   skip_these = .....
    #               y_runall     =  y_runall,
    #               y_seeresults =  y_seeresults,
    #               y_save       =  y_save,
    #               y_tempdir    =  y_tempdir,
    #               mydir        =  mydir
    # )
    paramsdefaults <- formals(test_ejam)
    params_summary = data.frame(
      default = cbind(paramsdefaults),
      current = cbind(params)
    )
    colnames(params_summary) <- c('default', 'current')
    cat("\nParameters (options) being used: \n")
    print(params_summary)
    cat("\n")

    # cat("\nParameters (options) being used:
    #
    #     ask          = ", ask, "
    #     noquestions  = ", noquestions, "
    #     useloadall   = ", useloadall, "
    #
    #     y_skipbasic      = ", y_skipbasic, "
    #       y_latlon     = ", y_latlon, "
    #       y_shp        = ", y_shp, "
    #       y_fips       = ", y_fips, "
    #
    #     y_runsome    = ", y_runsome, "
    #       run_these        = ", run_theseprint, "
    ##   skip_these = .....
    #     y_runall     = ", y_runall, "
    #
    #     y_seeresults = ", y_seeresults, "
    #     y_save       = ", y_save, "
    #     mydir        = ", "[not shown here]" , "
    #     "
    # )
  })
  ########################### #  ########################################## #
  ########################### #  ########################################## #

  # RUN 1 TEST FILE OR GROUP ####

  if (y_runsome) {

    if (y_runsome) {y_runall =  FALSE}
    shownlist = partial_testlist
    shownlist = cbind(testgroup = rep(names(shownlist), sapply(shownlist, length)), file = unlist(shownlist))
    rownames(shownlist) = NULL

    cat("\n USING THESE TEST FILES: \n\n")

    print(shownlist); cat('\n\n')

    secs1 = sum(timebygroup$seconds_bygroup[timebygroup$testgroup %in% shownlist[, 'testgroup']])
    mins1 = round(secs1 / 60, 1)
    cat("Predicted time to run tests is roughly", mins1, "minutes. Very rough estimate of ETA: ")

    print(Sys.time() + secs1)
    cat("\n\n")
    #
    # fnames = as.vector(unlist(shownlist))
    # secs2 = 1.3 * sum(timebyfile$seconds_byfile[timebyfile$file %in% fnames])
    # mins2 = round(secs2 / 60, 1)
    # cat("Predicted time to run tests is roughly", mins2, "minutes. Very rough estimate of ETA: ")
    # print(Sys.time() + secs2)
    # cat("\n\n")

    x <- testbygroup(testlist = partial_testlist, stop_on_failure = y_stopif)
    bytest <- x

    junk = loggable(file = logfilename, x = {

      cat("-------------------------------------------------- \n")
      cat("\n")
      cat("           TEST RESULTS AS OF "); cat(as.character(Sys.Date()), '\n')

      # cat("\n                            RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")

      if (any(x$flagged  > 0)) {
        # print(as.data.frame(x)[x$flagged  > 0, !grepl("byfile|bygroup", names(x))])
      } else {
        cat("All selected tests ran and passed.")
      }
      cat("\n")
    })
    ########################### #
    ## save results of some testing ####
    if (y_seeresults) {
      # will do save of everything after summarizing results
    } else {
      if (y_save) {
        fname <- paste0("results_of_some_unit_testing_", as.character(Sys.Date()), ".rda")
        fname = (  file.path(mydir, fname) )
        save(bytest, file = fname)
        junk = loggable(file = logfilename, x = {

          cat('\n  See', fname, ' for results of some unit testing.\n\n')
        })
      } # end if - save
    }
  }
  ########################### #  ########################################## #
  ########################### #  ########################################## #

  # RUN ALL TESTS (slow)  ####

  if (y_runall) {

    z <- system.time({

      shownlist = partial_testlist # testlist is universe but what is tested now may be limited by skip_these param

      shownlist = cbind(testgroup = rep(names(shownlist), sapply(shownlist, length)), file = unlist(shownlist))
      rownames(shownlist) = NULL
      cat("\n USING THESE TEST FILES: \n\n")
      print(shownlist); cat('\n\n')

      secs1 = sum(timebygroup$seconds_bygroup[timebygroup$testgroup %in% shownlist[, 'testgroup']])
      mins1 = round(secs1 / 60, 1)
      cat("Predicted time to run tests is roughly", mins1, "minutes. Very rough estimate of ETA: ")
      print(Sys.time() + secs1)
      cat("\n\n")
      #
      # fnames = as.vector(unlist(shownlist))
      # secs2 = 1.3 * sum(timebyfile$seconds_byfile[timebyfile$file %in% fnames])
      # mins2 = round(secs2 / 60, 1)
      # cat("Predicted time to run tests is roughly", mins2, "minutes. Very rough estimate of ETA: ")
      # print(Sys.time() + secs2)
      # cat("\n\n")

      rm(shownlist)

      x <- testbygroup(testlist = partial_testlist, stop_on_failure = y_stopif)
      bytest <- x

    })
    junk = loggable(file = logfilename, x = {

      cat("-------------------------------------------------- \n")
      cat("\n")
      cat("           TEST RESULTS AS OF "); cat(as.character(Sys.Date()), '\n')

      # cat("\n                            RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")

      if (any(x$flagged > 0)) {
        # print(as.data.frame(x)[x$flagged > 0, !grepl("byfile|bygroup", names(x))])
      } else {
        cat("All selected tests ran and passed.\n")
      }
      cat("\n")
    })
    ########################### #
    ## save results of all testing ####
    if (y_seeresults) {
      # will do save of everything after summarizing results
    } else {
      # y_save = askYesNo("Save results of unit testing?")
      if (is.na(y_save)) {stop("canceled")}
      if (y_save) {
        fname <- paste0("results_of_unit_testing_", as.character(Sys.Date()), ".rda")
        fname = (  file.path(mydir, fname) )
        save(bytest, file = fname)
        junk = loggable(file = logfilename, x = {
          cat('\n  See', fname, ' for full results of unit testing.\n\n')
        })
      } # end if - save
    }
  }
  ########################### #  ########################################## #
  ########################### #  ########################################## #

  # SUMMARIZE results ####

  # y_seeresults = askYesNo("View results of unit testing?")
  if (is.na(y_seeresults))  {stop("canceled")}
  if (y_seeresults) {
    # consoleclear()
    ########################### #  ########################### #
    junk <- loggable(file = logfilename, x = {

      # HOW MANY TOTAL PASS/FAIL?

      cat("\n")

      cat("COUNT PASS / FAIL \n\n")

      passcount = colSums(x[, .(total, passed, flagged,   untested_cant, untested_skipped, warned, failed)])
      print(passcount)
      cat("\n")

      cat("PERCENT PASS / FAIL ")

      cat("\n\n")
      passpercent = round(100 * colSums(x[, .( total, passed, flagged,   untested_cant, untested_skipped, warned, failed )])
                          / sum(x$total), 1)
      print(passpercent)
      ########################### #  ########################### #

      ## KEY GROUPS - WHICH TEST GROUPS or FILES HAVE THE MOST FAILING TESTS?

      bygroup <- x[ , .(total = sum(total), passed = sum(passed), flagged = sum(flagged),
                        untested_cant = sum(untested_cant), untested_skipped = sum(untested_skipped), warned = sum(warned), failed = sum(failed),
                        seconds_bygroup = seconds_bygroup[1]),
                    by = "testgroup"]
      cat("\n")

      cat("GROUPS OF FILES")

      cat("\n\n")
      print(bygroup)  # show even if no issues arose
      ########################### #  ########################### #

      ## WHICH FILES HAVE THE MOST FAILING TESTS?

      byfile <- x[ , .(
        flagged_byfile = flagged_byfile[1],    #    total, passed, flagged,   untested_cant, untested_skipped, warned, failed
        flagged_bygroup = flagged_bygroup[1],
        failed_byfile = failed_byfile[1],
        failed_bygroup = failed_bygroup[1],
        testgroup = testgroup[1]
      ),
      by = "file"]
      setorder(byfile, -failed_bygroup, -flagged_bygroup, testgroup, failed_byfile, -flagged_byfile, file)
      setcolorder(byfile, neworder = c("testgroup", "failed_bygroup", "flagged_bygroup", "file", "failed_byfile", "flagged_byfile"))
      byfile_key <- byfile[flagged_byfile > 0, ]
      cat("\n\n")
      if (NROW(byfile_key) == 0) {
        cat("No files had any tests with issues\n\n")
      } else {

        cat("KEY FILES")

        cat("\n\n")
        keyfilesprint = as.data.frame(byfile_key)[ , !grepl("_bygroup", names(byfile_key))]
        keyfilesprint = keyfilesprint[order(keyfilesprint$flagged_byfile, decreasing = TRUE), ]
        print(keyfilesprint)
      }
      ########################### #

      # WHICH TESTS?

      bytest_key = x[order(-x$failed, -x$warned, -x$flagged), ]
      these = bytest_key$flagged > 0
      if (any(these)) {
        bytest_key <- bytest_key[these, ]
        cat("\n\n")

        cat("KEY TESTS")

        cat("\n\n")
        bytest_key_niceview <- as.data.frame(bytest_key)[ , !grepl("_byfile|_bygroup|total|passed|flagged", names(bytest_key))]
        bytest_key_niceview <- bytest_key_niceview[, c('testgroup', 'file', 'test', 'failed', 'warned', 'untested_cant', 'untested_skipped')]
        print(bytest_key_niceview)
        cat("\n")
      } else {
        cat("\n")
        cat("No tests had issues\n\n")
        bytest_key = NA
        bytest_key_niceview = NA
      }
      ########################### #

      # show how to open key files

      if (NROW(byfile_key) != 0) {
        topfilenames <- as.data.frame(byfile_key)
        topfilenames = topfilenames[order(topfilenames$failed_byfile, topfilenames$flagged_byfile, decreasing = TRUE), ]
        topfilenames = topfilenames$file[topfilenames$flagged_byfile > 0]
        if (length(topfilenames) > 0) {
          topfilenames <- topfilenames[1:min(5, length(topfilenames))]

          cat("\nTO OPEN SOME KEY TEST FILES FOR EDITING, FOR EXAMPLE:\n" ,

              paste0("rstudioapi::navigateToFile('./tests/testthat/", topfilenames, "')", collapse = "\n "),
              "\n\n")
          # rstudioapi::navigateToFile("./tests/testthat/test-doaggregate.R")
          # rstudioapi::navigateToFile("./tests/testthat/test-ejamit.R")
          # rstudioapi::navigateToFile("./tests/testthat/test-latlon_df_clean.R")
        }
      }

    }) # end loggable
  } # end of big if - viewing results
  ########################### #  ########################################## #

  # COMPILE ALL RESULTS IN A LIST

  if (!exists("bytest")) {bytest <- NA}

  totalseconds = sum(x[ , seconds_bygroup[1], by = "testgroup"][,V1])
  totalminutes = round(totalseconds / 60, 1)

  biglist <- list(
    minutes = totalminutes,
    passcount = passcount,    #      total, passed, flagged,   untested_cant, untested_skipped, warned, failed
    passpercent = passpercent,
    bygroup = bygroup,
    byfile = byfile,
    bytest_key = bytest_key,
    bytest_key_niceview = bytest_key_niceview,
    bytest_all = bytest,
    folder = mydir,
    count_available_files_bygroup = count_available_files_bygroup,
    params = params
  )
  # SAVE results  ####
  if (y_save) {
    fname <- paste0("results_SUMMARY_of_unit_testing_", as.character(Sys.Date()), ".rda")
    fname = (file.path(mydir, fname))
    save(biglist, file = fname)

    cat(paste0('\nSaved results here: \n  "', fname, '" \n\n'))
  }
  loggable(file = logfilename, x = {
    cat(paste0(
      totalminutes, ' minutes elapsed running these tests',
      ' (finished at ', as.character(Sys.time()), ')\n'))
  })

  if (interactive()) {
    browseURL(mydir) # open folder in file explorer / finder
    if (rstudioapi::isAvailable()) {
      # view the file
      rstudioapi::navigateToFile(logfilename)
    }
  }
  if (interactive() && beepr_available) {beepr::beep()} # utils::alarm() may not work

  if (timing_needed) {
    cat( "
        ------------------------------------------------ \n
      Need to update the timing info on unit tests.
      Copy text output of dput (as done below) into source code of this file test_ejam.R

        x = test_ejam(ask=F, skip_these = '') # instead of default that was skipping app functionality tests that may have trouble working # skip_these = c('ejscreenapi', 'app')
        dput(data.frame(unique(x$bytest_all[, .(file, seconds_byfile)])))

             ------------------------------------------------ \n")
  }

  invisible(
    biglist
  )
} # end of function
################################### #  ################################### #  ################################### #


# ~ ####
# This is just an unexported helper function that tried to save a log like text in console, to a file

loggable <- function(x, file = 'will be created using timestamp if not provided and !exists(logfilename)',
                     append = TRUE, split = TRUE,
                     y_save_param=NULL) {

  if (missing(y_save_param)) {
    if (!exists('y_save')) {
      if (is.null(file)) {
        y_save <- FALSE
      } else {
        y_save <- TRUE
      }
    }
  } else {
    y_save <- y_save_param
  }

  if (y_save) {
    if (missing(file)) {
      if (exists('logfilename')) {
        file = logfilename
      } else {
        mydir = tempdir()
        file = paste0("testresults-",
                      gsub(" ", "_", gsub("\\.[0-9]{6}$", "", gsub(":", ".", as.character(Sys.time())))),
                      ".txt")
        file = (  file.path(mydir, file) )
      }
    }
    if (is.null(file)) {
      warning("file got set to NULL so NOT saving even though y_save was TRUE.")
    }
  } else {
    if (missing(file)) {
      file = NULL
    } else {
      if (!is.null(file)) {
        warning('file got specified so WILL save even though y_save was FALSE.')
      }
    }
  }

  capture.output(x, file = file, append = append, split = split) # this is supposed to print to console and to log file, but...

  # cat('\n  Adding to ', file, ' log of results of unit testing.\n\n')

  # use file = logfilename  or file = NULL  to override whatever the y_save value was when func was defined
  # file = NULL  will show only in console and not log it
  # split=T  will show output in console, and save to file simultaneously unless file=NULL

  ### how to use it    ## example
  # ## y_save = F will prevent logging unless you also specify a file
  # junk = loggable(file = logfilename, x = {
  #   })

  # junk = loggable(file = logfilename, x = {
  #   # comments do not get logged
  #   #  x  or  1 + 1  is not logged without print() or cat() ?
  #   print(cbind(a=1:3,b=2:4))
  #   cbind(c = 1:3, d = 2:4)
  #   x = 56
  #   print(x)
  #   cat(1234,'\n\n')
  #
  #   })
  ## use file = logfilename  or file = NULL  to override whatever the y_save value is

}
################################### #

