# ejam2excel()
## uses  table_xls_from_ejam()
##   which uses   table_xls_format

# EJAM:::args2(ejam2excel)
#
# ejam2excel(
#   ejamitout,
#   fname = NULL,
#   save_now = TRUE,
#   overwrite = TRUE,
#   launchexcel = FALSE,
#   interactive_console = TRUE,
#   ok2plot = TRUE,
#   in.testing = FALSE,
#   in.analysis_title = "EJAM analysis",
#   react.v1_summary_plot = NULL,
#   radius_or_buffer_in_miles = NULL,
#   buffer_desc = NULL,
#   radius_or_buffer_description = "Miles radius of circular buffer (or distance used if buffering around polygons)",
#   reports = EJAM:::global_or_param("default_reports"),
#   site_method = "",
#   mapadd = FALSE,
#   report_map = NULL,
#   community_reportadd = TRUE,
#   community_html = NULL,
#   shp = NULL,
#   ...  # 	optional additional parameters passed to table_xls_format(), such as heatmap_colnames, heatmap_cuts, heatmap_colors, etc.
# )

test_that("ejam2excel saves key tables, tabs, saved numbers match original", {

  expect_no_error({
    junk <- capture.output({
      tfile = tempfile(fileext = ".xlsx")
    suppressWarnings({
      fname <- ejam2excel(testoutput_ejamit_10pts_1miles, interactive_console = FALSE,
                          in.analysis_title = "test title", fname = tfile
                          )
    })
    })
  })

  tabs <- readxl::excel_sheets(fname)
  expect_equal(tabs,
               c("Each Site",
                 "Overall",
                 "Overall 2",
                 "Community Report",
                 "plot_ratios",
                 "notes",
                 "thresholds")
  )

  tab_bysite <- readxl::read_excel(fname, sheet = "Each Site") %>% as.data.frame()
  expect_equal(NROW(tab_bysite),
               NROW(testoutput_ejamit_10pts_1miles$results_bysite))

  tab_overall <- readxl::read_excel(fname, sheet = "Overall") %>% as.data.frame()
  expect_equal(NROW(tab_overall),
               1)

  tab_thresholds <- readxl::read_excel(fname, sheet = "thresholds") %>% as.data.frame()
  expect_equal(NROW(tab_thresholds),
               NROW(tab_bysite))

  expect_equal(names(tab_bysite), names(tab_overall))
  tab_rnames= fixcolnames(names(tab_overall), 'long', 'r')
  original_rnames = names(testoutput_ejamit_10pts_1miles$results_overall)
  common_names_r = intersect(original_rnames, tab_rnames)
  common_names_long = fixcolnames(common_names_r, 'r', 'long')

  # z = data.frame(common_names_r,common_names_long)
  #  z$in_orig = z$common_names_r %in% names(testoutput_ejamit_10pts_1miles$results_bysite)
  #  z$in_xls = z$common_names_long %in% names(tab_bysite)
  #  z$in_both = z$in_orig & z$in_xls
  #  table(z$in_both)
  #
  # TRUE
  # 429

  ############## #
  # overall  data in excel matches original data, except some columns that are logical vs integer, NA vs ""

  testin = as.data.frame(testoutput_ejamit_10pts_1miles$results_overall)[, common_names_r]
  testout = tab_overall[, common_names_long]
  names(testout) <- common_names_r
  zz = data.frame(sapply(testin, class), sapply(testout, class))
  zz = zz[zz[,1] != zz[,2], ]

  expect_equal(
     testout[, !(names(testout) %in% rownames(zz))] ,
     testin[, !(names(testin) %in% rownames(zz))]
  )
  ############## #
  # bysite data in excel matches original data, except some columns that are logical vs integer, NA vs ""

  testin = as.data.frame(testoutput_ejamit_10pts_1miles$results_bysite)[, common_names_r]
  testout = tab_bysite[, common_names_long]
  names(testout) <- common_names_r
  zz = data.frame(sapply(testin, class), sapply(testout, class))
  zz = zz[zz[,1] != zz[,2], ]

  expect_equal(
    testout[, !(names(testout) %in% rownames(zz))] ,
    testin[, !(names(testin) %in% rownames(zz))]
  )
  ############## #

  # t4 <- readxl::read_excel(fname, sheet = 4) %>% as.data.frame()
  # t5 <- readxl::read_excel(fname, sheet = 5) %>% as.data.frame()


## notes on possibly checking column names created in the spreadsheet:

  # rnames = fixcolnames(names(tab_bysite),  "long", "r")
  # rnames = fixcolnames(rnames, 'short', 'r')
  # dput(setdiff(rnames, names_all_r))
  ## 41 columns did not seem to be exactly a long or short version of an r name
  # c("EJScreen Report", "EJScreen Map", "ACS Report", "ECHO Report",
  #   "ejam_uniq_id", "valid", "invalid_msg", "in_how_many_states",
  #   "Percent of Households below Poverty Level", "Percent Males",
  #   "Percent Females", "Percent Owner Occupied households", "Percent under age 18",
  #   "Percent above age 17", "Percent of population speaking Other Indo-European at home",
  #   "Percent of population speaking Asian and Pacific Island languages at home",
  #   "Percent of population speaking Other and Unspecified languages at home",
  #   "Percent of population speaking English at home", "Percent of population speaking Spanish at home",
  #   "Percent of population speaking French at home", "Percent of population speaking Russian, Polish or Other Slavic at home",
  #   "Percent of population speaking Indo-European at home", "Percent of population speaking Vietnamese at home",
  #   "Percent of population speaking Other Asian and Pacific Island languages at home",
  #   "Percent of population speaking Arabic at home", "Percent of population speaking Non English languages at home",
  #   "Percent Speaking Spanish (as % of limited English households)",
  #   "Percent Speaking Other Indo-European languages (as % of limited English households)",
  #   "Percent Speaking Asian-Pacific Island languages (as % of limited English households)",
  #   "Percent Speaking Other languages (as % of limited English households)",
  #   "US percentile for %Low life expectancy", "State percentile for %Low life expectancy",
  #   "US Average for %Low life expectancy", "State Average for %Low life expectancy",
  #   "Universe for percent unemployed (denominator, count)", "Built housing units count (denominator for percent pre 1960)",
  #   "Persons who Speak English Not at All", "Speak Spanish (in limited English household)",
  #   "Speak Other Indo-European (in limited English household)", "Speak Asian-Pacific Island (in limited English household)",
  #   "Speak Other (in limited English household)")

  # x = data.frame(xls = names(tab_overall), xls_if_short2r=  fixcolnames(names(tab_overall), 'short', 'r'), xls_if_long2r = fixcolnames(names(tab_overall), 'long', 'r') )
  #   x$is_short = x$xls != x$xls_if_short2r
  #   x$is_long = x$xls != x$xls_if_long2r
  #   table(long=x$is_long, short=x$is_short)
  ##         short
  ## long    FALSE TRUE
  ## FALSE    41    6
  ## TRUE    393   29

  ## > # only 6 names are short only,

  ##  41 are NEITHER short nor long:

  # c("EJScreen Report", "EJScreen Map", "ACS Report", "ECHO Report",
  #   "ejam_uniq_id", "valid", "invalid_msg", "in_how_many_states", #

  # "%Low life expectancy", "Ratio to US avg %Low life expectancy", "Ratio to State avg %Low life expectancy",  --- but these are shortnames
  # "US percentile for %Low life expectancy", "State percentile for %Low life expectancy",
  # "US Average for %Low life expectancy",    "State Average for %Low life expectancy",

  #   "Universe for percent unemployed (denominator, count)",
  #   "Built housing units count (denominator for percent pre 1960)",
  # "Percent of Households below Poverty Level",
  # "Percent Males",   "Percent Females",
  # "Percent Owner Occupied households",
  # "Percent under age 18",  "Percent above age 17",

  #   "Percent of population speaking Other Indo-European at home",
  #   "Percent of population speaking Asian and Pacific Island languages at home",
  #   "Percent of population speaking Other and Unspecified languages at home",
  #   "Percent of population speaking English at home",
  #   "Percent of population speaking Spanish at home",
  #   "Percent of population speaking French at home",
  #   "Percent of population speaking Russian, Polish or Other Slavic at home",
  #   "Percent of population speaking Indo-European at home",
  #   "Percent of population speaking Vietnamese at home",
  #   "Percent of population speaking Other Asian and Pacific Island languages at home",
  #   "Percent of population speaking Arabic at home",
  #   "Percent of population speaking Non English languages at home",

  #   "Percent Speaking Spanish (as % of limited English households)",
  #   "Percent Speaking Other Indo-European languages (as % of limited English households)",
  #   "Percent Speaking Asian-Pacific Island languages (as % of limited English households)",
  #   "Percent Speaking Other languages (as % of limited English households)",

  #   "Speak Other Indo-European at Home", "Speak Asian-Pacific Island language at Home",
  #   "Persons who Speak English Not at All", "Speak Spanish at Home",
  #   "Speak Spanish (in limited English household)", "Speak Other Indo-European (in limited English household)",
  #   "Speak Asian-Pacific Island (in limited English household)",
  #   "Speak Other (in limited English household)")

    ## all others are longname

  # x[x$is_short & !x$is_long,]
  ##                                         xls              xls_if_short2r                               xls_if_long2r is_short is_long
  ## 46         Ratio to US avg %Low life expectancy       ratio.to.avg.lowlifex        Ratio to US avg %Low life expectancy     TRUE   FALSE
  ## 78      Ratio to State avg %Low life expectancy ratio.to.state.avg.lowlifex     Ratio to State avg %Low life expectancy     TRUE   FALSE
  ## 124                        %Low life expectancy                    lowlifex                        %Low life expectancy     TRUE   FALSE
  ## 340           Speak Other Indo-European at Home                      lan_ie           Speak Other Indo-European at Home     TRUE   FALSE
  ## 341 Speak Asian-Pacific Island language at Home                     lan_api Speak Asian-Pacific Island language at Home     TRUE   FALSE
  ## 345                       Speak Spanish at Home                 lan_spanish                       Speak Spanish at Home     TRUE   FALSE


})
