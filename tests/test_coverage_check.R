################################ #
# search for one query term in a list of files
find_in_files <- function(pattern, path = "./tests/testthat", filename_pattern = "\\.R$|\\.r$") {
  x <- list.files(path = path, pattern = filename_pattern, recursive = TRUE, full.names = TRUE)
  names(x) <- x
  x |>
    purrr::map(~grep(pattern, readLines(.x, warn = FALSE), value = TRUE)) |>
    purrr::keep(~length(.x) > 0)
}
################################ #
# search for vector of query terms, to see which ones are found in any of the files
found_in_files <- function(pattern_vector, path = "./R") {
  found = vector(length = length(pattern_vector))
  for (i in seq_along(pattern_vector)) {
    hits = find_in_files(pattern_vector[i], path = path)
    found[i] <- length(hits) > 0
  }
  foundones = pattern_vector[found]
  print(foundones)
  return(found) # logical vector
}
################################ #
# frequency of occurrences of each term within a list of files
# actually how many lines of code does it appear in so counts as 1 each line where it appears even if it appears >1x in that line
found_in_N_files_T_times <- function(pattern_vector, path = "./R") {
  nfiles <- vector(length = length(pattern_vector))
  nhits <- vector(length = length(pattern_vector))
  for (i in seq_along(pattern_vector)) {
    hits <- find_in_files(pattern_vector[i], path = path)
    nfiles[i] <- length(hits)
    nhits[i] <- length(as.vector(unlist(hits)))
    # found[i] <- length(hits) > 0
  }
  # foundones <- pattern_vector[found]
  out <- data.frame(term = pattern_vector,
                   nfiles = nfiles,
                   nhits = nhits
  )
   print(head(
     out[order(out$nfiles, out$nhits, decreasing = TRUE), ]
     ), 10)
  invisible(out)
}
################################ ################################# #

# see  also, test_coverage() which computes test coverage for your package. It's a shortcut for covr::package_coverage() plus covr::report().
# see  https://covr.r-lib.org/

# see  RStudio addin that does  covr::report()   ?covr::package_coverage()

################################ ################################# #

# this is a DRAFT / NOT IDEAL function to get a very basic check on
# which functions seem to clearly have unit test files already
# It could be replaced with something simpler and clearer.


## example of some of its output:

# Number of exported functions from the package (note the # might be higher if you have used load_all()...):  598
# Number of exported functions with exactly matching test file names:  57
# Number of exported functions with no matching test file names:  541
# or  129 where the function does not even appear at all in full text of any test file:
#
# These dont seem to have tests but are used (or mentioned) by the most R/*.R files:
#                       term nfiles nhits
# 1              table_round     23    48  ***
# 2               ejam2excel     16    29  ***
# 3      frs_update_datasets     15    20
# 4              ejam2report     14    41  ***
# 5      table_rounding_info     12    25  ***
# 6             testpoints_n     12    27
# 7              indexblocks     11    24
# 8         fixnames_aliases     11    32  ***
# 9                  frs_get     11    24
# 10     sitepoints_from_any     11    44  ***
# 11            table_signif     11    28  ***
# 12 table_signif_round_x100     11    23  ***
# 13                  app_ui     10    17
# 14                datapack      9    27
# 15          read_csv_or_xl      9    22
# 16               calc_ejam      8    32  ***



test_coverage_check <- function() {


  # MUST BE IN ROOT OF A PACKAGE WHOSE NAME MATCHES THE DIR so that functions_in_pkg(basename(getwd())) will work



  # removed dependency on fs pkg, and  dplyr, tibble, stringr pkgs are already in Imports of DESCRIPTION file.

  cat("Looking in the source package EJAM/R/ folder for files like xyz.R, and in the EJAM/tests/testthat/ folder for test files like test-xyz.R \n")
  tdat = dplyr::bind_rows(
    tibble::tibble(
      type = "R",
      path = file.path("R", list.files("R/", pattern = "\\.[Rr]$")),
      name = as.character(tools::file_path_sans_ext(basename(path)))
    ),
    tibble::tibble(
      type = "test",
      path = file.path("tests/testthat", list.files("tests/testthat/", pattern = "^test[^/]+\\.[Rr]$")),
      name = as.character(tools::file_path_sans_ext(stringr::str_remove(basename(path), "^test[-_]")))
    )
  ) |>
    tidyr::pivot_wider(names_from = type, values_from = path)
  tdat <- tdat[order(tdat$name), ]

  names(tdat) <- gsub("name",   "object", names(tdat))
  names(tdat) <- gsub("R",    "codefile", names(tdat))
  names(tdat) <- gsub("test", "testfile", names(tdat))

  tdat$object <- gsub("^[^a-zA-Z_]+", "", tdat$object) # remove leading non-alphabetic characters ?

  cat("Checking all exported functions, not internal ones, BUT, if you just did load_all() then this will check ALL\n")
  capture.output({
    suppressWarnings({
      y <- EJAM:::functions_in_pkg(basename(getwd()),data_included = F, exportedfuncs_included = T, internal_included = TRUE )
    })
  })
  tdat$object_is_in_pkg <- tdat$object %in% y$object
  tdat$utils_object_is_in_pkg <- gsub("^utils_", "", tdat$object) %in% y$object

  tdat <- tdat[order(tdat$object_is_in_pkg, tdat$object), ]

  tdat$object[!tdat$object_is_in_pkg & !tdat$utils_object_is_in_pkg] <- NA
  tdat$object[!tdat$object_is_in_pkg & tdat$utils_object_is_in_pkg] <- gsub("^utils_", "", tdat$object[!tdat$object_is_in_pkg & tdat$utils_object_is_in_pkg])


  tdat$notes <- ""
  tdat$notes[!is.na(tdat$testfile) & !is.na(tdat$codefile)] <- "ok? exact match of 'R/x.R' and 'tests/testthat/test-x.R'"
  tdat$notes[!is.na(tdat$testfile) & !is.na(tdat$codefile)  & tdat$object_is_in_pkg]       <- "ok, object has a test file, exact match"
  tdat$notes[!is.na(tdat$testfile) & !is.na(tdat$codefile)  &!tdat$object_is_in_pkg & tdat$utils_object_is_in_pkg] <- "ok, object has a test file, though .R file and test file have 'utils_' prefix"
  tdat$notes[!is.na(tdat$testfile) & is.na(tdat$codefile) & tdat$object_is_in_pkg] <- "ok, object name and test file name match, though .R file name differs"
  tdat$notes[ is.na(tdat$testfile) & !is.na(tdat$codefile) & "data_" != substr(tdat$object, 1,5) & tdat$object_is_in_pkg]       <- "ok, object has a test file??"
  tdat$notes[ is.na(tdat$testfile) & !is.na(tdat$codefile) & "data_" != substr(tdat$object, 1,5) & tdat$utils_object_is_in_pkg] <- 'tbd' #
  tdat$notes[grepl("functionality.R$|ui_and_server.R$|test1.R$|test2.R$", tdat$testfile)] <- "ok, test file is for app functionality not a function"
  tdat$notes[!is.na(tdat$testfile) & is.na(tdat$codefile) & tdat$utils_object_is_in_pkg & grepl("test-utils_", tdat$testfile)] <- "ok, testfile prefixed with utils_ but otherwise matches object, though .R filename differs"
  justdata <- "R/data_" == substr(tdat$codefile, 1,7) & !is.na(tdat$codefile)
  tdat$notes[is.na(tdat$testfile) & !is.na(tdat$codefile) & !justdata  ] <- "cant find testfile"

  funcs_not_in_txt_of_testfiles_at_all = NULL
  func2searchfor = tdat$object[!is.na(tdat$object) & tdat$notes == "cant find testfile"]
  for (i in seq_along(func2searchfor)) {
    x = find_in_files(paste0(func2searchfor[i], ""))
    if (length(x) > 1) {
      tdat$notes[tdat$object %in% func2searchfor[i]] <- paste0("cant find testfile, BUT at least obj appears in full txt of ", length(x), " testfile(s)")
    } else {
      funcs_not_in_txt_of_testfiles_at_all = c(funcs_not_in_txt_of_testfiles_at_all, func2searchfor[i])
      tdat$notes[tdat$object %in% func2searchfor[i]] <- paste0("cant find testfile, and obj not even used within any testfile")
    }
  }
  ################################ #
  cat("\n\nCOVERAGE CHECK \n\n")
  # tdat %>%   print(n = Inf) # to see everything
  ################################ #
  cat(' -----------------------------------------------

      MATCHED EXACTLY -- all test files that exactly match name of a .R file: \n\n')

  tdat[!is.na(tdat$testfile) & !is.na(tdat$codefile), ] |> print(n = 500)
  ################################ #
  cat(' -----------------------------------------------

  All test files that were not named based on a .R file,
     making it hard to know which .R files really lack tests
      but note some are actual objects in the package:\n\n')

  tdat[!is.na(tdat$testfile) & is.na(tdat$codefile),  ] |> print(n = 500)
  ################################ #
  cat(" -----------------------------------------------

      CHECK THESE
      - TESTS MISSING, like for dataload_dynamic() --  (note object_is_in_pkg column) -- or
      - SOME TEST FILES SPLIT OUT FROM MULTIFUNCTION code files, LIKE frs_from_xyz.R SPLIT INTO test-frs_from_naics.R ETC.  or
      - SOME TEST FILES GROUPED 2+ code files? (note object_is_in_pkg column)

      These are the .R files that lack a test file with exactly matching name:\n\n")
  justdata <- "R/data_" == substr(tdat$codefile, 1,7) & !is.na(tdat$codefile)
  x = tdat[is.na(tdat$testfile) & !is.na(tdat$codefile) & !justdata, ]
  x[order(x$object), ] |> print(n = 500)
  ################################ #
  cat('
      -----------------------------------------------\n\n')

  junk = capture.output({
    suppressWarnings({
      y = EJAM:::functions_in_pkg('EJAM', internal_included = TRUE, exportedfuncs_included = T, data_included = F, vectoronly = T)
    })})
  # print(setdiff(y, gsub("tests/testthat/test-|.R$", "", tdat$testfile)))
  cat('\n')
  cat("Number of exported functions from the package (note the # is much higher if you have used load_all()...): ",
      length(y), '\n'
  )
  cat("Number of exported functions with exactly matching test file names: ",
      length((intersect(y, gsub("tests/testthat/test-|.R$", "", tdat$testfile)))), '\n'
  )
  cat("Number of exported functions with no matching test file names: ",
      length((setdiff(y, gsub("tests/testthat/test-|.R$", "", tdat$testfile)))), '\n',
      "or ", length(funcs_not_in_txt_of_testfiles_at_all), "where the function does not even appear at all in full text of any test file:", '\n\n'
  )
  #  cat(paste0(funcs_not_in_txt_of_testfiles_at_all, collapse = ", "), "\n\n")
  (dput(funcs_not_in_txt_of_testfiles_at_all))
  cat("\n\n")

  freq = found_in_N_files_T_times(funcs_not_in_txt_of_testfiles_at_all, path = "./R")
  freq = freq[order(freq$nfiles, decreasing = T), ]
  rownames(freq) <- NULL
  cat("These dont seem to have tests but are used (or mentioned) by the most R/*.R files: \n")
  print(head(freq, 20))

  cat("\n\n")
  cat("also see https://covr.r-lib.org/ and test_coverage() which computes test coverage for your package. It's a shortcut for covr::package_coverage() plus covr::report().\n")
  invisible(tdat)
}


## to use this function:
#
#  tdat <-  test_coverage_check() # to get report and see key info
#  tdat %>%   print(n = Inf) # to see everything



## also see

# test_coverage() computes test coverage for your package. It's a shortcut for covr::package_coverage() plus covr::report().

# y = EJAM:::functions_in_pkg('EJAM')

## and

# x = EJAM:::linesofcode2(packages = 'EJAM')
