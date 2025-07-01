
# see  https://covr.r-lib.org/

# see  RStudio addin that does  covr::report()   ?covr::package_coverage()


################################ #

# below is a draft function to get a very basic check on
# which functions seem to clearly have unit test files already


test_coverage_check <- function() {

  # MUST BE IN ROOT OF A PACKAGE WHOSE NAME MATCHES THE DIR so that pkg_functions_and_data(basename(getwd())) will work

  # remove dependency on fs pkg, and  dplyr, tibble, stringr pkgs are already in Imports of DESCRIPTION file.

  cat("Looking in the source package EJAM/R/ folder for files like xyz.R, and in the EJAM/tests/testthat/ folder for test files like test-xyz.R \n")
  tdat = bind_rows(
    tibble(
      type = "R",
      path = fs::dir_ls("R/", regexp = "\\.[Rr]$"),
      name = as.character( fs::path_ext_remove(fs::path_file(path))),
    ),
    tibble(
      type = "test",
      path = fs::dir_ls("tests/testthat/", regexp = "/test[^/]+\\.[Rr]$"),
      name = as.character( fs::path_ext_remove(str_remove( fs::path_file(path), "^test[-_]"))),
    )
  ) |>
    tidyr::pivot_wider(names_from = type, values_from = path)
  tdat <- tdat[order(tdat$name), ]

  names(tdat) <- gsub("name",   "object", names(tdat))
  names(tdat) <- gsub("R",    "codefile", names(tdat))
  names(tdat) <- gsub("test", "testfile", names(tdat))

  y <- EJAM:::pkg_functions_and_data(basename(getwd()))
  tdat$object_is_in_pkg <- tdat$object %in% y$object
  tdat <- tdat[order(tdat$object_is_in_pkg, tdat$object), ]
  ################################ #
  cat("\n\nCOVERAGE CHECK \n\n")
  # tdat %>%   print(n = Inf) # to see everything
  ################################ #

  cat(' -----------------------------------------------

  All test files that were not named based on a .R file,
     making it hard to know which .R files really lack tests:\n\n')

  tdat[!is.na(tdat$test) & is.na(tdat$R),  ] |> print(n = 500)
  ################################ #

  cat(" -----------------------------------------------

      All .R files that lack a test file with exactly matching name:\n\n")

  tdat[is.na(tdat$test) & !is.na(tdat$R) & "data_" != substr(tdat$name, 1,5), ] |> print(n = 500)

  cat(" But those .R files contain functions that may have test files named after the functions not the whole .R file: \n")
  # can use  EJAM:::pkg_functions_and_sourcefiles()  to see which functions are defined within a given .R source filename
  x = tdat[is.na(tdat$test) & !is.na(tdat$R) & "data_" != substr(tdat$name, 1,5), ]
  y = pkg_functions_and_sourcefiles()

  ################################ #

  cat(' -----------------------------------------------

      MATCHED EXACTLY -- all test files that exactly match name of a .R file: \n\n')

  tdat[!is.na(tdat$test) & !is.na(tdat$R), c("R", "test")] |> print(n = 500)
  ################################ #
  cat('
      -----------------------------------------------\n\n')

  invisible(tdat)
}


## to use this function:
#
#  tdat <-  test_coverage_check() # to get report and see key info
#  tdat %>%   print(n = Inf) # to see everything


## also see

# test_coverage() computes test coverage for your package. It's a shortcut for covr::package_coverage() plus covr::report().

# y = EJAM:::pkg_functions_and_data('EJAM')

## and

# x = EJAM:::linesofcode2(packages = 'EJAM')
