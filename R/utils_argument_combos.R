
argument_combos = function(FUN, ..., quiet = FALSE) {

  # create a table of arguments to try in a function, in all combinations of those arguments

  #
  # ### example:
  # FUN = EJAM:::create_filename
  # arglist <- list(
  #   filename_base = 'EJAM',
  #   buffer_dist = c(0, 3.2),
  #   site_method = c("", "latlon"),
  #   ext = c(NULL, ".html"),
  #   file_desc=c("", "FILE DESCRIPTION"),
  #   title = c("", "My Title"))
  # argument_combos(FUN, arglist)


  if (missing(FUN) || !is.function(FUN)) {
    stop("FUN is essential")
  }
  if (missing(...)) {
    stop("... named list of arguments is needed")
  }

  parameters_table = expand.grid(
    ...,
    stringsAsFactors = FALSE
  )

  # run function with each set of arguments
  allout = purrr::pmap(parameters_table, FUN)

  if (!quiet) {
    # print results for each combo of arguments
    print(
      cbind(output_filename = cbind(allout),
            parameters_table)
    )
  }

  invisible(allout)
}

