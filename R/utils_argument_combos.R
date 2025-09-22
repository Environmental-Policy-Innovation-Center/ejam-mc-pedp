
argument_combos = function(FUN = create_filename, ..., quiet = FALSE) {

  # create a table of arguments to try in a function, in all combinations of those arguments

  if (missing(...)) {
    parameters_table = expand.grid(
      filename_base = 'EJAM',
      buffer_dist = c(0, 3.2),
      site_method = c("", "latlon"),
      ext = c(NULL, ".html"),
      file_desc=c("", "FILE DESCRIPTION"),
      title = c("", "My Title"),
      stringsAsFactors = FALSE
    )
  } else {
    parameters_table = expand.grid(
      ...,
      stringsAsFactors = FALSE
    )
  }

  # run function with each set of arguments
  allout = purrr::pmap(parameters_table, FUN)

  if (!quiet) {
    # print results for each combo of arguments
    cbind(output_filename = cbind(allout),
          parameters_table)
  }

  invisible(allout)
}
