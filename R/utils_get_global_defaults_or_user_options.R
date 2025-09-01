
#' utility that reconciles/ consolidates user-defined params passed via ejamapp() and settings from global_defaults_ files
#'
#' @param user_specified_options named list of any optonal arguments that were in the call to [ejamapp()]
#' @param bookmarking_allowed same as [shiny::shinyApp] enableBookmarking param
#'
#' @returns a list of global defaults or user options that [ejamapp()]
#'   uses as the golem_opts parameter in [golem::with_golem_options()]
#'   and that later can be retrieved by server or ui via [golem::get_golem_options()]
#'   or via [global_or_param()] (which both do almost the same thing).
#' @details
#' This function, called by [ejamapp()],
#' collects the shiny-app-related default settings that are defined in these places:
#'
#' 1. any options a user has passed as parameters to [ejamapp()]. If provided, these override defaults specified in global_defaults_*.R files.
#'
#' 2. "global_defaults_package" set in file `global_defaults_package.R`  -- sourced here but also initially by [.onAttach()]
#'
#' 3. global defaults set in file `global_defaults_shiny.R` -- sourced here
#'
#' 4. global defaults set in file `global_defaults_shiny_public.R` -- and in that file, depends on value of isPublic if passed as a param to [ejamapp()]
#'
#' and consolidates them all as a list, to be available to server/ui.
#'
#' For more details, see the
#' [article about defaults and custom settings](`r paste0(EJAM:::repo_from_desc('github.io', get_full_url=T), "/articles/")`).
#'
#' See other ideas for how to include global.R types of code/settings discussed [here](https://github.com/ThinkR-open/golem/issues/6)
#'
#' @keywords internal
#'
get_global_defaults_or_user_options <- function(user_specified_options = NULL, bookmarking_allowed = 'url') {

  ############ #  ############ #
  # Define a helper function:
  #   For each element of the input list,
  #    if it is not already defined in global_defaults_or_user_options,
  #    append it to global_defaults_or_user_options.
  # This assumes global_defaults_or_user_options is in the calling envt (or at least search path)
  # and returns an updated version of it.

  update_global_defaults_or_user_options <- function(app_defaults) {

    for (o in names(app_defaults)) {
      if (!o %in% names(global_defaults_or_user_options)) {
        global_defaults_or_user_options[[o]] <- app_defaults[[o]]
      }
    }
    return(global_defaults_or_user_options)
  }
  ############ #  ############ #

  ############ #
  # 1. get any options a user has passed as parameters to [ejamapp()] ####
  #
  # Save whatever options user specifies & prioritize those over what may be in global_defaults_*.R files.
  # Options unspecified may have default values in global_defaults_shiny_public.R
  # The final list of shiny options gets passed to golem_opts

  global_defaults_or_user_options <- user_specified_options
  global_defaults_or_user_options$bookmarking_allowed <- bookmarking_allowed

  ############ #
  # 2. get "global_defaults_package" set in file `global_defaults_package.R` ####
  #
  ## global_defaults_package  should already be in global env from .onAttach(), but if one does rmost() or rm(list=ls()) and then tries ejamapp(), it is missing and app cannot find logo for summary report.
  ## could re-load it here just in case that comes up:  # consider if local=T or =F makes sense here. ***
  ## source(local=F) means the parsed expressions are evaluated in the user's workspace (the global environment) not just this function's local env (the environment from which source is called).
  ## source() uses the latest local source code version of that file, not installed version, if devtools::load_all() has been done.

  source(system.file("global_defaults_package.R", package = "EJAM"), local = FALSE)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_package)

  ############ #
  # 3. settings defined in file global_defaults_shiny.R ####

  source(system.file("global_defaults_shiny.R", package = "EJAM"), local = TRUE)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_shiny)

  ############ #
  # 4. settings defined in file global_defaults_shiny_public.R ####
  #
  # The isPublic parameter controls what is displayed in a public/streamlined version of the app.
  # We handle isPublic in this special way below since it has to be available in this calling envt
  # so that when we source global_defaults_shiny_public.R local=T it can be checked and used to set defaults correctly.

  if ("isPublic" %in% names(user_specified_options)) {
    isPublic <- user_specified_options$isPublic
  }
  source(system.file("global_defaults_shiny_public.R", package = "EJAM"), local = TRUE) # local=T avoids filling global env with the lists like help_texts etc.
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_shiny_public)
    # other lists of settings also from that file:
  global_defaults_or_user_options <- update_global_defaults_or_user_options(aboutpage_texts)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(help_texts)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(html_fmts)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(sanitize_functions)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(extratable_stuff)

  ############ #
  # 5. Bookmarks:
  #
  # Also note that url-encoded bookmarked inputs can launch the app
  # and pass various input$ settings to the app, which would override
  # the default values of those input$ settings provided by the global_defaults_*.R
  # and/or ejamapp() parameters handled by the function
  # get_global_defaults_or_user_options().
  # See ?ejamapp() and the article about defaults and custom settings.

  ############ #
  # not needed but ok:
  rm(global_defaults_shiny_public)
  rm(global_defaults_shiny)
  rm(global_defaults_package)

  return(global_defaults_or_user_options)
}
