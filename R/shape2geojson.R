
#' convert spatial data.frame to a vector of geojson text strings
#' @details helper for [url_ejamapi()]
#' Note it removes all spaces in the string.
#'
#' @param shp spatial data.frame to be written via [sf::st_write()]
#' @param file optional file path and name, useful if txt=F
#' @param txt optional logical, set to FALSE to just get the path to a temp .geojson file
#'
#' @returns if txt=T, returns 1 geojson text string for the entire input spatial data.frame
#'   if txt=F, returns 1 file path/name of .geojson file
#' @examples
#' shp =  testinput_shapes_2[2, c("geometry", "FIPS")]
#' x = shape2geojson(shp)
#' nchar(x)
#'
#' @export
#'
shape2geojson = function(shp, file = file.path(tempdir(), "shp.geojson"), txt = TRUE) {

  # sf::st_write(EJAM::testinput_shapes_2[2,], file.path(tempdir(), "junk1.geojson"))

  sf::st_write(shp, dsn = file, delete_dsn = TRUE)
  if (txt) {
    x = readLines(file)
    x = paste0(x, collapse = "")
    x = gsub(" ", "", x) # is it ok to remove all spaces?
    cat("use URLencode() to encode this string for use in a URL \n")
    return(x) # text string
  } else {
    return(file) # path
  }
}
##################################################################### #
