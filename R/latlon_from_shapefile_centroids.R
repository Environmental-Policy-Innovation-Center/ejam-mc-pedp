
latlon_from_shapefile_centroids = function(shapefile)  {

  if ("INTPTLAT" %in% names(shapefile) & "INTPTLON" %in% names(shapefile)) {
  sitepoints = data.frame(lat = as.numeric(shapefile$INTPTLAT),
                          lon = as.numeric(shapefile$INTPTLON))
} else {
  # at least get points that are coordinates of centroids of polygons
  suppressWarnings({
    sitepoints = sf::st_coordinates(sf::st_centroid(shapefile) )
  })
  colnames(sitepoints) <- c("lon", "lat")
  sitepoints = as.data.frame(sitepoints)
}
return(sitepoints )
}

