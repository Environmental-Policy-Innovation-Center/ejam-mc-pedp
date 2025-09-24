
nacounts = function(x, showall = FALSE) {

  if (NCOL(x) == 1) {
    return(
      data.frame(nas = sum(is.na(x)),
                 other = sum(!is.na(x)))
    ) # ignores showall
  } else {
    if (showall) {
      shown <- rep(TRUE, NCOL(x))
    } else {
      shown <- sapply(x, anyNA)
    }
    z = data.frame(
      nas = sapply(x, function(y) sum(is.na(y))),
      other = sapply(x, function(y) sum(!is.na(y)))
    )
    z = z[shown,]
    return(z)
  }
}
### e.g.,  nacounts(data.frame(a = 1:3, b= c(NA, NA, 0), d = c(NA,1,1)))
##################################################################### #
