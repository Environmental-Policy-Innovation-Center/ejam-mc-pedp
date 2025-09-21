if (basename(getwd()) != "EJAM") {stop("must start in root of source folder")}
source("./data-raw/datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R")

library(EJAM)
EJAM:::pkgdown_update(doask = F)
beepr::beep()
