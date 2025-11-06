
meters_per_mile <-  convert_units(1, from = 'miles', towhat = 'meters') # 1609.344  convert_units()
attributes(meters_per_mile) <- NULL
usethis::use_data(meters_per_mile, internal = FALSE, overwrite = TRUE)
dataset_documenter("meters_per_mile", "how many meters are in one mile (for conversions between units)", description = "1609.344 meters per mile")
