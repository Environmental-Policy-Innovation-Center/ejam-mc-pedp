library(golem)
library(rmarkdown)
unlink("tests/shinytestlog.txt") # deletes this file if it exists

## This line was causing a problem:

#shinytest2::load_app_env()

# That function normally Executes all ./R files and global.R into the current environment. This is useful when wanting access to functions or values created in the ./R folder for testing purposes.
