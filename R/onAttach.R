.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to lextremagam. For the proper functionning of this package, please ensure that mgcv is loaded.\nOther useful packages include marginaleffects, dplyr, gratia, data.table, and ggplot2")
}
