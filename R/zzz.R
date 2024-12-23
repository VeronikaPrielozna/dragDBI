# zzz.R: Initialization for dragDBI

utils::globalVariables(c("DBI_SA", "DBI_CE", "DBI_CEC"))

.onLoad <- function(libname, pkgname) {
  utils::data(list = c("DBI_SA", "DBI_CE", "DBI_CEC"),
              package = pkgname,
              envir = parent.env(environment()))
}
