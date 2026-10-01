.onLoad <- function(libname, pkgname) {
  # Without a registered handler, logging messages are not printed
  logging::basicConfig()
}
