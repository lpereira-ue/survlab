.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "survlab loaded!\n",
    "Use survlab_logo() to display the package logo.\n",
    "Use survlab_info() for package information."
  )
}