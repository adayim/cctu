#' Gets copies of objects from parent environments, or returns alternative if
#' not found
#'
#' @param name text string of an object to find
#' @param alt an alternative to return if the object is not found
#'
#' @return either a copy of the object \code{name} or an alternative if it does
#'  not exist
#' @details this is designed to use within functions in the library to access
#'  global objects, but provide a route to avoid hard-coding specific variable
#'  names in. It is not exported.
#'
#' @keywords internal
#' @examples
#' library(cctu)
#' rm(PATH)
#' cctu:::get_obj("PATH")
#' cctu:::get_obj("PATH", alt = getwd())
#' PATH <- "C:/MyFile"
#' cctu:::get_obj("PATH")
#' cctu:::get_obj("PATH", alt = getwd())
get_obj <- function(name, frame = parent.frame(), alt = NULL) {
  if (exists(name, where = frame)) {
    # this searches upwards into enclosing frames
    get(name, pos = frame)
  } else {
    warning(paste(name, "not found"))
    alt
  }
}
