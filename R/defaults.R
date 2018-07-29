#' @export
set_defaults <- function (x, ...) {
  values <- list(...)
  stopifnot(is_all_named(values))
  
  for (n in names(values)) {
    attr(x, n) <- values[[n]]
  }
  
  x
}

#' @export
get_default <- function (x, name, default = NULL) {
  if (is.null(attr(x, name))) return(default)
  attr(x, name)
}
