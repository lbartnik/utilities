#' Get/set default values as attributes.
#'
#' @param x Object to get/set attributes on/from.
#' @param ... Named attribute values.
#'
#' @export
#' @rdname defaults
set_defaults <- function (x, ...) {
  values <- list(...)
  stopifnot(is_all_named(values))
  
  for (n in names(values)) {
    attr(x, n) <- values[[n]]
  }
  
  x
}

#' @param name Name of the default value.
#' @param default Return if attribute not set on `x`.
#'
#' @export
#' @rdname defaults
get_default <- function (x, name, default = NULL) {
  if (is.null(attr(x, name))) return(default)
  attr(x, name)
}

