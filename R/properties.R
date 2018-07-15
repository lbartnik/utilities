#' Accessing properties.
#' 
#' @export
#' @rdname properties
has_name <- function (x, name) isTRUE(all(name %in% names(x)))

#' @export
#' @rdname properties
is_empty <- function (x) {
  if (is.environment(x)) return(!length(x))
  is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))
}

#' @export
#' @rdname properties
is_error <- function (x) inherits(x, c('error', 'try-error', 'simpleError'))

#' @export
#' @rdname properties
is_atomic_class <- function (x) isTRUE(x %in% c("numeric", "character", "integer", "logical", "complex"))

#' @export
#' @rdname properties
is_recorded_plot <- function (x) inherits(x, 'recordedplot')

#' @export
#' @rdname properties
is_index_of <- function (i, x) {
  if (is.numeric(i)) return(i > 0 && i <= length(x))
  i %in% names(x)
}
