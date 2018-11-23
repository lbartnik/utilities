#' Accessing properties.
#'
#' @param x Object to be checked.
#' @param name Name that `x` is supposed to have.
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

#' @param i Index `x` is supposed to have.
#'
#' @export
#' @rdname properties
is_index_of <- function (i, x) {
  if (is.numeric(i)) return(i > 0 && i <= length(x))
  i %in% names(x)
}

#' @param table Container where `x` is not supposed to be present.
#'
#' @export
#' @rdname properties
`%nin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

is_double_colon <- function (x) identical(x, quote(`::`))

is_nonempty_string <- function (x) is.character(x) && identical(length(x), 1L) && nchar(x)


#' @description `is_rstudio` returns `TRUE` if is called inside RStudio.
#'
#' @export
#' @rdname properties
is_rstudio <- function () {
  requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()
}

#' @importFrom utils getS3method
has_print <- function (x) {
  is <- map_lgl(class(x), function (cx) !is.null(getS3method("print", cx, optional = TRUE)))
  any(is)
}
