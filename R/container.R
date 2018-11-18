#' Abstract container.
#'
#' Used to store artifacts or commits.
#'
#' @param x object to be converted to a `container` or tested.
#'
#' @export
#' @rdname container
is_container <- function (x) inherits(x, 'container')

#' @export
#' @rdname container
as_container <- function (x) {
  stopifnot(is.list(x))
  structure(x, class = 'container')
}

#' @export
#' @rdname container
#' @inheritDotParams base::print
print.container <- function (x, ...) {
  cat0('<container, ', length(x), ' element(s)>')
  invisible(x)
}
