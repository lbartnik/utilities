#' Working with names.
#' 
#' @param x Object to check or set `names` on.
#' @param names Names to set on `x`.
#' @return `x` with `names` set as names.
#'
#' @export
#' @rdname names
with_names <- function (x, names) {
  stopifnot(identical(length(x), length(names)))
  names(x) <- names
  x
}

#' @export
#' @rdname names
all_named <- function (x) {
  nms <- names(x)
  if (is.null(nms)) return(with_names(x, rep("", length(x))))
  x
}

#' @export
#' @rdname names
is_all_named <- function (x) {
  all(names(x) != "")
}

