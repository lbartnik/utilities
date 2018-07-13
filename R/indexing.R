#' Indexing.
#' 
#' @export
#' @rdname indexing
nth <- function(x, n) {
  if (!length(x)) return(vector(mode = typeof(x)))
  x[[n]]
}

#' @export
#' @rdname indexing
last <- function(x) nth(x, length(x))

#' @export
#' @rdname indexing
first <- function(x) nth(x, 1)

#' @export
#' @rdname indexing
second <- function(x) nth(x, 2)
