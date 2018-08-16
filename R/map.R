#' Purrr-like API.
#'
#' @param x Container.
#' @param f Function.
#' @param ... Extra arguments for `f`.
#' 
#' @export
#' @rdname map
map <- function (x, f, ...) {
  ans <- lapply(x, f)
  if (!is.null(names(ans))) return(ans)
  names(ans) <- as.character(x)
  ans
}

#' @export
#' @rdname map
map_chr <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.character(unlist(ans))
}

#' @export
#' @rdname map
map_dbl <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.numeric(unlist(ans))
}

#' @export
#' @rdname map
map_int <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.integer(unlist(ans))
}

#' @export
#' @rdname map
map_lgl <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.logical(unlist(ans))
}

#' @export
#' @rdname map
imap <- function (x, f, ...) {
  if (!length(x)) return(list())
  x <- as.list(x)
  
  if (is.null(names(x))) {
    nms <- seq_along(x)
  }
  else {
    nms <- names(x)
  }
  
  ans <- mapply(value = x, name = nms, function (value, name) f(value, name, ...),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
  names(ans) <- nms
  ans
}

