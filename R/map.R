#' Purrr-like API.
#' 
#' @export
#' @rdname map
map_lst <- function (x, f, ...) {
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
imap <- function (lst, f, ...) {
  if (!length(lst)) return(list())
  lst <- as.list(lst)
  
  if (is.null(names(lst))) {
    nms <- seq_along(lst)
  }
  else {
    nms <- names(lst)
  }
  
  ans <- mapply(value = lst, name = nms, function (value, name) f(value, name, ...),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
  names(ans) <- nms
  ans
}
