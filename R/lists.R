#' Merge lists in order.
#'
#' @param ... Lists to be merged.
#' @return A single, reduced `list`.
#' 
#' @export
#' @rdname lists
combine <- function (...) {
  lsts <- list(...)
  stopifnot(all(vapply(lsts, is.list, logical(1))),
            all(vapply(lsts, is_all_named, logical(1))))

  Reduce(x = lsts, init = list(), function (a, b) {
    c(a, b[setdiff(names(b), names(a))])
  })
}

#' Concatenate multiple strings.
#'
#' @param x Container of strings to concatenate.
#' @param sep Separator.
#'
#' @export
#' @rdname lists
join <- function (x, sep) {
  paste(unlist(x), collapse = sep)
}

