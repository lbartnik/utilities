#' Operations on lists.
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

#' @export
#' @rdname lists
join <- function (x, sep) {
  paste(x, collapse = sep)
}
