#' Working with names.
#' 
#' @export
#' @rdname names
with_names <- function (lst, names) {
  stopifnot(identical(length(lst), length(names)))
  names(lst) <- names
  lst
}

#' @export
#' @rdname names
all_named <- function (lst) {
  nms <- names(lst)
  if (is.null(nms)) return(with_names(lst, rep("", length(lst))))
  lst
}

#' @export
#' @rdname names
is_all_named <- function (x) {
  all(names(x) != "")
}
