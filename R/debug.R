log <- function (level, ...) {
  ccat0("red", '[', level, '] ', ..., '\n')
}

#' Debug utilities.
#' 
#' @export
#' @rdname debug
dbg <- function (...) {
  if (isTRUE(getOption("repository.debug"))) log("DEBUG", ...)
}

#' @export
#' @rdname debug
guard <- function () {
  x <- sys.call(-1)[[1]]
  fname <- if (is.symbol(x)) deparse(x) else '<unnamed>'
  dbg("-> ", fname, '()')
  
  parent <- sys.frame(sys.parent(1))
  expr <- substitute(dbg(x), list(x = paste0('<- ', fname, '()')))
  do.call(on.exit, list(expr = expr, add = TRUE), envir = parent)
  
  invisible()
}

#' @export
#' @rdname debug
stopif <- function (...) {
  i <- which(map_lgl(list(...), function(x)isTRUE(as.logical(x))))
  if (!length(i)) return(invisible(FALSE))
  mc <- match.call()
  lb <- map_chr(mc[i+1], deparse)
  stop('following conditions are true: ', join(lb, ', '), call. = FALSE)
}
