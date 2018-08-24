#' Runtime checks.
#'
#' @name runtime 
#' @rdname runtime
NULL


#' @description `is_knitr` returns `TRUE` if invoked while knitr is
#' compiling a document.
#' 
#' @rdname runtime
#' @export
is_knitr <- function () getOption("knitr.in.progress", FALSE)
