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


#' @description `try_load` attempts to load package `package` and returns
#' `TRUE` if succeeded; `FALSE` otherwise.
#'
#' @param package Package name; character or symbol.
#'
#' @importFrom rlang quo quo_text is_symbol as_character
#' @rdname runtime
#' @export
#' @examples
#' \dontrun{
#'   try_load(dplyr)
#'   try_load("dplyr")
#' }
try_load <- function (package) {
  package <- quo(package)

  name <- quo_expr(package)
  if (is_symbol(name)) name <- as_character(name)
  if (!is_character(name)) {
    abort(glue("cannot load package"))
  }

  requireNamespace(name, quietly = TRUE)
}
