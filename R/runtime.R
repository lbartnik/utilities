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
#' @importFrom rlang abort enquo quo_get_expr is_symbol is_character
#' @importFrom glue glue
#' @rdname runtime
#' @export
#' @examples
#' \dontrun{
#'   try_load(dplyr)
#'   try_load("dplyr")
#' }
try_load <- function (package) {
  package <- enquo(package)

  name <- quo_get_expr(package)
  if (is_symbol(name)) name <- as.character(name)
  if (!is_character(name)) {
    abort(glue("cannot load package"))
  }

  as.logical(suppressWarnings(requireNamespace(name, quietly = TRUE)))
}


#' @description `is_rstudio` returns `TRUE` if is called inside RStudio.
#'
#' @export
#' @rdname runtime
is_rstudio <- function () {
  requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()
}
