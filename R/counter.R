#' State-full counter object.
#'
#' @param start initial value
#' @return A counter function, each call on which returns a subsequent
#'         number.
#'
#' @rdname counter
#'
#' @export
#' @examples
#' c <- counter()
#' c() # returns 0
#' c() # returns 1
#' c() # returns 2
counter <- function (start = 0) {
  value <- start
  c <- function() {
    value <<- value + 1
    value-1
  }
  structure(c, class = 'counter')
}

#' @inheritParams base::print
#' @inheritDotParams base::print
#'
#' @export
#' @rdname counter
print.counter <- function (x, ...) {
  e <- environment(x)
  cat0('<counter, current value ', e$value, '>\n')
  invisible(x)
}
