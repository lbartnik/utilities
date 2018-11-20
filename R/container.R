#' Abstract container.
#'
#' Used to store artifacts or commits.
#'
#' @param x object to be converted to a `container` or tested.
#'
#' @export
#' @rdname container
is_container <- function (x) inherits(x, 'container')

#' @export
#' @rdname container
as_container <- function (x) {
  stopifnot(is.list(x))
  structure(x, class = 'container')
}

#' @param n print that many elements
#' @inheritDotParams base::print
#'
#' @export
#' @rdname container
print.container <- function (x, n = 3, ...) {
  stopifnot(is.numeric(n))
  n <- max(0, min(n, length(x)))

  if (n > 0) {
    ccat0(default = 'grey', '# A container of ', length(x), ' element', if(n>1)'s', '\n')
    lapply(x[seq(n-1)], function(y) { print(y); cat('\n') })
    print(nth(x, n))
    if (length(x) > n) {
      ccat0(default = 'grey', '# ... with ', length(x)-n, ' more element', if(n>1)'s', '\n')
    }
  } else {
    cat0(toString(x), '\n')
  }

  invisible(x)
}

#' @export
#' @rdname container
toString.container <- function (x, ...) {
  n <- length(x)
  c <- table(map_chr(x, function(y)first(class(y))))
  nc <- length(c)

  paste0('<container of ',
         n, if(n>1 && nc>1)'various ', ' element', if(n != 1)'s',
         if(n>1 && nc==1)paste0(' of class ', names(c)),
         '>')
}
