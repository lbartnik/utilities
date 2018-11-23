#' Abstract container.
#'
#' Used to store artifacts or commits.
#'
#' @param x object to be converted to a `container` or tested.
#'
#' @seealso [container_sort]
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
#' @param ... arguments passed on to [base::print] or [base::toString]
#'
#' @export
#' @rdname container
print.container <- function (x, n = 3, ...) {
  stopifnot(is.numeric(n))
  n <- max(0, min(n, length(x)))

  if (n > 0) {
    # if pretty-printing makes sense
    if (is_atomic_class(class(first(x))) || has_print(first(x))) {
      ccat0(default = 'grey', '# A container of ', length(x), ' element', if(n>1)'s')

      lapply(x[seq(n)], function(y) { cat('\n'); print(y) })
      if (length(x) > n) {
        ccat0(default = 'grey', '# ... with ', length(x)-n, ' more element', if(n>1)'s', '\n')
      }
    } else {
      ccat(toString(x), grey = "\n# No print() method for element class")
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
         if(n>0 && nc==1)paste0(' of class ', names(c)),
         '>')
}


#' Sort container.
#'
#' @param x a container object; see [as_container].
#' @param decreasing logical; should the sort be decreasing or increasing?
#' @param ... optional; extract these names from each element and use their
#'        values to sort.
#'
#' @export
#' @rdname container-methods
#' @importFrom rlang quos
#'
#' @examples
#' c <- as_container(list(list(time = 2), list(time = 1)))
#' container_sort(c, time)
#'
#' c <- as_container(as.list(3:1))
#' sort(c)
container_sort <- function (x, ..., decreasing = FALSE) {
  nms <- map_chr(quos(...), function(q) as.character(quo_get_expr(q)))
  lapply(nms, function (n) {
    if (!all(map_lgl(x, has_name, name = n))) {
      abort(glue("name {n} not present in all objects"))
    }
    i <- order(unlist(lapply(x, `[[`, i = n)), decreasing = decreasing)
    x <<- x[i]
  })

  as_container(x)
}

#' @export
#' @rdname container-methods
sort.container <- function (x, decreasing = FALSE, ...) container_sort(x, ..., decreasing = decreasing)
