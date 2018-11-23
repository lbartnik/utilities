#' Callbacks for knitr.
#'
#' @description `ansi_handler` converts ANSI color codes to HTML tags;
#' this is the callback for `knitr`, see [ansi_to_html] for the actual
#' implementation.
#'
#' @seealso ansi_to_html
#'
#' @param x Text snippet.
#' @param options options.
#'
#' @export
#' @rdname knitr-hooks
#'
#' @examples
#' \dontrun{
#' knitr::knit_hooks$set(output = ansi_handler)
#' knitr::knit_hooks$set(message = ansi_handler)
#' }
ansi_handler <- function(x, options) {
  paste0(
    "<pre class=\"r-output\"><code>",
    ansi_to_html(x),
    "</code></pre>"
  )
}

#' @description `create_trimming_hook` Creates an output callback
#' function (hook) for `knitr`. When registered, it gets triggered by
#' the `output.lines` option passed in a `knitr`'s code snippet. See this
#' \href{https://stackoverflow.com/questions/23114654/knitr-output-hook-with-an-output-lines-option-that-works-like-echo-26}{Stack Overflow}
#' question for more details.
#'
#' @param output_hook `knitr`'s output hook used to print the
#'        transformed output.
#'
#' @importFrom utils head
#' @export
#' @rdname knitr-hooks
#'
#' @examples
#' \dontrun{
#' output_hook <- knit_hooks$get("output")
#' trimming_hook <- create_trimming_hook(output_hook)
#' knit_hooks$set(output = trimming_hook)
#' }
create_trimming_hook <- function (output_hook) {
  function(x, options) {
    # this is the option this hook reacts to
    output.lines <- options$output.lines

    # if not set, pass the snippet to the original hook
    if (is.null(output.lines)) {
      return(output_hook(x, options))  # pass to default hook
    }

    more <- function (y) cpaste(grey = paste("# ... with", y, "more line(s)"))

    # split into lines
    lines <- unlist(strsplit(x, "\n"))

    # if there is only one number in the setting show that many lines
    # from the start and trim the rest
    if (identical(length(output.lines), 1L)) {
      if (length(lines) > output.lines) {
        lines <- c(head(lines, output.lines), more(length(lines)-output.lines))
      }
    }
    # otherwise, treat the setting as a range specification and show
    # only the lines explicitly pointed to; assumes there are no gaps
    else {
      before <- abs(min(output.lines))
      after  <- abs(max(output.lines))
      lines <- c(if (before>1) more(before-1) else NULL,
                 lines[output.lines],
                 if (length(lines) >= after) more(length(lines) - after) else NULL
      )
    }

    # paste these lines together and pass them to the original hook
    x <- paste(c(lines, ""), collapse = "\n")
    output_hook(x, options)
  }
}


#' Convert ANSI color codes to HTML tags.
#'
#' @param x Text snippet.
#' @return Text snippet with ANSI color codes replaced by HTML tags.
#'
#' @export
ansi_to_html <- function(x) {
  stopifnot(requireNamespace('fansi', quietly = TRUE))
  stopifnot(requireNamespace('htmltools', quietly = TRUE))

  fansi::sgr_to_html(htmltools::htmlEscape(x))
}
