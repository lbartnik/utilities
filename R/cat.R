#' Colored cat.
#'
#' @importFrom stringi stri_paste
#'
#' @seealso colourize
#'
#' @param ... Strings to be printed; optionally named with color names.
#' @param sep Separator.
#' @param default Default color; `"default"` means no color.
#'
#' @export
#' @rdname cpaste
cpaste <- function (..., sep = ' ', default = "default")
{
  cat_chunk <- function (color, chunk, sep) {
    if (is_empty(chunk)) return('')

    if (identical(color, 'default') || identical(color, '')) {
      color <- default
    } else {
      color <- get_color(color)
    }
    stri_paste(color(chunk), sep, sep = '')
  }

  grey_style <- crayon::make_style(grDevices::grey(.6), grey = TRUE)
  grey <- function(...) crayon::style(paste0(...), grey_style)

  get_color <- function (color) {
    if (identical(color, "grey")) return(grey)
    get(color, envir = asNamespace("crayon"), inherits = FALSE)
  }

  default <- if (identical(default, 'default')) as.character else get_color(default)

  chunks <- list(...)
  chunks <- chunks[!map_lgl(chunks, is.null)]
  chunks <- lapply(chunks, stri_paste, collapse = sep)

  if (!length(names(chunks))) names(chunks) <- rep("", length(chunks))

  chunks <- unlist(Map(cat_chunk, names(chunks), chunks, c(rep(sep, length(chunks)-1), '')))
  stri_paste(chunks, collapse = '')
}

#' Colored cat().
#'
#' @param ... Strings passed to [cpaste()].
#' @param sep Separator.
#' @param default Default color; `"default"` means no special color.
#'
#' @export
#' @rdname ccat
cat0 <- function (..., sep = '') cat(..., sep = sep)

#' @export
#' @rdname ccat
ccat <- function (..., sep = ' ', default = "default") cat(cpaste(..., sep = sep, default = default))

#' @export
#' @rdname ccat
ccat0 <- function (..., default = "default") ccat(..., sep = '', default = default)

#' @importFrom rlang inform
#' @export
#' @rdname ccat
cinform <- function (..., sep = ' ', default = "default") inform(cpaste(..., sep = sep, default = default))

#' @export
#' @rdname ccat
cinform0 <- function (..., default = "default") cinform(..., sep = '', default = default)



#' Add color to text.
#'
#' Transforms the input `text` by adding colors to substrings specified
#' in `...`.
#'
#' @param text character string to amend.
#' @param ... substrings with colors assigned as names.
#' @param default default color.
#'
#' @return `text` with ANSI control sequences around colorized substrings.
#'
#' @importFrom rlang is_character
#'
#' @rdname colorize
#' @export
#'
#' @examples
#' colorize("text with words", yellow = "with")
#' colorize("longer text with many words", yellow = "text", red = "many")
colorize <- function (text, ..., default = 'default') colorize_(text, list(...), default)


#' @param repl named vector or list; names are colors, values are substrings.
#' @rdname colorize
#' @export
colorize_ <- function (text, repl, default = 'default') {
  stopifnot(all(map_lgl(repl, is_character)))

  pieces <- break_text(text, as.character(repl))

  colourized <- map_chr(pieces, function (piece) {
    i <- match(piece, repl, nomatch = 0L)
    c <- if (identical(i, 0L)) default else names(repl)[i]
    cpaste(piece, default = c)
  })

  paste0(colourized, collapse = '')
}


#' @importFrom stringi stri_split_fixed stri_detect_fixed
#' @importFrom utils tail
break_text <- function (text, by) {
  stopifnot(identical(unique(by), by))

  for (split_with in by) {
    text <- map_chr(text, function (piece) {
      if (!any(stri_detect_fixed(piece, by))) return(piece)
      pieces <- unlist(stri_split_fixed(piece, split_with))
      pieces <- Filter(nchar, pieces)
      if (!length(pieces)) return(piece)
      c(map_chr(head(pieces, -1), function(x) c(x, split_with)), tail(pieces, 1))
    })
  }

  text
}
