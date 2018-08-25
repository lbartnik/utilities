#' Colored cat.
#' 
#' @importFrom stringi stri_paste
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

