# BSD_2_clause

#' Update the CSS from GDoc HTML
#'
#' A wrapper to: extract the CSS from a Google Docs HTML export; remove the
#' \@import GDocs contain; wrap the extracted (inline) CSS; and add style
#' revisions
#'
#' @param txt An HTML <head> character vector
#' @param no_import Remove the google \@import statement from CSS? (TRUE)
#' @param wrap_css Wrap CSS for pretty-printing? (TRUE)
#' @param add_style Add new styles with \link{add_styles}? (TRUE)
#' @return CSS character string
#' @seealso \link{update_css}
#' @import xml2 rvest
#' @export
update_css <- function(txt,
                       no_import = TRUE,
                       wrap_css = TRUE,
                       add_style = TRUE) {
  css <- get_css(txt)
  if(no_import) css <- remove_at_import(css)
  if(wrap_css) css <- wrap_css(css)
  if(add_style) css <- add_styles(css)
  return(css)
}

#' Extract the <style> from HTML text
#'
#' @param txt An HTML <head> character vector
#' @return CSS character string
#' @seealso \link{update_css}
#' @import xml2 rvest
#' @export
get_css <- function(txt) {
  h1 <- xml2::read_html(txt)
  css <- rvest::html_text(rvest::html_nodes(h1, "style"))
  return(css)
}

#' Remove google's \@import line from <style>
#'
#' @param css The CSS from which the import is stripped
#' @return The clean CSS
#' @importFrom stringr str_replace
#' @export
remove_at_import <- function(css) {
  cln <- str_replace(css, pattern = "[ ]{0,2}@import url\\(.*\\);", "")
  return(cln)
}

#' Wrap CSS for pretty-printing
#'
#' @param css The (inlined) CSS to be wrapped
#' @return The wrapped CSS
#' @importFrom stringr str_replace_all
#' @export
wrap_css <- function(css) {
  cln <- str_replace_all(css, pattern = "\\{", " {\n    ")
  cln <- str_replace_all(cln, pattern = "\\}", "\n}\n\n")
  cln <- str_replace_all(cln, pattern = ";", ";\n    ")
  return(cln)
}

#' Append new styles to CSS
#'
#' The default new styles are internal data (new_styles in R/sysdata.rda), but
#' \code{new_css} can be specified for users to append their own CSS.
#'
#' @param css The CSS to which new_styles or new_css will be added
#' @return Updated \code{css} with new styles appended
#' @seealso if any see alsos
#' @export
add_styles <- function(css, new_css = NULL) {
  if(is.null(new_css)) {
    return(paste(css, new_styles, sep = "\n"))
  } else {
    return(paste(css, new_css, sep = "\n"))
  }
}

#' Use 'new_styles.css' from system.file
#'
#' The internal data 'new_styles' includes formatting for the major HTML
#' attributes, e.g., p, h1-6, li, ol. This data may included in, e.g.,
#' \link{add_default_yaml}.
#'
#' @return Path to the system.file new_styles.css
#' @seealso \link{write_example_css}
#' @export
use_example_css <- function() {
  system.file("extdata", "new_styles.css", package = "gdoc2rmd")
}

#' Write 'new_styles' CSS to file
#'
#' The internal data 'new_styles' includes formatting for the major HTML
#' attributes, e.g., p, h1-6, li, ol. This data may be written to file, modified,
#' and then included in, e.g., \link{add_default_yaml}.
#'
#' @param path The path to which the CSS will be written
#' @return None
#' @seealso \link{write_bootstrap_css}
#' @export
write_example_css <- function(path) {
  style_file <- use_example_css()
  res <- try(file.copy(style_file, path), silent = TRUE)
  if(class(res) != "try-error") {
    message(paste("Example CSS written to", path))
  } else {
    message(paste("Problem writing the example CSS", res))
  }
}
