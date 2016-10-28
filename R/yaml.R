# BSD_2_clause

#' Add rmarkdown YAML front-matter to markdown doc
#'
#' @details Rmarkdown documents require a YAML header to describe output types
#' and options. This function adds the basic header:
#'
#' \code{---
#' output:
#'   html_document:
#'     css: <path/to/css>
#'     toc: true
#'     toc_float: true
#' ---}
#'
#' Users may supply their own YAML header, but must correctly specify the
#' (absolute) path to the CSS file.
#'
#' @param doc The markdown document to append to YAML
#' @param css Path to CSS file to be included during knitting
#' @param yml Optional user-supplied YAML frontmatter (NULL)
#' @return A base rmarkdown version of markdown \code{doc}
#' @export
add_yaml <- function(doc, css = NULL, yml = NULL) {
  if(is.null(yml) & !is.null(css)) {
    yml <- sprintf("---
output:
  html_document:
    css: %s
    toc: true
    toc_float: true
---
", css)
  }
  doc <- paste(yml, doc, sep = "\n")
  return(doc)
}
