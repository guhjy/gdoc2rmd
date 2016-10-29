# BSD_2_clause

#' Add rmarkdown YAML front-matter to markdown doc
#'
#' @details Rmarkdown documents take a YAML header to describe output options.
#' This function adds the basic header:
#'
#' \preformatted{---
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     css: [path/to/css1, path/to/css2]
#' ---}
#'
#' Users may supply any CSS they wish; an example is provided with
#' \link{use_example_css} (and write with \link{write_example_css}. Users may
#' supply their own YAML header, but must #' correctly specify the (absolute)
#' path to the CSS file.
#'
#' @param doc The markdown document to append to YAML
#' @param theme A Bootswatch theme (see \url{http://bootswatch.com}) ["default"]
#' @param styles List of paths to one or more CSS style sheets
#' @param shiny Whether to use the Shiny runtime [FALSE]
#' @param yml Optional user-supplied YAML frontmatter [NULL]
#' @return An Rmarkdown version of \code{doc} with a YAML header
#' @export
add_yaml <- function(doc, theme = "default", styles = list(),
                             shiny = FALSE, yml = NULL) {
  if(is.null(yml)) {
    yml <- sprintf(
"---
output:
  html_document:
    theme: %s
    toc: true
    toc_float: true
    css: [ %s ]
%s
---",
      theme,
      paste(styles, collapse = ", "),
      ifelse(shiny, "runtime: shiny", "")
    )
  }
  doc <- paste(yml, doc, sep = "\n")
  return(doc)
}

