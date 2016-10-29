# BSD_2_clause

#' Un-escape square brackets
#'
#' Pandoc escapes square brackets in the HTML:md conversion, which makes the
#' Rmarkdown invalid. This function un-escapes the brackets.
#'
#' @param doc The markdown text to be cleaned
#' @return A clean version of doc
#' @export
#' @examples
#' \dontrun{
#' doc <- "This is the issue \\[here\\]"
#' clean_brackets(doc)
#' }
clean_brackets <- function(doc) {
  cln <- stringr::str_replace_all(doc, "\\\\\\[", "[")
  cln <- stringr::str_replace_all(cln, "\\\\\\]", "]")
  return(cln)
}
