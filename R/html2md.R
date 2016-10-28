# BSD_2_clause

#' Use \code{pandoc} to convert HTML to markdown
#'
#' @param html The HTML to be searched and sub'd
#' @return An updated form of \code{html}
#' @export
html2md <- function(html, mdf) {
  tmp <- gsub(mdf, pattern = ".md", ".html", fixed = TRUE)
  writeLines(html, tmp)
  cmd <- paste0("pandoc ", tmp, " -o ", mdf, " --wrap=none")
  system(cmd, intern = FALSE)
  res <- paste(readLines(mdf), collapse = "\n")
  return(res)
}
