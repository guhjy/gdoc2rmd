# BSD_2_clause

#' Separate the <head> from the <body> of an html file
#'
#' This function works line-wise to separate the <head> from the <body>,
#' so using, e.g., \code{readLines(infile)} as the input is suitable. Checks
#' if \code{length(doc) == 1} and splits on newlines to create the vector.
#'
#' @param doc An object representation of an html file
#' @return A list with the <head> and <body> of \code{doc}
#' @seealso if any see alsos
#' @importFrom stringr str_split
#' @export
#' @examples
#' \dontrun{
#' decapitate(html_file)
#' }
decapitate <- function(doc) {
  if(length(doc) == 1) {
    doc <- str_split(doc, "\n")
  }
  hst <- grep(doc, pattern = "</head>", fixed = TRUE)
  head <- paste(doc[2:hst], collapse = "\n")
  body <- paste(doc[(hst + 1):(length(doc)-1)], collapse = "\n")
  return(list(head = head, body = body))
}

