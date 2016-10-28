# BSD_2_clause

#' Insert <div>s in front of <p>s
#'
#' \code{pandoc} converts HTML to Markdown in part by inserting newlines where
#' there are <p> (</p>) elements in HTML. When those <p> elements are refined
#' by a class specification then formatting is lost.
#'
#' To keep the class details, this function inserts a <div> behind each <p>.
#' As a result, HTML such as:
#'
#' \code{<p class="newstyle">This is a p-element.</p>}
#'
#' becomes
#'
#' \code{<p><div class="newstyle">This is a p-element</div></p>},
#'
#' which then becomes
#'
#' \code{\\n<div class="newstyle">This is a p-element</div>\\n}
#'
#' after conversion to markdown and retains the information in class "newstyle".
#'
#' @param html The HTML whose <p>-elements will be converted
#' @return An updated html page with <p><div>...</div></p>
#' @export
#' @examples
#' html <- <p class="newstyle">This is a p-element.</p>
#' pelem_conv(html)
pelem_conv <- function(html) {
  t1 <- stringr::str_replace_all(html, "<p class", "<p><div class")
  t2 <- stringr::str_replace_all(t1, "</p>", "</div></p>")
  return(t2)
}

#' Very specific string sub for our demo
#'
#' @param html The HTML to be searched and sub'd
#' @return An updated form of \code{html}
#' @importFrom stringr str_replace
#' @export
update_gdoc_statement <- function(html) {
  replace <- paste("This page was written with Google Docs and converted",
                   "using the `gdoc2rmd` R package. It")
  new <- str_replace(html, "This Google Doc", replace)
  return(new)
}

#' Upate the GDoc title for proper formatting
#'
#' @param html The HTML to be searched and sub'd
#' @return An updated form of \code{html}
#' @export
fix_title_span <- function(html) {
  splt <- str_split(html, "\n")[[1]]
  offend <- grep(splt, pattern = "class=\"[ ]{0,1}title\"")
  splt[offend + 2] <- gsub(splt[offend + 2], pattern = "span", replacement = "div")
  return(paste(splt, collapse = "\n"))
}

#' Fix HTML escape codes for equals and space in URLs
#'
#' @param html The HTML to be searched and sub'd
#' @return An updated form of \code{html}
#' @export
fix_html_escapes <- function(txt) {
  txt <- str_replace_all(txt, "%3D", "=")
  txt <- str_replace_all(txt, "%2520", "%20")
  return(txt)
}

#' Remove empty heading elements
#'
#' @param html The HTML to be searched and sub'd
#' @return An updated form of \code{html}
#' @export
remove_empties <- function(txt) {
  cln <- str_replace_all(txt, "<h[1-6] class=\".*\" id=\".*\"></h[1-6]>\n", "")
  return(cln)
}

#' Remove extraneous strings from URLs
#'
#' Google adds a bunch of its own modifiers to URLs in GDocs, and these modifiers
#' will break the links. This removes the modifiers.
#'
#' @param html The HTML to be searched and sub'd
#' @return An updated form of \code{html}
#' @export
clean_hrefs <- function(html) {
  cln <- stringr::str_replace_all(html, "https://www.google.com/url\\?q=", "")
  cln <- str_replace_all(cln, "\\&sa=D&ust=.*\\)", ")")
  return(cln)
}

