# BSD_2_clause

get_css <- function(txt) {
  h1 <- read_html(txt)
  h2 <- html_text(html_nodes(h1, "style"))
  h3 <- remove_at_import(h2)
  h4 <- wrap_css(h3)
  h5 <- add_styles(h4)
  return(h5)
}

remove_at_import <- function(css) {
  cln <- str_replace(css, pattern = "[ ]{0,2}@import url\\(.*\\);", "")
  return(cln)
}

wrap_css <- function(css) {
  cln <- str_replace_all(css, pattern = "\\{", " {\n    ")
  cln <- str_replace_all(cln, pattern = "\\}", "\n}\n\n")
  cln <- str_replace_all(cln, pattern = ";", ";\n    ")
  return(cln)
}

add_styles <- function(css) {
  more_style <- '
  /* Override defaults for attrs */
  li {
  color: #000000;
  font-size: large;
  font-family: "Garamond";
  padding-left: 20px;
  }

  ol {
  padding-left: 40px;
  }

  p {
  margin: 0;
  color: #000000;
  font-size: large;
  font-family: "Garamond";
  padding-bottom: 10pt;
  }

  span {
  margin: 0;
  color: #000000;
  font-size: inherit;
  font-family: "Garamond";
  padding-bottom: 10pt;
  }

  h1 {
  padding: 0;
  color: #000000;
  font-weight: 700;
  font-size: 20pt;
  font-family: "Open Sans";
  line-height: 1.2;
  text-align: left
  }

  h2 {
  padding-top: 0pt;
  color: #000000;
  font-weight: 700;
  font-size: 16pt;
  font-family: "Open Sans";
  line-height: 1.2;
  font-style: italic;
  text-align: left
  }

  h3 {
  padding-top: 12pt;
  color: #000000;
  text-decoration: underline;
  font-size: 12pt;
  padding-bottom: 2pt;
  font-family: "Garamond";
  line-height: 1.2;
  font-style: italic;
  text-align: left
  }

  h4 {
  padding-top: 12pt;
  color: #000000;
  text-decoration: underline;
  font-size: 10pt;
  padding-bottom: 2pt;
  font-family: "Open Sans";
  line-height: 1.2;
  font-style: italic;
  text-align: left
  }

  h5 {
  padding-top: 0pt;
  color: #000000;
  font-size: 10pt;
  padding-bottom: 10pt;
  font-family: "Source Code Pro";
  line-height: 1.2;
  text-align: left
  }

  h6 {
  padding-top: 0pt;
  color: #000000;
  font-size: 12pt;
  padding-bottom: 10pt;
  font-family: "Garamond";
  line-height: 1.2;
  text-align: left
  }
  '
  return(paste(css, more_style, sep = "\n"))
}
