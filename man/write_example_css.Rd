% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/styling.R
\name{write_example_css}
\alias{write_example_css}
\title{Write 'new_styles' CSS to file}
\usage{
write_example_css(path)
}
\arguments{
\item{path}{The path to which the CSS will be written}
}
\value{
None
}
\description{
The internal data 'new_styles' includes formatting for the major HTML
attributes, e.g., p, h1-6, li, ol. This data may be written to file, modified,
and then included in, e.g., \link{add_default_yaml}.
}
\seealso{
\link{write_bootstrap_css}
}

