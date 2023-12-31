% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales.R
\name{scalesServer}
\alias{scalesServer}
\title{Scales Control Server}
\usage{
scalesServer(
  id,
  choices = c("fixed", "free_x", "free_y", "free"),
  selected = NULL
)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function.}

\item{choices}{c("fixed", "free_x", "free_y", "free"), a string vector specifying choices}

\item{selected}{NULL, a string vector specifying selected choices}
}
\value{
The return value, if any, from executing the module server function
}
\description{
Create a scales control server.

\preformatted{
## Example
scalesServer("scales")
}
}
\seealso{
\code{\link[=scalesUI]{scalesUI()}}
}
