% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logScale.R
\name{logScaleServer}
\alias{logScaleServer}
\title{Log Scale Control Server}
\usage{
logScaleServer(id, choices = c("force", "log"), selected = NULL)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function.}

\item{choices}{c("force", "log"), a string vector specifying choices}

\item{selected}{NULL, a string vector specifying selected choices}
}
\value{
The return value, if any, from executing the module server function
}
\description{
Create a log scale control server.

\preformatted{
## Example
logScaleServer("logscale")
}
}
\seealso{
\code{\link[=logScaleUI]{logScaleUI()}}
}
