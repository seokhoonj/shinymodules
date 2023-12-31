% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/action.R
\name{actionUI}
\alias{actionUI}
\title{Action Button UI}
\usage{
actionUI(id, label = "Run")
}
\arguments{
\item{id}{The input slot that will be used to access the value.}

\item{label}{Display label for the control, or NULL for no label.}
}
\value{
An action button UI
}
\description{
Create an action button UI.
}
\details{
\preformatted{
## Example
actionUI("action", label = "Run")
}
}
\seealso{
\code{\link[=actionServer]{actionServer()}}
}
