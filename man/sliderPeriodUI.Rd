% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sliderPeriod.R
\name{sliderPeriodUI}
\alias{sliderPeriodUI}
\title{Slider Period Input Control UI}
\usage{
sliderPeriodUI(
  id,
  label = "Period",
  choices = c(0, 1),
  selected = c(0, 1),
  hide_min_max = FALSE
)
}
\arguments{
\item{id}{The input slot that will be used to access the value.}

\item{label}{Display label for the control, or NULL for no label.}

\item{choices}{Default c(0, 1), list of values to select from.}

\item{selected}{Default C(0, 1), the initially selected value.}

\item{hide_min_max}{Hides min and max labels.}
}
\value{
A slider period input UI
}
\description{
Create a slider period input ui.
}
\seealso{
\code{\link[=sliderPeriodServer]{sliderPeriodServer()}}, \code{\link[=dynSliderPeriodServer]{dynSliderPeriodServer()}}
}
