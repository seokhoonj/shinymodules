% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sliderPeriod.R
\name{dynSliderPeriodServer}
\alias{dynSliderPeriodServer}
\title{Dynamic Slider Period Input Control Server}
\usage{
dynSliderPeriodServer(
  id,
  data,
  column,
  selected = NULL,
  reverse = FALSE,
  fun = function(x) format(x, "\%y-\%m"),
  invfun = function(x) as.Date(paste0(x, "-01"), "\%y-\%m-\%d")
)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function.}

\item{data}{A reactive data}

\item{column}{A data column for unique list of values}

\item{selected}{The initially selected value.}

\item{reverse}{A boolean value whether to reverse the choices or not}

\item{fun}{An expression function for period.}

\item{invfun}{An inverse function for original value of period.}
}
\value{
The return value, if any, from executing the module server function
}
\description{
Create a dynamic slider period input control server.
}
\seealso{
\code{\link[=sliderPeriodUI]{sliderPeriodUI()}}
}
