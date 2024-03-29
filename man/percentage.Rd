% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percentage.R
\name{percentage}
\alias{percentage}
\alias{as.percentage}
\alias{is.percentage}
\alias{print.percentage}
\alias{format.percentage}
\title{Transform to percentage}
\usage{
as.percentage(x, ...)

is.percentage(x)

\method{print}{percentage}(x, ...)

\method{format}{percentage}(x, digits = NULL, ...)

percentage(x, digits = NULL, ...)
}
\arguments{
\item{x}{input}

\item{...}{other parameters passed on to methods}

\item{digits}{how many digits should be printed. It defaults to printing all decimals available in the data after transforming to a percentage, with a minimum of 0 and a maximum of 3.}
}
\description{
Transform input to a percentage. The actual values are numeric, but will be printed as formatted percentages.
}
\details{
Printing percentages will always have a percentage symbol and is never written in scientific format (like 2.5e+04\%).

The function \code{percentage} is a wrapper around \code{format(as.percentage(...))} with automatic determination of the number of digits, varying between 0 and 1. It also, unlike \R, rounds according to basic math rules: \code{percentage(0.4455)} returns \code{"44.6\%"} and not \code{"44.5\%"}. This function always returns a character, and can also be used in plotting, see Examples.
}
\examples{
proportion <- as.percentage(c(0.25, 2.5, 0.0025))
proportion
sum(proportion)
max(proportion)
mean(proportion)

as.percentage(2.5e-14)

as.percentage(pi)
format(as.percentage(pi))
format(as.percentage(pi), digits = 6)

round(0.4455 * 100, 1) # mind the rounding
percentage(0.4455) # does not round to 44.5\%

if (require("ggplot2")) {
  ggplot(iris) +
    geom_col(aes(Species, Sepal.Length / sum(Sepal.Length)),
             position = "stack") +
    # add percentage as function to the labels:
    scale_y_continuous(labels = percentage)
}
}
