% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_with_sigmoid.R
\name{scale_with_sigmoid}
\alias{scale_with_sigmoid}
\title{Create sigmoid relationship}
\usage{
scale_with_sigmoid(x, k, x0)
}
\arguments{
\item{x}{Numeric or vector of numeric input values.}

\item{k}{Numeric. Shape parameter dictating 'steepness' of curve (higher k => steeper).}

\item{x0}{Numeric. Location of midpoint of curve.}
}
\value{
Vector of numbers between 0 and 1 corresponding to sigmoid curve.
}
\description{
Scales continuous input to between 0 and 1 according to a sigmoidal relationship.
}
\examples{
x <- seq(0, 100, length=100)
scale_with_sigmoid(log10(x), 10^(-(log10(log10(max(x)))-1)), log10(max(x))/2)
}
