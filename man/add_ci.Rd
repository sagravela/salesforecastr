% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{add_ci}
\alias{add_ci}
\title{Helper function to add CI to fables}
\usage{
add_ci(fbl, target)
}
\arguments{
\item{fbl}{A \code{fable} object with forecasts.}

\item{target}{A \code{character} indicating the target variable.}
}
\value{
A \code{tsibble} with lower and upper bounds.
}
\description{
Adds 80\% and 95\% CI bounds to a fable object.
}
