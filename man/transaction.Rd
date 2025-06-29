% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{transaction}
\alias{transaction}
\title{Weekly Retail Sales Dataset}
\format{
A \code{tbl_ts} (tsibble) with 3,909 distinct time series and 13 variables:
\itemize{
\item \code{week} A year-week object (\code{week}). Represents the start of each sales week (e.g., "2009 W03").
\item \code{week_end_date} POSIXct. Date marking the end of each sales week.
\item \code{store_id} Character. Identifier for the store.
\item \code{upc_id} Character. Identifier for the product (UPC).
\item \code{units} Numeric. Number of units sold during the week.
\item \code{visits} Numeric. Number of shopper visits that included the product.
\item \code{hhs} Numeric. Number of distinct households that purchased the product.
\item \code{spend} Numeric. Total spend ($) on the product.
\item \code{price} Numeric. Price per unit paid by customers.
\item \code{base_price} Numeric. Regular (non-promotional) price.
\item \code{feature} Numeric. Whether the product was featured in advertising (1 = featured, 0 = not featured).
\item \code{display} Numeric. Whether the product was on display in-store (1 = yes, 0 = no).
\item \code{tpr_only} Numeric. Temporary Price Reduction without display or feature (1 = yes, 0 = no).
}
}
\source{
sourcefiles@dunnhumby.com
}
\usage{
transaction
}
\description{
A weekly time series dataset of product-level sales across multiple stores.
This dataset includes unit sales, visits, and promotional attributes over time.
}
\details{
This dataset is a \code{tsibble} (time-aware tibble), indexed by \code{week}, with keys: \code{store_id}, \code{upc_id}
These keys define unique time series for each product-store combination.
}
\keyword{datasets}
