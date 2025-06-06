#' Store Information Dataset
#'
#' A dataset containing details for 77 retail stores, including location, sales performance, and customer segmentation.
#'
#' @format A tibble with 77 stores and 9 variables:
#' \describe{
#'   \item{store_id}{Character. Unique identifier for each store.}
#'   \item{store_name}{Character. Name of the store.}
#'   \item{address_city_name}{Character. City where the store is located.}
#'   \item{address_state_prov_code}{Character. Two-letter state or province code.}
#'   \item{msa_code}{Numeric. Metropolitan statistical area code.}
#'   \item{seg_value_name}{Character. Customer segmentation category (e.g., "MAINSTREAM", "UPSCALE", "VALUE").}
#'   \item{parking_space_qty}{Numeric. Number of parking spaces (may contain NAs).}
#'   \item{sales_area_size_num}{Numeric. Size of the sales floor area in square feet.}
#'   \item{avg_weekly_baskets}{Numeric. Average number of baskets (transactions) per week.}
#' }
#'
#' @source sourcefiles@dunnhumby.com
"store"


#' Product Information Dataset
#'
#' A dataset containing details for 58 retail products, including category, size, manufacturer, and volume or weight.
#'
#' @format A tibble with 58 produtcts and 8 variables:
#' \describe{
#'   \item{upc_id}{Character. Universal Product Code (UPC) identifier.}
#'   \item{description}{Character. Product description.}
#'   \item{manufacturer}{Character. Name of the product manufacturer.}
#'   \item{category}{Character. Product category (e.g., "BAG SNACKS", "ORAL HYGIENE PRODUCTS").}
#'   \item{sub_category}{Character. More specific product classification.}
#'   \item{product_size}{Character. Product size as indicated on the packaging (e.g., "15 OZ", "1.5 LT").}
#'   \item{volume_ml}{Numeric. Product volume in milliliters (may be NA if not applicable).}
#'   \item{mass_oz}{Numeric. Product mass in ounces (may be NA if not applicable).}
#' }
#'
#' @source sourcefiles@dunnhumby.com
"product"


#' Weekly Retail Sales Dataset
#'
#' A weekly time series dataset of product-level sales across multiple stores.
#' This dataset includes unit sales, visits, and promotional attributes over time.
#'
#' @format A `tbl_ts` (tsibble) with 3,909 distinct time series and 13 variables:
#' \describe{
#'   \item{week}{A year-week object (`week`). Represents the start of each sales week (e.g., "2009 W03").}
#'   \item{week_end_date}{POSIXct. Date marking the end of each sales week.}
#'   \item{store_id}{Character. Identifier for the store.}
#'   \item{upc_id}{Character. Identifier for the product (UPC).}
#'   \item{units}{Numeric. Number of units sold during the week.}
#'   \item{visits}{Numeric. Number of shopper visits that included the product.}
#'   \item{hhs}{Numeric. Number of distinct households that purchased the product.}
#'   \item{spend}{Numeric. Total spend ($) on the product.}
#'   \item{price}{Numeric. Price per unit paid by customers.}
#'   \item{base_price}{Numeric. Regular (non-promotional) price.}
#'   \item{feature}{Numeric. Whether the product was featured in advertising (1 = featured, 0 = not featured).}
#'   \item{display}{Numeric. Whether the product was on display in-store (1 = yes, 0 = no).}
#'   \item{tpr_only}{Numeric. Temporary Price Reduction without display or feature (1 = yes, 0 = no).}
#' }
#'
#' @details
#' This dataset is a `tsibble` (time-aware tibble), indexed by `week`, with keys:
#' \itemize{
#'   \item \code{store_id}
#'   \item \code{upc_id}
#' }
#' These keys define unique time series for each product-store combination.
#'
#' @source sourcefiles@dunnhumby.com
"transaction"
