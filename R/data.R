#' Store Information Dataset
#'
#' A dataset containing details for 77 retail stores, including location, sales performance, and customer segmentation.
#'
#' @format A tibble with 77 stores and 9 variables:
#'  
#'   * `store_id` Character. Unique identifier for each store.
#'   * `store_name` Character. Name of the store.
#'   * `address_city_name` Character. City where the store is located.
#'   * `address_state_prov_code` Character. Two-letter state or province code.
#'   * `msa_code` Numeric. Metropolitan statistical area code.
#'   * `seg_value_name` Character. Customer segmentation category (e.g., "MAINSTREAM", "UPSCALE", "VALUE").
#'   * `parking_space_qty` Numeric. Number of parking spaces (may contain NAs).
#'   * `sales_area_size_num` Numeric. Size of the sales floor area in square feet.
#'   * `avg_weekly_baskets` Numeric. Average number of baskets (transactions) per week.
#' 
#'
#' @source sourcefiles@dunnhumby.com
"store"


#' Product Information Dataset
#'
#' A dataset containing details for 58 retail products, including category, size, manufacturer, and volume or weight.
#'
#' @format A tibble with 58 produtcts and 8 variables:
#'  
#'   * `upc_id` Character. Universal Product Code (UPC) identifier.
#'   * `description` Character. Product description.
#'   * `manufacturer` Character. Name of the product manufacturer.
#'   * `category` Character. Product category (e.g., "BAG SNACKS", "ORAL HYGIENE PRODUCTS").
#'   * `sub_category` Character. More specific product classification.
#'   * `product_size` Character. Product size as indicated on the packaging (e.g., "15 OZ", "1.5 LT").
#'   * `volume_ml` Numeric. Product volume in milliliters (may be NA if not applicable).
#'   * `mass_oz` Numeric. Product mass in ounces (may be NA if not applicable).
#'
#' @source sourcefiles@dunnhumby.com
"product"


#' Weekly Retail Sales Dataset
#'
#' A weekly time series dataset of product-level sales across multiple stores.
#' This dataset includes unit sales, visits, and promotional attributes over time.
#'
#' @format A `tbl_ts` (tsibble) with 3,909 distinct time series and 13 variables:
#'  
#'   * `week` A year-week object (`week`). Represents the start of each sales week (e.g., "2009 W03").
#'   * `week_end_date` POSIXct. Date marking the end of each sales week.
#'   * `store_id` Character. Identifier for the store.
#'   * `upc_id` Character. Identifier for the product (UPC).
#'   * `units` Numeric. Number of units sold during the week.
#'   * `visits` Numeric. Number of shopper visits that included the product.
#'   * `hhs` Numeric. Number of distinct households that purchased the product.
#'   * `spend` Numeric. Total spend ($) on the product.
#'   * `price` Numeric. Price per unit paid by customers.
#'   * `base_price` Numeric. Regular (non-promotional) price.
#'   * `feature` Numeric. Whether the product was featured in advertising (1 = featured, 0 = not featured).
#'   * `display` Numeric. Whether the product was on display in-store (1 = yes, 0 = no).
#'   * `tpr_only` Numeric. Temporary Price Reduction without display or feature (1 = yes, 0 = no).
#'
#' @details
#' This dataset is a `tsibble` (time-aware tibble), indexed by `week`, with keys: `store_id`, `upc_id`
#' These keys define unique time series for each product-store combination.
#'
#' @source sourcefiles@dunnhumby.com
"transaction"
