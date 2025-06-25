# Sales Time Series Analysis and Forecasting with R

Forecasted over 3,500 time series for various products across multiple stores, applying advanced forecasting models such as *ARIMA*, *STL+ETS*, *NNETAR* and *Prophet*. Forecasted data was integrated into an interactive dashboard, enabling real-time visualization and data-driven decision-making for enhanced inventory and sales strategy.  
  
 ![Sales Time Series Dashboard](assets/salesforecast.gif)

## Getting Started
Shiny App Demo can be run without any setup, click on [shinyapp.io](https://sagravela.shinyapps.io/salesforecastr/).  
The entire pipeline is available as an R package, `salesforecastr`, which can be used to run the forecasting and validation process easily.
To run it locally, build with:
```{r}
devtools::install_github("sagravela/salesforecastr")
```
Then the pipeline can be run with `dh_forecasting()` and `dh_validation()` functions.  
> For more details, check the documentation package:
> ```{r}
> help(package = "salesforecastr")
> ```


## Dataset Overview: Breakfast at the Frat, a Time Series Analysis
Breakfast at the Frat contains a representation of sales and 
promotion information on five products from three brands 
within four categories (mouthwash, pretzels, frozen pizza, and 
boxed cereal), over 156 weeks. Included in this Source File:
Unit sales, households, visits, and spend data by product, 
store, and week.  
- Base Price and Actual Shelf Price, to determine a product’s 
discount, if any.
- Promotional support details (e.g., sale tag, in-store 
display), if applicable for the given product/store/week.
- Store information, including size and location, as well as a 
price tier designation (e.g., upscale vs. value).
- Product information, including UPC, size, and description.

## Tables
![dataset_details](/assets/data_table.png)

## transactions
**Description**: This table contains 156 weeks of Mouthwash, 
Pretzels, Frozen Pizza and Boxed Cereal transactions, at the 
product level by store, by week.  
**Number of records**: 524,950.  
**Features details**:  
`base_price`: Base price of item.  
`display`: Indicator of whether product was part of in-stroe display. 1 if on display, 0 if not.  
`feature`: Indicator of whether product was in in-store circular. 1 if in the circular, 0 if not.  
`hhs`: Number of purchasing households.  
`price`: Actual amount charged for the product at shelf.  
`spend`: Total spend (ie. dollar sales).  
`store_num`: Unique store identifier.  
`tpr_only`: Temporary price reduction only (ie. shelf tag only, product was reduced in price but not on display or in an advertisement). 1 if on temporary price reduction only, 0 if not.  
`units`: Units sold.  
`visits`: Number of unique purchases (baskets) that included the product.  
`week_end_date`: Week ending date.  
`upc`: Standard unique product code.  

## products
**Description**: Provides detailed product information for each upc in **transactions**.   
**Number of records**: 58.  
**Features details**:  
`category`: Category of the product.  
`description`: Product description.  
`manufacturer`: Manufacturer.  
`product_size`: Package size or quantity of the product.  
`sub_category`: Sub-category of the product.  
`upc`: Standard unique product code.  

## store
**Description**: Provides detailed store information for each store in **transactions**.  
**Number of records**: 79.  
**Features details**:  
`address_state_prov_code`: State.  
`parking_space_qty`: Number of parking spaces in the parking lot.  
`sales_area_size_num`: Square footage of store.  
`seg_value_name`: Designated store value segment. Possible values: Value, Mainstream and 
Upscale.  
`store_num`: unique store identifier.  

## CONTACT INFORMATION
For general questions about dunnhumby or the Source Files 
programme, or for technical questions regarding the use of this 
dataset, please contact:
**sourcefiles@dunnhumby.com**
