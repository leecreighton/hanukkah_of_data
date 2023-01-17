library(tidyverse)
library(lubridate)
orders <- read_csv("noahs-orders.csv")
items <- read_csv("noahs-orders_items.csv")
customers <- read_csv("noahs-customers.csv")
products <- read_csv("noahs-products.csv")

the_answer <- customers |> 
  # Add orders data to customer info
  inner_join(orders, by = "customerid", suffix = c("", ".y")) |>
  select(-ends_with(".y")) |>  # delete duplicate columns
  # Add item SKUs for each order 
  inner_join(items, by = "orderid") |>
  # Add description of each sku
  inner_join(products, by = "sku") |>
  # Where is the for-sale price larger than wholesale price?
  mutate(price_difference = unit_price - wholesale_cost) |> 
  mutate(percent_difference = price_difference / wholesale_cost) |> 
  filter(price_difference < 0) |> 
  arrange(percent_difference) |>
  # only one person buys the most drastic sales:
  slice_head()