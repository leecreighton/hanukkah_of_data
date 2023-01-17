library(tidyverse)
library(lubridate)
library(glue)
orders <- read_csv("noahs-orders.csv")
items <- read_csv("noahs-orders_items.csv")
customers <- read_csv("noahs-customers.csv")
products <- read_csv("noahs-products.csv")

# see what we can find out about pastries in the products data set.
products |> 
  count(sku, desc)

# looks like all the pastries have the same SKU prefix: BKY. So we
# need to get a list of customers that bought pastries early in 
# the morning.

the_customer <- customers |> 
  # add orders data to customer info
  inner_join(orders, by = "customerid", suffix = c("", ".y")) |>
  select(-ends_with(".y")) |>  # delete duplicate columns
  # add item SKUs for each order 
  inner_join(items, by = "orderid") |>
  # Add description of each sku
  inner_join(products, by = "sku") |>  
  # get bakery items based on prefix
  filter(str_detect(sku, "^BKY")) |>
  # get the early risers. Earliest dawn is around 5am in NYC
  filter(hour(ordered) >= 4 & hour(ordered) < 5) |>
  # examine a table showing number of visits per person
  count(name) |> 
  arrange(desc(n)) |> 
  # only one person visits a lot; get the name
  slice_head()

the_answer <- customers |> 
  filter(name == as.character(the_customer[1]))
