library(tidyverse)
library(lubridate)
orders <- read_csv("noahs-orders.csv")
items <- read_csv("noahs-orders_items.csv")
customers <- read_csv("noahs-customers.csv")
products <- read_csv("noahs-products.csv")

# Look for products that have bagels or coffee
products |> 
  filter(str_detect(desc, "Bagel"))
products |> 
  filter(str_detect(desc, "Coffee"))

the_answer <- customers |> 
  # Add orders data to customer info
  inner_join(orders, by = "customerid", suffix = c("", ".y")) |>
  select(-ends_with(".y")) |>  # delete duplicate columns
  # Add item SKUs for each order 
  inner_join(items, by = "orderid") |>
  # Add description of each sku
  inner_join(products, by = "sku") |> 
  # Get orders that were ordered in 2017
  filter(year(ordered) == 2017) |>
  # Concatenate all items in each order
  group_by(orderid) |>  
  mutate(whole_order=paste0(desc, collapse=" ")) |> #concatenate order descriptions
  distinct(orderid, .keep_all = TRUE) |> 
  # Get orders involving coffee or bagels
  filter(str_detect(whole_order, "Coffee") & str_detect(whole_order, "Bagel")) |>
  # Get initials from names
  mutate(first_name = word(name, 1),
         last_name = word(name, 2),
         initials = paste0(str_sub(first_name, end = 1), str_sub(last_name, end = 1))) |> 
  # Get rows where initials are JD
  filter(initials == "JD") 
