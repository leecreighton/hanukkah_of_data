library(tidyverse)
library(lubridate)
orders <- read_csv("noahs-orders.csv")
items <- read_csv("noahs-orders_items.csv")
customers <- read_csv("noahs-customers.csv")
products <- read_csv("noahs-products.csv")

# a clue was that she was wearing a noah's sweatshirt. How might
# that be listed in the product database?
products |> 
  filter(str_detect(desc, "Noah's"))
# ...which shows that it's listed as "Noah's Jersey (color)"

# another clue was that she has a large number of old cats. What
# cat products are listed in the product database?
products |> 
  filter(str_detect(desc, "Cat"))
# this leads me to the fact that there are senior cat products:
products |> 
  filter(str_detect(desc, "Senior Cat"))

the_customer <- customers |> 
  # Add orders data to customer info
  inner_join(orders, by = "customerid", suffix = c("", ".y")) |>
  select(-ends_with(".y")) |>  # delete duplicate columns
  # Add item SKUs for each order 
  inner_join(items, by = "orderid") |>
  # Add description of each sku
  inner_join(products, by = "sku") |> 
  filter(str_detect(citystatezip, "Queens Village")) |> 
  filter(str_detect(desc, "Senior Cat")) |>
  # only one person visits a lot; get the name
  count(name) |> 
  arrange(desc(n)) |> 
  slice_head()

the_answer <- customers |> 
  filter(name == as.character(the_customer[1]))
