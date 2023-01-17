library(tidyverse)
library(lubridate)
orders <- read_csv("noahs-orders.csv")
items <- read_csv("noahs-orders_items.csv")
customers <- read_csv("noahs-customers.csv")
products <- read_csv("noahs-products.csv")

# Have a look at the products of various colors. Need to
# match a single ( plus one or more non-space characters
# plus a single ) at the end of a word.
products |> 
  filter(str_detect(desc, "\\(\\S+\\)$")) |> View()

# Function to trim the last word of a string
trim_last_word <- function(my_string){
  # split string into words, which results in a list
  split_string <- str_split(my_string, " ")
  # to get to the word list use split_string[[1]]
  # return the list elements except the last pasted together
  return (paste(split_string[[1]][1:length(split_string[[1]]) - 1], collapse = " "))
  
}

customers |> 
  # Add orders data to customer info
  inner_join(orders, by = "customerid", suffix = c("", ".y")) |>
  select(-ends_with(".y")) |>  # delete duplicate columns
  # Add item SKUs for each order 
  inner_join(items, by = "orderid") |>
  # Add description of each sku
  inner_join(products, by = "sku") |> 
  # Select orders happening within one hour of each other
  filter(shipped < ordered + hours(1)) |> 
  # Select orders that contain a colored item, which uses the
  # same regular expression as above 
  filter(str_detect(desc, "\\(\\S+\\)$")) |>
  # Get item descriptions without their color
  rowwise() |> 
  mutate(desc_no_color = trim_last_word(desc)) |> 
  ungroup() |> 
  # Concatenate all items in each order
  group_by(orderid) |>  
  mutate(whole_order=list(desc_no_color)) |>
  View()
