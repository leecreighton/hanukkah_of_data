library(tidyverse)
library(lubridate)

# get customer data
customers <- read_csv("noahs-customers.csv")

# According to https://en.wikipedia.org/wiki/Aries_(astrology),
# Aries ranges between March 21 to April 19
#
# Also, https://en.wikipedia.org/wiki/Dog_(zodiac) shows
# that Year of the Dog births for March 21 to April 19 are in
# 1910, 1922, 1934, 1946, 1958. 1970, 1982, 1994, 2006, and 2018.

# Day of the year for Aries date range
march21 <- yday(as_date("2023-03-21")) # Day  80 of the year
april19 <- yday(as_date("2023-04-19")) # Day 109 of the year

the_answer <- customers |> 
  # get customers born in year of the dog
  filter(year(birthdate) %in% c(1910, 1922, 1934, 1946, 1958, 1970, 1982, 1994, 2006, 2018)) |>
  # get those born in Aries range
  filter(yday(birthdate) > march21 & yday(birthdate) < april19) |> 
  # get those who live "in the neighborhood" of the contractor
  # which Puzzle 2 revealed to be South Ozone Park, NY 11420
  filter(citystatezip == "South Ozone Park, NY 11420")
