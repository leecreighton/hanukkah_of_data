library(tidyverse)

# Functions ---------------------------------------------------------------

# get_number takes a single character as input and returns
# the corresponding number on a telephone dial
get_number <- function(i) {
  i = toupper(i)
  if (i=="A" | i=="B" | i=="C") {
    return("2")
  } else if (i=="D" | i=="E" | i=="F") {
    return("3")
  } else if (i=="G" | i=="H" | i=="I") {
    return("4")
  } else if (i=="J" | i=="K" | i=="L") {
    return("5")
  } else if (i=="M" | i=="N" | i=="O") {
    return("6")
  } else if (i=="P" | i=="Q" | i=="R" | i=="S") {
    return("7")
  } else if (i=="T" | i=="U" | i=="V") {
    return("8")
  } else if (i=="W" | i=="X" | i=="Y" | i=="Z") {
    return("9")
  } else {
    return(NA)
  }
}

# translate_name_to_number() takes a column of strings and returns
# the equivalent phone number
translate_name_to_number <- function(the_column) {
  for (the_string in the_column) {
    name_vector <- strsplit(the_string, "")[[1]]
    built_phone_number <- ""
    for (a_letter in name_vector) {
      built_phone_number <- str_c(built_phone_number, get_number(a_letter))
    }
    return(built_phone_number)
  }
}


# Main Program ------------------------------------------------------------

customers <- read.csv("noahs-customers.csv") 

the_answer <- customers |>
  # 1. Prepare phone numbers: xxx-xxx-xxxx to xxxxxxxxxx
  mutate(phone_number = str_replace_all(phone, "-", ""),
  # 2. Extract last name       
        last_name = word(name, 2)
        ) |> 
  # 3. compute phone number from last names
  rowwise() |> 
  mutate(computed_phone_number = translate_name_to_number(last_name)
  ) |> 
  ungroup() |> 
  #4. Find the answer!
  filter(phone_number == computed_phone_number)
