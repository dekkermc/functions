if(!require("pacman")){install.packages("pacman")}

pacman::p_load(tidyverse)


#get all non numeric characters
get_non_numeric <- function(input_date){
  
  input_date %>%
  str_extract_all(pattern = "[^0-9]") %>%
    unlist() %>%
    table() %>%
    return
  
}



get_character_lengths <- function(input_date){
  
  input_date %>%
    map(nchar) %>%
    unlist() %>%
    table() %>%
    return()
  
}


#still need to incorporate these into a proper date identification function