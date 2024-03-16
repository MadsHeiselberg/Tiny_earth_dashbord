# Library load
library(readr)
library(jsonlite)
#install.packages("tidyjson")
library(tidyjson)
library(dplyr)
library(tidyverse)
library(tidyr)


#------ Dataprocessing ------------------------------------------

  #----- Antismash data --------------------------------
  data_anti <- read_json("E:/BT-7-1_results/Results2.0/AntiSMASH/Antismash_results_PAQ87969.barcode39/assembly.json")
    
  data <-fromJSON("E:/BT-7-1_results/Results2.0/AntiSMASH/Antismash_results_PAQ87969.barcode39/assembly.json")
  
    data_areas <- data %>%  enter_object(areas) %>% gather_array() #virker
    data_areas %>% json_types
    #Virker, mangler dog lige at fin pusses
    data_areas_all <- data %>%  enter_object(areas) %>%  gather_array() %>%  unnest(..JSON) %>% unnest_auto(protoclusters) %>%unnest_auto(candidates) %>% unnest_auto(products)
    
    ###
    
    get_qbr_data <- function(row_n) {
      #player_stats <- data_anti %>% 
      #  purrr::pluck("athletes", row_n, "categories", 1, "totals") %>% 
     #  # assign names from category
     #   set_names(nm = category_names)
      
      player_nm <- raw_json %>% 
        purrr::pluck("records", row_n, "records", ) %>% 
      
    #  headshot <- raw_json %>% 
    #    purrr::pluck("athletes", row_n, "athlete", "headshot", "href")
      
      #output named list
      c(player_stats)
    }
    
    # test the function
    get_qbr_data(1)
    
    new <- data_anti %>% purrr::pluck("records", 1, "modules" ) %>% spread_all()
    new2 <- data_anti %>% purrr::pluck("records", 1, "modules",1,"" ) %>% 
    