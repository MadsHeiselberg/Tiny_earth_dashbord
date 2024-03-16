# Library load
library(readr)
library(jsonlite)
#install.packages("tidyjson")
library(tidyjson)
library(dplyr)


#------ Dataprocessing ------------------------------------------

  #----- Antismash data --------------------------------
  data_anti <- read_json("E:/BT-7-1_results/Results2.0/AntiSMASH/Antismash_results_PAQ87969.barcode39/assembly.json")

  data_anti_new <- data_anti %>%  spread_all
  

  