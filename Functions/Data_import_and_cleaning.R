
# This script is used to import and clean data from the nanopore sequencing. The data is used to evaluate the quality of the sequencing, the assembly and the annotation of the genome.
# Path links to data

path_QC <- "D:/BT-7-1_results/Results2.0/results_final/results/"
path_prokka <- "D:/BT-7-1_results/Results2.0/Prokka_results/Prokka_annotation/"
path_quast <- "D:/BT-7-1_results/Results2.0/Quast_results/Quast_report/"


#------ Sekvensing data performens--------

  # Reads

  # Loads and cleans reads quality data from nanopore sequencing
  Reads_quality_func <- function(path){
  path_QC2 <- path
  
  # Inhetning af filer i en liste
  files <- list.files(path = path_QC2,  
                      pattern = "NanoStats.txt$", # Pattern = kigger efter filer det hedder transposed_report.tsv
                      recursive = TRUE,  # Recursive før den kigger i alle mapper der kommer efter stiens lokation
                      full.names = TRUE) # full.names gør det tage hele sti navnet med. 
  
  data_QC2 <- data.frame()
  for(i in 1:length(files)){
    
    # Indlæsning af tsv filer til en dataframe 
    QC_2_data <- map_dfr(files[i],~read_delim(., delim = ":",col_names = F)) %>%  # læser filer fra en liste = files, funktionen  = read_tsv
      pivot_wider(names_from = X1, values_from = X2) %>% 
      mutate(file_name = files[i], #Sætter file stien ind i dataframe 
             Assembly = str_extract(file_name, pattern = "barcode\\d+"),
             QC_number = str_extract(file_name, pattern = "QC\\d+"))
    
    data_QC2 <- bind_rows(data_QC2, QC_2_data)
  }
  
  data_QC2_clean <- data_QC2 %>%
    rename_all(~ gsub(" ", "_", .)) %>% 
    select(Assembly, QC_number,Number_of_reads, Total_bases, Read_length_N50, Mean_read_quality) %>%
    mutate(Number_of_reads = as.numeric( str_remove_all(str_remove_all(as.character(unlist(Number_of_reads)), pattern = " "), pattern = ",")),
           Total_bases = as.numeric( str_remove_all(str_remove_all(as.character(unlist(Total_bases)), pattern = " "), pattern = ",")),
           Read_length_N50 = as.numeric( str_remove_all(str_remove_all(as.character(unlist(Read_length_N50)), pattern = " "), pattern = ",")),
           Mean_read_quality = as.numeric( str_remove_all(str_remove_all(as.character(unlist(Mean_read_quality)), pattern = " "), pattern = ","))) %>%
    mutate(QC_name = ifelse(str_detect(QC_number, pattern ="QC1")==TRUE, "Read quality from nanopore sequnecing", QC_number),
           QC_name = ifelse(str_detect(QC_number, pattern ="QC2")==TRUE, "Read quality after filtring", QC_name))
  
  write.csv(data_QC2_clean, "data/QC_data.csv")
}
  #TODO: Fiks the function so it can be used in the pipeline (Data saving and recovery from multipul groups)

  # Check if the file already exists otherwise run the function
  if(file.exists("data/QC_data.csv") == FALSE){
  Reads_quality_func(path = path_QC)
  }


#---- Assembly data ----

# Loads and cleans assembly data from nanopore sequencing

  if(file.exists("data/Quality_assembly.csv") == FALSE){
  #TODO: Insert function for assembly data fra quast
  }


#---- Annotation data ----

  # Loads and cleans annotation data from nanopore sequencing
  Annotation_data_func <- function(path){
  
  
  file_prokka_tsv <- list.files(path,  pattern = "prokka.tsv$", recursive = TRUE, full.names = TRUE)
  
  data_total <- data.frame()
  
  
  for (i in 1:length(file_prokka_tsv)){
    data_prokka <- read_tsv(file_prokka_tsv[i]) %>% 
      mutate(Assembly = str_extract(file_prokka_tsv[i], pattern = "barcode\\d+"))
    
    data_prokka_group <- data_prokka %>%
      mutate(Type = ifelse(str_detect(product, pattern = "S ribosomal RNA") == TRUE, "Ribosomal RNA", NA),
             Type = ifelse(str_detect(product, pattern = "tRNA-[a-zA-Z]{3}\\([a-zA-Z]{3}\\)") == TRUE, "tRNA", Type),
             Type = ifelse(str_detect(product, pattern = "hypothetical protein") == TRUE, "Hypothetical protein", Type),
             Type = ifelse(is.na(Type) == TRUE , "Annotated protein", Type)) %>% 
      group_by(Type, Assembly) %>%
      summarise(Number = n()) %>% 
      ungroup() %>% 
      mutate(total = sum(Number)) %>%
      mutate(Percentage = (Number/total)*100)
    
    data_total <- bind_rows(data_total, data_prokka_group)
  }
  write.csv(data_total, "data/Annotation_data.csv")
}
  if(file.exists("data/Annotation_data.csv") == FALSE){
  Annotation_data_func(path = path_prokka)
  
  }


#---- Antismash data ----

  # Loads and cleans antismash data from nanopore sequencing
  Antismash_data_func <- function(path){
    print("Lav databehandling funktion MADS!")
  }

#  if(file.exists("data/Antismash_data.csv") == FALSE){
#  Antismash_data_func(path = )
#  }










