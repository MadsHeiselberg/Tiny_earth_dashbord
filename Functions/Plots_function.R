# PLOTS function file

# library
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")

# Indlæs pakker
library(tidyverse) 
library(readr) 
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(DT)
library(scales)
library(reshape2)
library(RColorBrewer)
library(heatmaply)



#---- Reads quality data plots----
#QC_data <- read.csv("data/QC_data.csv")
# Plots QC1 results from sequencing data
QC1_plot_func <- function(data){
  data %>%
    filter(QC_number == "QC1") %>% 
    ggplot(aes(x = Number_of_reads, y = Total_bases, colour = Read_length_N50)) +
    #geom_jitter(width = 1, height = 1)+
    geom_point()+
    scale_color_gradient(low ="steelblue", high = "coral1")+
    scale_y_continuous(breaks = seq(0, max(data$Total_bases), 500e+6), labels = scales::label_number(scale = 1e-6))+
    scale_x_continuous(breaks = seq(0, max(data$Number_of_reads), 1e+5), labels = scales::label_number(scale = 1e-5))+
    geom_hline(yintercept = 500e+6, linetype = "dashed", color = "black")+
    annotate("text", x = 0, y = 500e+6, label = "Filtlong setting", vjust = -1, hjust = -4, size = 4
    )+
    labs(x = expression("Number of reads [" * 10^5 * "]"),
         y= "Total number of bases [Mb]",
         color = " ",
         shape = "Qulity check of the reads")+
    theme_light() + 
    theme(legend.text= element_text(size = 10),
          legend.position = "top",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14))+
    guides(fill = guide_colorbar(title.position = "top"))
  
}

# Plots QC2 results from sequencing data after filtlong
QC2_plot_func <- function(data){

  data %>%
    filter(QC_number == "QC2") %>% 
    ggplot(aes(x = Number_of_reads, y = Total_bases, colour = Read_length_N50)) +
    #geom_jitter(width = 0.5, height = 0.8)+
    geom_point()+
    scale_color_gradient(low ="steelblue", high = "coral1")+
    scale_y_continuous(breaks = seq(0, max(data$Total_bases), 100e+6), labels = scales::label_number(scale = 1e-6))+
    scale_x_continuous(breaks = seq(0, max(data$Number_of_reads), 1e+5), labels = scales::label_number(scale = 1e-5))+
    #geom_hline(yintercept = 500e+6, linetype = "dashed", color = "black")+
    #annotate("text", x = 0, y = 500e+6, label = "Filtlong setting", vjust = -1, hjust = -4, size = 4
    #          )+
    labs(x = expression("Number of reads [" * 10^5 * "]"),
         y= "Total number of bases [Mb]",
         color = "",
         shape = "Qulity check of the reads")+
    theme_light() + 
    theme(legend.text  = element_text(size = 10),
          legend.position = "top",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14))
  
}




#---- Assembly data plots----

Contig_function <- function(data){
#Funktion så værdierne står som 2*10^2 og ikke f.eks. 10^4.5
  exp_format <- function(x) {
  no_plus <- gsub("\\+","", scales::scientific_format()(x))
  parse(text=gsub("e", " %*% 10^", no_plus))
}

#Vi laver plottet
ggplot(data, aes(x = `n_contigs_..._0_bp.`, y = Completeness, color = N50)) +
  geom_jitter() +
  scale_color_gradient(name = "N50 value [bp]", low = "blue", high = "red", trans = "log10", breaks = trans_breaks("log10", function(x) 10^x), labels = exp_format) +
  labs(x = "Number of contigs", y = "Completeness [%]")
}

Assebly_quality_plot <- function(data){
  data %>%
    ggplot(aes(x=Quality)) +
    geom_histogram(stat = "count", binwidth = 1, fill = "steelblue")+
    theme_classic()}



#---- Annotation data plots----

Annotation_function <- function(data){
  p1 <-data %>%
    distinct(Assembly, .keep_all = TRUE) %>%
    ggplot(aes(x = reorder(Assembly, total), y = Number))+
    geom_bar(stat = "identity")+
    labs(x = "Assembly", y = "Number of genes")+
    coord_flip()+
    theme_classic()
  
  ggplotly(p1)
}

#Annotation_function(data = read.csv("data/Annotation_data.csv"))

Annotation_type_function <- function(data, Assembly_name = "barcode09"){
  ggplotly(
    data %>% 
    filter(Assembly == Assembly_name) %>%
   ggplot(aes(x = reorder(Type, Number), y = Number, fill = Type))+
    geom_bar(stat = "identity")+
    labs(x = "Type of gene", y = "Number")+
    theme_classic()
  )
}

#Annotation_type_function(data = read.csv("data/Annotation_data.csv"))

Annotation_table_function <- function(data, Assembly_name = "barcode09"){
  data %>% 
    filter(Assembly == Assembly_name) %>%
    mutate(Percentage = round(Percentage, 2)) %>% 
    select(Type, Number, Percentage) %>%
    DT::datatable(rownames = FALSE, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)))
}

#Annotation_table_function(data = read.csv("data/Annotation_data.csv"))


#---- Antismash data plots----

    #Total BGC bar plot
BGC_bar_plot <- function(data){
  # Ny dataframe laves, hvor der bliver grupperet for species og hvor mange BGCs der er per species i alt. 
  data_bar_total <- data  %>%
    group_by(Barcode,Species_name_new) %>% 
    count()	

  
  # Barplot. Kommandoen "coord_flip" g?r at vi f?r et liggende barplot, s?ledes der er plads til species navnene. 
  ggplot(data_bar_total, aes(x = Species_name_new, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue3") + coord_flip() + 
    scale_y_continuous(breaks = seq(0, 90, 5))+
    #scale_fill_gradient(low = "blue", high = "red") +
    theme_light() + 
    theme(legend.title = element_text(size = 10), 
          axis.text.x = element_text(size = 6)) +
    labs(x = NULL, y = NULL, fill = "Total number of BGCs")
  
}

    #Type af BGC antismash har fundet
BGC_heatmap_plot <- function(data, low, high){
  # Ny dataframe laves, hvor der bliver grupperet for species og hvor mange BGCs der er per species i alt. 
  data_heatmap <- data  %>%
    filter(Similarity_pct <= high, Similarity_pct >= low) %>%
     mutate(Most_smimilar_known_cluster_type = ifelse(str_detect(Most_smimilar_known_cluster_type, pattern = ":") == TRUE, unlist(strsplit(Most_smimilar_known_cluster_type, ":")), Most_smimilar_known_cluster_type )) %>%
      mutate(Most_smimilar_known_cluster_type = ifelse(str_detect(Most_smimilar_known_cluster_type, pattern = "\\+") == TRUE, unlist(strsplit(Most_smimilar_known_cluster_type, "\\+")), Most_smimilar_known_cluster_type )) %>%
      group_by(Species_name_new, Most_smimilar_known_cluster_type, Barcode) %>%
      count()

    # Create a heatmap using plot_ly
    heatmap <- plot_ly(data = data_heatmap ,x= ~Most_smimilar_known_cluster_type, y= ~Species_name_new, z = ~n , type = "heatmap", text = ~n)
    
    # Add layout options for better visualization
    heatmap <- heatmap %>% layout(
      title = "Interactive Heatmap",
      xaxis = list(title = "Type of BGCs"),
      yaxis = list(title = ""))
}



BGC_heatmap_plot2 <- function(data, low, high, type){
  # Ny dataframe laves, hvor der bliver grupperet for species og hvor mange BGCs der er per species i alt. 
  data_heatmap <- data  %>%
    filter(Similarity_pct <= high, Similarity_pct >= low, Most_smimilar_known_cluster_type == type) %>%
    mutate(Most_smimilar_known_cluster_type = ifelse(str_detect(Most_smimilar_known_cluster_type, pattern = ":") == TRUE, unlist(strsplit(Most_smimilar_known_cluster_type, ":")), Most_smimilar_known_cluster_type )) %>%
    mutate(Most_smimilar_known_cluster_type = ifelse(str_detect(Most_smimilar_known_cluster_type, pattern = "\\+") == TRUE, unlist(strsplit(Most_smimilar_known_cluster_type, "\\+")), Most_smimilar_known_cluster_type )) %>%
    group_by(Species_name_new, Most_smimilar_known_cluster_name, Barcode) %>%
    count()
  
  # Create a heatmap using plot_ly
  heatmap <- plot_ly(data = data_heatmap, x = ~Most_smimilar_known_cluster_name, y = ~Species_name_new, z = ~n, type = "heatmap", text = ~n, colorscale = c("white", "red"))
  
  # Add layout options for better visualization
  heatmap <- heatmap %>% layout(
    title = paste("Interactive Heatmap", type),
    xaxis = list(title = "Type of BGCs"),
    yaxis = list(title = ""))
  
  return(heatmap)
}


