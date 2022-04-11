source("sequence_check_per_species.R")

library(rjson)

library(ape)

apply_df_list_genes_and_their_species <- function(partition_file, DNA_file){
  
  matrix_of_values <- sequence_check_per_species(partition_file, DNA_file)
  
  total_list <- list()
  
  for (gene_name in colnames(matrix_of_values)) {
    #comment explaing what this is doing
    total_list[[gene_name]] <- names(which(matrix_of_values[, gene_name] == 1))
  }
  
  return(total_list)
}