source("parsing_partitions.R")
source("vectorized_search_per_species.R")
library(ape)

sequence_check_per_species <- function(partition_file, DNA_file){
  
  labridae_phylip.dna <- ape::read.dna(xzfile(DNA_file), format = "sequential", as.matrix = TRUE)
  
  partition_df <- parsing_partitions(partition_file)
  
  vector_species <- vectorized_search_per_species(partition_df, labridae_phylip.dna)
  
  return(vector_species)
  
}