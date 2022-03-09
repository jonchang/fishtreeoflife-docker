source("parsing_partitions.R")
source("vectorized_search_per_species.R")
library(ape)

sequence_check_per_species <- function(partition_file, DNA_file){
  
  labridae_phylip.dna <- ape::read.dna(xzfile(DNA_file), format = "sequential", as.matrix = TRUE)
  
  path <- getwd()
  full_name <- paste(path, partition_file, sep = "")
  partition_df <- parsing_partitions(full_name)
  
  vector_species <- vectorized_search_per_species(partition_df, labridae_phylip.dna)
  #print(vector_species)
  
  #Now any gene that was not in a species will have a mean of 0
  means_of_genes <- colMeans(vector_species)
  
  len_means <- length(means_of_genes)
  
  for (i in 1:len_means){
    if (means_of_genes[i] == 0){
      print("This gene was not sequenced:")
      print(partition_df[i, 1])
    }
  }
  
  
}

#Testing
#fileName <- '/final_alignment.partitions'
#DNA_file <- "Labridae.phylip.xz"
#sequence_check_per_species(fileName, DNA_file)