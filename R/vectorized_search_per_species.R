vectorized_search_per_species <- function(parsed_partition, full_genes){
  gene_names <- parsed_partition$gene_Name
  #First create a matrix to store if a species has a gene
  mat1 <- matrix(0, 
                 nrow=nrow(full_genes), 
                 ncol=length(gene_names), 
                 byrow=TRUE,
                 dimnames = list(rownames(full_genes), gene_names)
                )
  
  #This is the value for when there are no A's, C's, T's, or G's
  gap_character <- as.raw(4)
  
  #Need to go through each species
  for(row in 1:nrow(full_genes)){
    #Go through all the sections from the partition file
    for(i in 1:nrow(parsed_partition)){
      #Go through all the sections from the partition file
      if(all(full_genes[ row, parsed_partition[i, 2] : parsed_partition[i, 3] ] == gap_character) == FALSE){
        #There was some nucletoide in the range
        mat1[row, i] <- 1
      }
    }
  }
  
  return(mat1)
}