vectorized_search_per_species <- function(parsed_partition, full_genes){
  gene_names <- parsed_partition$gene_Name
  #First create a matrix to store if a species has a gene
  mat1 <- matrix(0,nrow=nrow(full_genes),ncol=length(gene_names),byrow=TRUE)
  colnames(mat1) <- gene_names
  rownames(mat1) <- rownames(full_genes)
  
  length_gene_places = nrow(parsed_partition)
  
  row_number <- nrow(full_genes)
  
  #Need to go through each species
  for(row in 1:row_number) {
    #Go through all the sections from the partition file
    for(i in 1:length_gene_places){
      #Go through all the sections from the partition file
      #page 35 of the book
      #a or c or t or g : 11110000 = 240
      if(all(full_genes[ row, strtoi(parsed_partition[i, 2]) : strtoi(parsed_partition[i, 3]) ] == as.raw(4)) == FALSE){
        #There was some nucletoide in the range
        mat1[row, i] <- 1
      }
    }
  }
  
  return(mat1)
}