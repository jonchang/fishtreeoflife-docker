source("sequence_check_per_species.R")

#install.packages("rjson")

library(rjson)

library(ape)

df_list_genes_and_their_species <- function(partition_file, DNA_file){
  #print("In generate rank data")
  #json output, doesn't support the matrix data type
  #convert the matrix to a nested list
  #genes <- rag1, co1, etc.
  #list of gene names
  # each of the genes names are a vector of species that are present in that gene
  
  #out$genes <- list(coi = ..., cytb = ...)
  #What it should look like
  
  #when creating that list, easier to make the values of the list first then add the names
  
  #apply
  #Run apply on each column
  #Filter out the species, so only get ones that are sampled
  #return as a list
  
  #List with names,
  #Names are gene names
  #Values of that list is the vector of species names for that gene
  
  #Testing
  # path <- getwd()
  # #print("Path is:")
  # #print(path)
  # partition_file <- '/final_alignment.partitions'
  # DNA_file <- "Labridae.phylip.xz"
  # #DNA_file <- df
  # full_name <- paste(path, partition_file, sep = "")
  #print("Full name is:")
  #print(full_name)
  matrix_of_values <- sequence_check_per_species(partition_file, DNA_file)
  #print("After matrix of values")
  
  #Extract out the gene names from the matrix
  
  row_names<-rownames(matrix_of_values)
  #print("The row names are:")
  #print(row_names)
  
  #print("Column names are:")
  column_names<-colnames(matrix_of_values)
  #print(column_names)
  
  #print("Making a list")
  total_list <- list()
  
  
  #Now returning a dataframe for the JSON
  #Now trying to return a lists of dataframes
  
  #For each gene name
  for (col_number in 1:length(column_names)) {
  #for(row_number in 1:nrow(matrix_of_values)) {
    #A new vector for each gene name
    new_vector <- c()
    #new_vector <- append(new_vector, column_names[col_number])
    gene_name <- column_names[col_number]
    
    #print("The gene name is:")
    #print(column_names[col_number])
    
    #Now go through the column in the matrix with that gene name and append to the vector only if the value is 1
    for(row_number in 1:nrow(matrix_of_values)) {       # for-loop over columns
    #for (col_number in 1:length(column_names)) {
      #matrix_of_values[ , i] <- data1[ , i] + 10
      #If there is a 1, that gene belongs to that specific species
      
      #print("The row number is:")
      #print(row_number)
      
      #[row, column]
      if(matrix_of_values[row_number, col_number] == 1){
        new_vector <- append(new_vector, row_names[row_number])
      }
      # else{
      #   new_vector <- append(new_vector, "NA")
      # }
    }
    
    #Now doing dataframe
    #Each column must be filled, but not in a list of dataframes
    #columns are the species names
    df <- data.frame(matrix(ncol = 1, nrow = length(new_vector)))
    rownames(df) <- new_vector
    colnames(df) <- gene_name
    
    # print("Before adding on extra items")
    # for(species_of_gene in length(new_vector):length(column_names)){
    #   new_vector <- append(new_vector, "NULL")
    # }
    # print("After adding on extra items, it now looks like:")
    # print(new_vector)
    
    #Finally adding in the new vector
    #print("The vector is:")
    #print(new_vector)
    
    #print("Before add in dataframe, dataframe is:")
    #print(df)
    #df[nrow(df) + 1,] <- new_vector
    df[,1] <- new_vector
    #print("After adding in dataframe, dataframe is")
    #print(df)
    
    ##Now done with looping through that column
    ##Add the vector to the list
    ##total_list.append(new_vector)
    list2 <- list(df)
    total_list <- append(total_list, list2)
    
  }
  
  #print("The list of vectors")
  #print(total_list)
  return(total_list)
}