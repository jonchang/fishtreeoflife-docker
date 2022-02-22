setwd('/home/striker/Desktop/UCLA/BS Research Paper/SideWork')

#As a start, try turning the bit that parses the .partitions file into a function 
#that returns a data frame with three columns, 

#the first being the gene name, 
#the second being the starting offset, 
#and the third being the ending offset.

parsing_partitions <- function(file_name) {
  #Need to return a dataframe with 3 columns:
  #1st column: The gene name
  gene_names <- c()
  #2nd column: The starting offset
  starting_offset <- c()
  #3rd column: The ending offset
  ending_offset <- c()
  
  
  fileName <- file_name
  partions_strings <- readChar(fileName, file.info(fileName)$size)
  
  partion_strings_newline_split <- scan(text = partions_strings, what = "\n")
  print(partion_strings_newline_split)
  

  gene_places <- c()
  
  n = length(partion_strings_newline_split)
  #print(n)
  for (i in 1:n) {
    #print(i)
    #print(partion_strings_newline_split[i])
    if(partion_strings_newline_split[i] == "DNA,"){
      #i is "DNA,"
      #i+1 is the name
      gene_names <- c(gene_names, partion_strings_newline_split[i+1])

      #need to break apart [i+3] and get the value after "-"
      #i+3 is the range
      #print(partion_strings_newline_split[i+3])
      #split_values <- scan(text = partion_strings_newline_split[i+3], what = "-")
      split_values <- strsplit(partion_strings_newline_split[i+3], split = "-")
      
      starting_offset<-c(starting_offset, strtoi(split_values[[1]][1]))
      ending_offset<-c(ending_offset, strtoi(split_values[[1]][2]))
      
    }
  }
  
  df <- data.frame(gene_names, starting_offset, ending_offset)
  
  return (df)
}

#testing
#fileName <- 'final_alignment.partitions'

#partition_df <- parsing_partitions(fileName)


#print(partition_df)