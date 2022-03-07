#path <- getwd()
#Must have this
setwd('/home/striker/Desktop/UCLA/BS Research Paper/SideWork')

#install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")

#As a start, try turning the bit that parses the .partitions file into a function 
#that returns a data frame with three columns, 

#the first being the gene name, 
#the second being the starting offset, 
#and the third being the ending offset.

parsing_partitions <- function(file_name) {
  path <- getwd()
  
  fileName <- file_name
  
  #using readlines instead of scan and capturing the relevant text from each line using str_match
  full_name <- paste("/", fileName, sep="")
  readLinesText <- readLines(paste(path, full_name, sep = ""))
  print("The output of readLines is:")
  print(readLinesText)
  #Now have a vector of the lines
  
  #now use str_match
  #https://regex101.com/r/hyUgZh/1
  #It is vectorized and so I don't need to write a for loop
  #HAVE TO USE an extra for every '\'!, and use the website:
  #https://regex101.com/
  pattern <- "(\\w+),\\s*(\\w+)\\s*=\\s*(\\d+)\\s*-\\s*(\\d+)"
  df <- as.data.frame(str_match(readLinesText, pattern))
  #Removing the first 2 columns as they contained the full string and "DNA, "
  returned_df <- subset(df, select = -c(V1, V2))
  #Renaming the remaining 3 columns in proper fashion
  names(returned_df)[names(returned_df) == 'V3'] <- 'gene_Name'
  names(returned_df)[names(returned_df) == 'V4'] <- 'start_Offset'
  names(returned_df)[names(returned_df) == 'V5'] <- 'ending_Offset'
  return (returned_df)
}

#testing
# fileName <- 'final_alignment.partitions'
# partition_df <- parsing_partitions(fileName)
# print(partition_df)