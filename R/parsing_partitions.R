#install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")

#turning the bit that parses the .partitions file into a function 
#that returns a data frame with three columns, 

#the first being the gene name, 
#the second being the starting offset, 
#and the third being the ending offset.

parsing_partitions <- function(file_name) {
  
  #using readlines instead of scan and capturing the relevant text from each line using str_match
  readLinesText <- readLines(file_name)
  #Now have a vector of the lines
  
  #now use str_match
  #https://regex101.com/r/hyUgZh/1
  #It is vectorized and so I don't need to write a for loop
  #HAVE TO USE an extra for every '\'!, and use the website:
  #https://regex101.com/
  #1st: "Data," : (\\w+),
  #2nd: space: \\s*
  #3rd: gene name: (\\w+)
  #4th: space: \\s*
  #5th: =: =
  #6th: space: \\s*
  #7th: starting sequence: (\\d+)
  #8th: space: \\s*
  #9th: -: -
  #10th: space: \\s*
  #11th: ending sequence: (\\d+)
  #Overall: DNA, (name) = (start seq) - (end seq)
  pattern <- "(\\w+),\\s*(\\w+)\\s*=\\s*(\\d+)\\s*-\\s*(\\d+)"
  #extract relevant contents before converting to a data frame
  matched <- str_match(readLinesText, pattern)
  #Removing the first 2 columns as they contained the full string and "DNA, "
  needed_data <- matched[, 3:5]
  df <- as.data.frame(needed_data)
  #Renaming the remaining 3 columns in proper fashion
  colnames(df) <- c("gene_Name", "starting_Offset", "ending_Offset")
  return (df)
}

#testing
# #Must have this
# setwd('/home/striker/Desktop/UCLA/BS Research Paper/SideWork')
# path <- getwd()
# fileName <- '/final_alignment.partitions'
# full_name <- paste(path, fileName, sep = "")
# partition_df <- parsing_partitions(full_name)
# print(partition_df)