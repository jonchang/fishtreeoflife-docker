library("stringr")

# Parse a RAxML-style .partitions file into a data frame containing
# the gene name, starting offset, and ending offset.
parsing_partitions <- function(file_name) {
  readLinesText <- readLines(file_name)

  # Capture groups: (DNA), (name) = (start seq) - (end seq)
  pattern <- "(\\w+),\\s*(\\w+)\\s*=\\s*(\\d+)\\s*-\\s*(\\d+)"
  matched <- str_match(readLinesText, pattern)

  # Drop full match and captured "DNA" string
  needed_data <- matched[, 3:5]
  df <- as.data.frame(needed_data)
  colnames(df) <- c("gene_Name", "starting_Offset", "ending_Offset")
  return (df)
}
