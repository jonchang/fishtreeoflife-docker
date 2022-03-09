requireNamespace("stringr")

# Parse a RAxML-style .partitions file into a data frame containing
# the gene name, starting offset, and ending offset.
parsing_partitions <- function(file_name) {
  readLinesText <- readLines(file_name)

  # Capture groups: DNA, (name) = (start seq) - (end seq)
  pattern <- "\\w+,\\s*(\\w+)\\s*=\\s*(\\d+)\\s*-\\s*(\\d+)"
  matched <- stringr::str_match(readLinesText, pattern)

  data.frame(gene = matched[, 2],
             starting_offset = as.integer(matched[, 3]),
             ending_offset = as.integer(matched[, 4]))
}
