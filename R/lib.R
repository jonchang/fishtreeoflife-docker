requireNamespace("glue")
requireNamespace("stringr")
requireNamespace("ape")

make_nexus <- function(path, spp, seqs, tree = NULL, charsets = NULL) {
  nexus_data <- "#nexus
    begin data;
    dimensions ntax={ntax} nchar={nchar};
    format datatype=dna interleave=no gap=-;
    matrix
    "
  xz <- xzfile(path, "w")
  sink(xz)
  glue::glue(nexus_data, ntax = length(spp), nchar = nchar(seqs[1])) %>% cat(fill = TRUE)
  for (ii in 1:length(spp)) {
    cat(spp[ii])
    cat(" ")
    cat(seqs[ii], fill = TRUE)
  }
  cat(";\nend;\n\n")
  if (!is.null(charsets)) {
    cat("begin assumptions;\n")
    for (ii in 1:length(charsets)) {
      cat(paste0("charset ", charsets[ii], ";\n"))
    }
    cat("end;\n\n")
  }
  if (!is.null(tree)) {
    cat("begin trees;\n")
    cat("tree time_calibrated =", write.tree(tree))
    cat("\nend;\n")
  }
  sink(NULL)
  close(xz)
}

make_phylip <- function(path, spp, seqs) {
  xz <- xzfile(path, "w")
  sink(xz)
  cat(paste(length(spp), nchar(seqs[1])), fill = TRUE)
  for (ii in 1:length(spp)) {
    cat(spp[ii])
    cat(" ")
    cat(seqs[ii], fill = TRUE)
  }
  sink(NULL)
  close(xz)
}

get_rank_trees <- function(tree, spp) {
  tree_species <- stringr::str_replace_all(spp, " ", "_")
  mrca_tree <- ape::extract.clade(tree, ape::getMRCA(tree, tree_species))
  pruned_tree <- ape::drop.tip(mrca_tree, mrca_tree$tip.label[!mrca_tree$tip.label %in% tree_species])
  rogues <- setdiff(mrca_tree$tip.label, pruned_tree$tip.label)
  list(rogues = rogues, mrca_tree = mrca_tree, pruned_tree = pruned_tree)
}

# Parse a RAxML-style .partitions file into a data frame containing
# the gene name, starting offset, and ending offset.
parse_partitions <- function(partition_file) {
  # Capture groups: DNA, (name) = (start seq) - (end seq)
  pattern <- "\\w+,\\s*(\\w+)\\s*=\\s*(\\d+)\\s*-\\s*(\\d+)"
  matched <- stringr::str_match(readLines(partition_file), pattern)
  
  data.frame(gene = matched[, 2],
             starting_offset = as.integer(matched[, 3]),
             ending_offset = as.integer(matched[, 4]))
}

get_gene_sampling <- function(dna, partitions) {
  gap_character <- as.raw(4)
  gene_names <- partitions$gene
  
  ret <- matrix(0,
                nrow = nrow(dna),
                ncol = length(gene_names),
                byrow = TRUE,
                dimnames = list(rownames(dna), gene_names)
  )
  
  
  # For each species
  for (row in 1:nrow(dna)) {
    # For each partition
    for (i in 1:nrow(partitions)) {
      part_range <- partitions$starting_offset[i]:partitions$ending_offset[i]
      # Check for all gap characters
      if (!all(dna[row, part_range] == gap_character)) {
        ret[row, i] <- 1
      }
    }
  }
  ret
}

apply_df_vectors_genes_and_their_species <- function(partition_file, DNA_file){
  
  matrix_of_values <- sequence_check_per_species(partition_file, DNA_file)
  
  total_list <- list()
  
  for (gene_name in colnames(matrix_of_values)) {
    #comment explaing what this is doing
    total_list[[gene_name]] <- names(which(matrix_of_values[, gene_name] == 1))
  }
  
  return(total_list)
}


sequence_check_per_species <- function(partition_file, DNA_file){
  
  labridae_phylip.dna <- ape::read.dna(xzfile(DNA_file), format = "sequential", as.matrix = TRUE)
  
  partition_df <- parse_partitions(partition_file)
  
  vector_species <- vectorized_search_per_species(partition_df, labridae_phylip.dna)
  
  return(vector_species)
}

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
