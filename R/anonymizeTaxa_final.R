setwd("~/Desktop/fishtreeoflife")

library(ape)
library(fishtree)
library(stringr)

#If the input_label (tip.label) corresponds to a known unsampled species, 
#rewrite the tip.label to contain its anonymized handle
anonymize_taxa <- function(input_label){
  if(input_label %in% names1){
    index <- match(input_label, names1)
    input_label <- names2[index]
  }else{
    input_label <- input_label
  }
}
#Anonymize any tip.labels representing unsampled species in a given input_tree
anonymize_tree <- function(input_tree){
  lapply(input_tree$tip.label, anonymize_taxa)
}

anonymize_actinopt_full_trees_xz <- function(){
  #read in list of trees
  listOfTrees <- read.tree("downloads/actinopt_full.trees.xz")

  #get list of unsampled species  
  tax <- fishtree_taxonomy(rank = "Labridae")
  unsampledSpecies <- tax$Labridae$unsampled_species
  unsampledSpecies_anon <- unsampledSpecies

  #generate anonymized handles
  i = as.integer(0)
  i <- i + 1
  for(value in unsampledSpecies_anon){
    indexOfSpace <- stringr::str_locate(value, pattern = " ")
    genus <- substr(value, 1, indexOfSpace - 1)
    unsampledSpecies_anon[i] <- paste(genus, "TACT", i, sep = "_")
    i <- i + 1
  }

  i = as.integer(0)
  i <- i + 1
  for(value in unsampledSpecies){
    indexOfSpace <- stringr::str_locate(value, pattern = " ")
    genus <- substr(value, 1, indexOfSpace - 1)
    species <- substr(value, indexOfSpace + 1, nchar(value))
    unsampledSpecies[i] <- paste(genus, species, sep = "_")
    i <- i + 1
  }

  #anonymize the tip.labels
  lapply(listOfTrees, anonymize_tree)
}