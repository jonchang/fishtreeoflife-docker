#!/usr/bin/env Rscript
library(ape)
library(dplyr)
library(readr)

source("R/monophy_minimal.R")

wanted_rank <- commandArgs(TRUE)
if (length(wanted_rank) < 1) {
    cat("Need to specify a rank on the command line\n")
    q(status = 1)
}

OUTDIR <- "_data/monophyly"

tre <- read.tree("downloads/actinopt_12k_treePL.tre.xz")

tax_orig <- read_csv("downloads/PFC_taxonomy.csv.xz", col_types = cols(.default = "c")) %>% transmute(tip = gsub(" ", "_", genus.species), label = .data[[wanted_rank]])

tax <- tax_orig %>% filter(tip %in% tre$tip.label) %>% as.data.frame()

res <- AssessMonophyly(tre, tax, verbosity = 0)

# purge the wicked
evil <- c(unlist(res$label$IntruderTips, use.names = F), unlist(res$label$OutlierTips, use.names = F))

tre2 <- drop.tip(tre, evil)
tax2 <- filter(tax, !tip %in% evil)
mono <- res$label$result["Monophyly"]
mono$label <- rownames(res$label$result)

# wrapper for mrca that returns NA instead of null if the clade is monotypic
mrca_na <- function(phy, tip) {
    r <- getMRCA(phy, tip)
    if (is.null(r)) return(NA)
    return(r)
}

# compute mcra nodes and the ages of those
btimes <- branching.times(tre2)
stats <- tax2 %>% group_by(label) %>% summarise(node = mrca_na(tre2, tip), depth = btimes[as.character(node)], N = n(), exemplar = first(tip))

# drop the extra tips that we don't need
skeleton <- drop.tip(tre2, which(!tre2$tip.label %in% stats$exemplar))
skeleton$tip.label <- stats$label[match(skeleton$tip.label, stats$exemplar)]
skeleton <- ladderize(skeleton, right = FALSE)

# save skeletal tree
write.tree(skeleton, file.path("downloads/taxonomy", paste0(wanted_rank, "_skeletal.tre")))

# open a null plot device to extract plot metrics
pdf(NULL)
plot(skeleton)
metrics <- .PlotPhyloEnv$last_plot.phylo
dev.off()

# extracted from ape
compute_phylogram <- function(edge, Ntip, Nnode, xx, yy) {
    nodes <- (Ntip + 1):(Ntip + Nnode)
    x0v <- xx[nodes]
    y0v <- y1v <- numeric(Nnode)
    NodeInEdge1 <- vector("list", Nnode)
    e1 <- edge[, 1]
    for (i in seq_along(e1)) {
        j <- e1[i] - Ntip
        NodeInEdge1[[j]] <- c(NodeInEdge1[[j]], i)
    }
    for (i in 1:Nnode) {
        j <- NodeInEdge1[[i]]
        tmp <- range(yy[edge[j, 2]])
        y0v[i] <- tmp[1]
        y1v[i] <- tmp[2]
    }
    x0h <- xx[edge[, 1]]
    x1h <- xx[edge[, 2]]
    y0h <- yy[edge[, 2]]
    tibble(x = c(x0h, x0v), y = c(y0h, y0v), xend = c(x1h, x0v), yend = c(y0h, y1v))
}

dir.create(OUTDIR, showWarning = FALSE, recursive = TRUE)

write_csv(with(metrics, compute_phylogram(edge, Ntip, Nnode, xx, yy)), file.path(OUTDIR, paste0(wanted_rank, "_svg.csv")))

# implementation detail: 1:Ntip should be the coordinates of the terminal eges

xx <- metrics$xx[1:metrics$Ntip]
yy <- metrics$yy[1:metrics$Ntip]

ramp <- colorRamp(RColorBrewer::brewer.pal(9, "BuGn")[1:8])
richness <- tax %>% group_by(label) %>% summarise(richness = n()) %>% mutate(color = rgb(ramp(log(richness) / log(max(richness))), maxColorValue = 255))

to_svg <- tibble(x = xx, y = yy, label = skeleton$tip.label) %>% left_join(mono) %>% left_join(stats) %>% select(-exemplar, -node) %>% mutate(depth = max(x) - depth) %>% left_join(richness)

js <- jsonlite::toJSON(to_svg)
cat(js, file = file.path(OUTDIR, paste0(wanted_rank, "_data.json")))



