#import pylugenetic data from http://purl.org/phylo/treebase/phylows/tree/TB2:Tr2026

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("treeio")
BiocManager::install("ggtree")
library(treeio)
library(ggtree)

data <- read.newick("C:/Users/madsh/Downloads/uL-z1W8sGZUkxLrYPR9ANA_newick.txt")
ggtree(data, layout='circular') + geom_tiplab()
tree <- data %>%
  mutate(kingdom = list(
    Archaea = c(
      "Nanoarchaeum_equitans", "Pyrobaculum_aerophilum", "Aeropyrum_pernix",
      "Sulfolobus_solfataricus", "Sulfolobus_tokodaii", "Thermoplasma_volcanium",
      "Thermoplasma_acidophilum", "Archaeoglobus_fulgidus", "Halobacterium_sp._NRC-1",
      "Methanosarcina_acetivorans", "Methanosarcina_mazei", "Pyrococcus_furiosus",
      "Pyrococcus_horikoshii", "Pyrococcus_abyssi", "Methanococcus_maripaludis",
      "Methanococcus_jannaschii", "Methanopyrus_kandleri",
      "Methanobacterium_thermautotrophicum"
    ),
    Eukaryota = c(
      "Giardia_lamblia", "Leishmania_major", "Thalassiosira_pseudonana",
      "Plasmodium_falciparum", "Cryptosporidium_hominis", "Cyanidioschyzon_merolae",
      "Oryza_sativa", "Arabidopsis_thaliana", "Dictyostelium_discoideum",
      "Schizosaccharomyces_pombe", "Saccharomyces_cerevisiae", "Eremothecium_gossypii",
      "Caenorhabditis_elegans", "Caenorhabditis_briggsae", "Drosophila_melanogaster",
      "Anopheles_gambiae", "Danio_rerio", "Takifugu_rubripes", "Gallus_gallus")))
    
ggtree(tree, layout="circular") + geom_tiplab(aes(angle=angle), color='blue')


ggtree(tree, layout = "circular") +  geom_nodelab(geom='label') + hexpand(.1)

# Kingdom: Archaea
archaea <- c(
  "Nanoarchaeum_equitans", "Pyrobaculum_aerophilum", "Aeropyrum_pernix",
  "Sulfolobus_solfataricus", "Sulfolobus_tokodaii", "Thermoplasma_volcanium",
  "Thermoplasma_acidophilum", "Archaeoglobus_fulgidus", "Halobacterium_sp._NRC-1",
  "Methanosarcina_acetivorans", "Methanosarcina_mazei", "Pyrococcus_furiosus",
  "Pyrococcus_horikoshii", "Pyrococcus_abyssi", "Methanococcus_maripaludis",
  "Methanococcus_jannaschii", "Methanopyrus_kandleri",
  "Methanobacterium_thermautotrophicum"
)

# Kingdom: Eukaryota
eukaryota <- c(
  "Giardia_lamblia", "Leishmania_major", "Thalassiosira_pseudonana",
  "Plasmodium_falciparum", "Cryptosporidium_hominis", "Cyanidioschyzon_merolae",
  "Oryza_sativa", "Arabidopsis_thaliana", "Dictyostelium_discoideum",
  "Schizosaccharomyces_pombe", "Saccharomyces_cerevisiae", "Eremothecium_gossypii",
  "Caenorhabditis_elegans", "Caenorhabditis_briggsae", "Drosophila_melanogaster",
  "Anopheles_gambiae", "Danio_rerio", "Takifugu_rubripes", "Gallus_gallus",
  "Homo_sapiens", "Pan_troglodytes", "Rattus_norvegicus", "Mus_musculus"
)


library(ggtree)
library(dplyr)

# Create a dataframe mapping each tip label to its corresponding kingdom
tip_kingdom <- data.frame(
  label = tree$tip.label,
  kingdom = ifelse(tree$tip.label %in% archaea, "Archaea",
                   ifelse(tree$tip.label %in% eukaryota, "Eukaryota", "Bacteria"))
)

# Create a phylo object
phy_tree <- tree

# Set tip labels
# Set tip labels
phy_tree$tip.label <- tip_kingdom$label



ggtree_obj <- ggtree(phy_tree, layout = "circular") +
geom_nodelab()+
  scale_color_manual(values = c("Archaea" = "red", "Eukaryota" = "blue", "Bacteria" = "green")) +
  theme_tree2()

# Plot the tree
ggtree_obj

# Create ggtree object with node and area coloring
ggtree_obj <- ggtree(phy_tree, layout = "circular") +
  geom_tree(aes(color = tip_kingdom$kingdom), alpha = 0.5) +  # Color the tree branches
  scale_color_manual(values = c("Archaea" = "red", "Eukaryota" = "blue", "Bacteria" = "green")) +
  theme_tree2()

# Plot the tree
ggtree_obj

# Create ggtree object with node and area coloring

