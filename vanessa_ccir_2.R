# Load necessary libraries
library(readxl)
library(ape)
library(ggtree)
library(phytools)
library(dplyr)
library(ggtreeExtra)
library(treeio)
library(tidytree)
library(ggstar)
library(ggplot2)
library(ggnewscale)

# Read CSV file
metadata_data <- read.csv("C:/Users/shenj/sources/repos/computational-biology/treedata-book/vanessa_ccir_metadata.csv", na.strings = c("", "NA"))
genomic_data <- read.csv("C:/Users/shenj/sources/repos/computational-biology/treedata-book/vanessa_ccir_genomic.csv", na.strings = c("", "NA"))
resistance_data <- read.csv("C:/Users/shenj/sources/repos/computational-biology/treedata-book/vanessa_ccir_resistance.csv", na.strings = c("", "NA"))

# Inspect the GCA identifiers for mismatches
all_metadata_gca <- unique(metadata$GCA)
all_genomic_gca <- unique(genomic$GCA)
all_resistance_gca <- unique(resistance$GCA)

# Find GCA values not present in one or more dataframes
missing_in_genomic <- setdiff(all_metadata_gca, all_genomic_gca)
missing_in_resistance <- setdiff(all_metadata_gca, all_resistance_gca)

# Output the missing values to inspect them
print(missing_in_genomic)
print(missing_in_resistance)

# Preprocess the data
metadata_data <- metadata_data[complete.cases(metadata_data), ]
genomic_data <- genomic_data[complete.cases(genomic_data), ]
resistance_data <- resistance_data[complete.cases(resistance_data), ]

# Convert resistance gene columns to numeric
resistance_data[, -1] <- lapply(resistance_data[, -1], as.numeric)

# Assuming 'resistance_data' contains the binary gene presence/absence matrix
binary_data <- resistance_data[, -1]

# Calculate the distance matrix
dist_matrix <- dist(binary_data, method = "binary")

# Construct a phylogenetic tree using the distance matrix
tree <- nj(dist_matrix)

# Root the tree if desired
tree <- midpoint.root(tree)

# Prepare metadata for annotation
metadata <- metadata_data %>% select(GCA, Country, Strain)

# Merge resistance data into metadata
metadata <- left_join(metadata, resistance_data, by = "GCA")

# Check for NA values in the resistance data
cat("NA values in resistance data:", sum(is.na(resistance_data)), "\n")

# Check the unique values in the resistance data
cat("Unique values in resistance data:", unique(unlist(resistance_data[, -1])), "\n")

# Check for missing cases in the metadata
cat("Missing cases in metadata:", sum(!complete.cases(metadata_data)), "\n")

# ... (after creating binary_data and dist_matrix)

# Check the length of metadata and tree tip labels
cat("Number of tree tips:", length(tree$tip.label), "Number of metadata rows:", nrow(metadata), "\n")

# Check if there are any GCA values in metadata not present in the tree tip labels
missing_GCA <- setdiff(metadata$GCA, tree$tip.label)
if (length(missing_GCA) > 0) {
  cat("Missing GCA in tree tip labels:", missing_GCA, "\n")
}

Error in if (stats::var(as.numeric(dat[[xid]]), na.rm = TRUE) != 0) { : 
  missing value where TRUE/FALSE needed

Error in `[[<-.data.frame`(`*tmp*`, paste0("new_", xid), value = 0) : 
  replacement has 1 row, data has 0

# Ignore negative edge lengths
options(ignore.negative.edge=TRUE)

# Plot the tree in circular layout with ggtree
p <- ggtree(tree, layout = 'circular', size = 0.5) +
     geom_tiplab(size = 2, align = TRUE)

# Add country color strips
p <- p + new_scale_fill() +
         geom_fruit(
           data = metadata,
           geom = geom_tile,
           mapping = aes(y = Strain, x = Country, fill = Country),
           offset = 0.05,
           size = 0.1
         ) +
         scale_fill_manual(values = c("Canada" = "red", "USA" = "blue", "China" = "green"))

# Add resistance gene presence/absence strips
resistance_genes <- c("msrA", "ileS.2", "tetK", "tetL", "tetM", "dfrG")
for (gene in resistance_genes) {
  p <- p + new_scale_fill() +
           geom_fruit(
             data = metadata,
             geom = geom_tile,
             mapping = aes_string(y = "Strain", x = gene, fill = gene),
             offset = 0.05,
             size = 0.1
           ) +
           scale_fill_manual(values = c("0" = "white", "1" = "black"), na.value = "grey")
}

# Add a legend for resistance genes
p <- p + theme(
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6),
  legend.position = "right",
  legend.spacing.y = unit(0.2, "cm")
)

# Render the plot
print(p)