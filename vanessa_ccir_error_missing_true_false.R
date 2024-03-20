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

# Define the path to the new Excel file
file_path <- "C:/Users/shenj/sources/repos/computational-biology/treedata-book/vanessa_ccir_2.xlsx"

# Read the data from each sheet of the Excel file
tab1_data <- read_excel(file_path, sheet = "tab1")
tab2_data <- read_excel(file_path, sheet = "tab2")
tab3_data <- read_excel(file_path, sheet = "tab3")

# Preprocess the data
tab1_data <- tab1_data[complete.cases(tab1_data), ]
tab2_data <- tab2_data[complete.cases(tab2_data), ]
tab3_data <- tab3_data[complete.cases(tab3_data), ]

# Convert resistance gene columns to numeric
tab3_data[, -1] <- lapply(tab3_data[, -1], as.numeric)

# Assuming 'tab3_data' contains the binary gene presence/absence matrix
binary_data <- tab3_data[, -1]

# Calculate the distance matrix
dist_matrix <- dist(binary_data, method = "binary")

# Construct a phylogenetic tree using the distance matrix
tree <- nj(dist_matrix)

# Root the tree if desired
tree <- midpoint.root(tree)

# Prepare metadata for annotation
metadata <- tab1_data %>% select(GCA, Country, Strain)

# Merge resistance data into metadata
metadata <- left_join(metadata, tab3_data, by = "GCA")

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