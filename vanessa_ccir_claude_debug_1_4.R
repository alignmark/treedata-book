library(ape)
library(ggtree)
library(phytools)

# Assuming 'resistance_data' contains the binary gene presence/absence matrix
binary_data <- resistance_data[, -1]

# Calculate the distance matrix
dist_matrix <- dist(binary_data, method = "binary")

# Construct a phylogenetic tree using the distance matrix
tree <- nj(dist_matrix)

# Root the tree if desired
tree <- midpoint.root(tree)

# Ignore negative edge lengths
options(ignore.negative.edge = TRUE)

# Plot the tree in circular layout with ggtree
p <- ggtree(tree, layout = 'circular', size = 0.5) +
  geom_tiplab(size = 2, align = TRUE)

# Render the plot
print(p)