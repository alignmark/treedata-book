# Load necessary libraries
library(readxl)
library(ape)
library(ggtree)
library(phytools)
library(dplyr)

# Define the path to the new Excel file
file_path <- "C:/Users/shenj/sources/repos/computational-biology/treedata-book/vanessa_ccir_2.xlsx"

# Read the data from each sheet of the Excel file
tab1_data <- read_excel(file_path, sheet = "tab1")
tab2_data <- read_excel(file_path, sheet = "tab2")
tab3_data <- read_excel(file_path, sheet = "tab3")

# Assuming 'tab3_data' contains the binary gene presence/absence matrix
# Note: Adjust the column indices to match your actual data
binary_data <- tab3_data[, -c(1:2)] # Remove non-binary columns, if there are any

# Calculate the distance matrix
dist_matrix <- dist(binary_data, method = "binary")

# Construct a phylogenetic tree using the distance matrix
tree <- nj(dist_matrix)

# Root the tree if desired
tree <- midpoint.root(tree)

# Prepare metadata for annotation, adjust the column names to match your actual data
metadata <- tab1_data %>% select(GCA, Country)

# Merge resistance data into metadata if needed
metadata <- left_join(metadata, tab3_data, by = "GCA")

# Plot the tree in circular layout with ggtree
p <- ggtree(tree, layout = 'circular') +
     geom_tiplab() +
     geom_tippoint(aes(color = Country))

# Adjust color settings as needed
p <- p + scale_color_manual(values = c("Canada" = "red",
                                       "USA" = "blue",
                                       "China" = "green"))

# Optionally, if you have resistance data, use it to adjust tip point shapes
p <- p + geom_tippoint(aes(shape = mecA)) # 'mecA' is an example gene column

# Optionally, add other ggtree layers for additional annotations

# Render the plot
print(p)
