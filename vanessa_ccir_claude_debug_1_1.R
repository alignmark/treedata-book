# Load necessary libraries
library(readxl)

# Define the path to the new Excel file
file_path <- "C:/Users/shenj/sources/repos/computational-biology/treedata-book/vanessa_ccir_3.xlsx"

# Read the data from each sheet of the Excel file
metadata_data <- read_excel(file_path, sheet = "metadata")
genomic_data <- read_excel(file_path, sheet = "genomic")
resistance_data <- read_excel(file_path, sheet = "resistance")

# Print the structure and first few rows of each data frame
print(str(metadata_data))
print(head(metadata_data))

print(str(genomic_data))
print(head(genomic_data))

print(str(resistance_data))
print(head(resistance_data))