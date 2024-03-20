# Preprocess the data
metadata_data <- metadata_data[complete.cases(metadata_data), ]
genomic_data <- genomic_data[complete.cases(genomic_data), ]
resistance_data <- resistance_data[complete.cases(resistance_data), ]

# Convert resistance gene columns to numeric
resistance_data[, -1] <- lapply(resistance_data[, -1], as.numeric)

# Check for missing values in each data frame
print(colSums(is.na(metadata_data)))
print(colSums(is.na(genomic_data)))
print(colSums(is.na(resistance_data)))