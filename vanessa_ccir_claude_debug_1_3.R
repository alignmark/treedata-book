library(dplyr)

# Prepare metadata for annotation
metadata <- metadata_data %>% select(GCA, Country, Strain)

# Merge resistance data into metadata
metadata <- left_join(metadata, resistance_data, by = "GCA")

# Check for mismatches in GCA column
mismatch_gca <- setdiff(metadata$GCA, resistance_data$GCA)
print(mismatch_gca)