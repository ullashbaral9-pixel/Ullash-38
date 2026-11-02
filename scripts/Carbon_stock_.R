data_path <- "data/metadata/bagmati_raw_inventory.csv"
raw_data <- read.csv(data_path, stringsAsFactors = FALSE)
dim(raw_data)

data_path <- "Data/metadata/bagmati_raw_inventory.csv"
raw_data <- read.csv(data_path, stringsAsFactors = FALSE)

dim(raw_data)
head(raw_data)
str(raw_data)

num_stands <- length(unique(raw_data$Stand_ID))
num_plots <- length(unique(raw_data$Plot_ID))
num_trees <- nrow(raw_data)

cat(sprintf("Total Forest Stands: %d\n", num_stands))
cat(sprintf("Total Plots: %d\n", num_plots))
cat(sprintf("Total Tree Observations: %d\n", num_trees))

missing_counts <- colSums(is.na(raw_data))
cat("\nMissing values per column:\n")
print(missing_counts)

clean_data <- subset(raw_data, 
                     DBH_cm > 0 & 
                     Biomass_kg > 0 & 
                     Plot_area_ha > 0 & 
                     Wood_density > 0 & Wood_density <= 1.2)

rows_removed <- nrow(raw_data) - nrow(clean_data)
cat(sprintf("Total rows removed during Quality Control: %d\n", rows_removed))
cat(sprintf("Total clean rows remaining for analysis: %d\n", nrow(clean_data)))            

analyze_stand_carbon <- function(stand_df) {
  
  # 1. Calculate individual tree carbon stock in kilograms (IPCC default fraction = 0.47)
  c_tree_kg <- stand_df$Biomass_kg * 0.47
  
  # 2. Convert individual tree carbon stock to Megagrams (Mg C)
  c_tree_mg <- c_tree_kg / 1000
  
  # 3. Calculate total stand-level carbon stock (Mg C) by summing all trees in the stand
  total_carbon_mg <- sum(c_tree_mg, na.rm = TRUE)
  
  # 4. Calculate total sampled area (ha) for the stand 
  # (Taking unique plot areas to ensure we don't duplicate overlapping records)
  plot_areas <- unique(stand_df[, c("Plot_ID", "Plot_area_ha")])
  total_sampled_area_ha <- sum(plot_areas$Plot_area_ha, na.rm = TRUE)
  
  # 5. Calculate carbon density per hectare (Mg C ha⁻¹)
  # Formula: Total Carbon Stock (Mg) / Total Sampled Area (ha)
  carbon_density_ha <- total_carbon_mg / total_sampled_area_ha
  
  # 6. Gather all summary metrics into a single return vector
  results <- c(
    Total_Trees = nrow(stand_df),
    Species_Richness = length(unique(stand_df$Species)),
    Max_Tree_Biomass_kg = max(stand_df$Biomass_kg, na.rm = TRUE),
    Total_Carbon_Mg = total_carbon_mg,
    Total_Sampled_Area_ha = total_sampled_area_ha,
    Carbon_Density_Mg_ha = carbon_density_ha
  )
  
  return(results)
}

# Task 5: Dynamic stand-level partitioning
# split() automatically divides our clean data into a list of subsets based on Stand_ID
stand_list <- split(clean_data, clean_data$Stand_ID)

# Apply our custom analytical function across all stands using sapply()
stand_analysis_results <- sapply(stand_list, analyze_stand_carbon)

# Task 6: Generate a summary matrix
# Convert the results into a clean data frame where forest stands are columns and metrics are rows
summary_matrix <- as.data.frame(stand_analysis_results)

# Print the final summary matrix to the console
cat("FINAL STAND-LEVEL CARBON STOCK SUMMARY MATRIX\n")
print(summary_matrix)


hist(as.numeric(summary_matrix["Carbon_Density_Mg_ha", ]), 
     main = "Histogram of Forest Carbon Stock per Hectare",
     xlab = "Above-Ground Carbon Density (Mg C ha⁻¹)",
     ylab = "Frequency of Stands",
     col = "forestgreen",
     border = "white")
