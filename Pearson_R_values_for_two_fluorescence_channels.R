# ============================================================
# Co-localization analysis of two fluorescence channels
#
# INPUT:
# Two normalised fluorescence datasets produced by the
# normalisation/downsampling workflow.
#
# OUTPUT:
# 1. Merged fluorescence profiles
# 2. Pearson correlation coefficients
# 3. Peak localisation comparison
# ============================================================

library(readxl)
library(writexl)

# ============================================================
# USER INPUT
# Specify the two normalised Excel files - CHANGE THE EXCEL FILE NAMES
# ============================================================

Channel1_file <- "your_channel1_data.xlsx"

Channel2_file <- "your_channel2_data.xlsx"

# Read the datasets
Channel1_list <- read_excel(Channel1_file)

channel2_list <- read_excel(Channel2_file)

# Check that x coordinates are identical
if (!isTRUE(all.equal(Channel1_list$x, Channel2_list$x))) {
  stop("The x coordinates of the two datasets are not identical.")
}

# Number of cells
n_cells <- ncol(Channel1_list) - 1

# ============================================================
# PART 1
# Merge Channel1 and Channel2 fluorescence profiles
# ============================================================

merged_data <- data.frame(x = Channel1_list$x)

for (i in seq_len(n_cells)) {
  
  merged_data[[paste0("Cell", i, "_Channel1")]] <-
    Channel1_list[[i + 1]]
  
  merged_data[[paste0("Cell", i, "_Channel2")]] <-
    Channel2_list[[i + 1]]
}

write_xlsx(
  list(Colocalisation_profiles = merged_data),
  "Channel1_Channel2_profiles.xlsx"
)

cat("✓ Part 1 complete\n")

# ============================================================
# PART 2
# Pearson correlation for each cell
# (equivalent to Excel =CORREL())
# ============================================================

pearson_r <- numeric(n_cells)
pearson_p <- numeric(n_cells)

for (i in seq_len(n_cells)) {
  
  channel1 <- Channel1_list[[i + 1]]
  channel2 <- Channel2_list[[i + 1]]
  
  test <- cor.test(
    channel1,
    channel2,
    method = "pearson"
  )
  
  pearson_r[i] <- unname(test$estimate)
  pearson_p[i] <- test$p.value
}

pearson_results <- data.frame(
  Cell = paste0("Cell_", seq_len(n_cells)),
  Pearson_r = pearson_r,
  P_value = pearson_p
)

write_xlsx(
  list(Pearson_results = pearson_results),
  "Pearson_results.xlsx"
)

cat("✓ Part 2 complete\n")

# ============================================================
# PART 3
# Determine peak localisation of each protein
# ============================================================

Peak_Channel1 <- numeric(n_cells)
Peak_Channel2 <- numeric(n_cells)

x <- Channel1_list$x

for (i in seq_len(n_cells)) {
  
  channel1 <- Channel1_list[[i + 1]]
  channel2 <- Channel2_list[[i + 1]]
  
  # Channel1 peak
  channel1_peak_indices <- which(channel1 == max(channel1, na.rm = TRUE))
  Peak_channel1[i] <- mean(x[channel1_peak_indices])
  
  # Channel2 peak
  channel2_peak_indices <- which(channel2 == max(channel2, na.rm = TRUE))
  Peak_channel2[i] <- mean(x[channel2_peak_indices])
}

peak_positions <- data.frame(
  Cell = paste0("Cell_", seq_len(n_cells)),
  Peak_channel1 = Peak_channel1,
  Peak_channel2 = Peak_channel2,
  Peak_difference = Peak_channel1 - Peak_channel2
)

write_xlsx(
  list(Peak_positions = peak_positions),
  "Peak_positions.xlsx"
)

cat("✓ Part 3 complete\n")
