#change the directory to your folder

# ------------------------------------------------------------
# Complete workflow: normalize, downsample, and export fluorescence data
# ------------------------------------------------------------

# Load required packages
library(readxl)    # To read Excel files
library(dplyr)     # For data manipulation
library(writexl)   # To write Excel files

# ----------------------------
# Step 1: Read raw Excel data
# ----------------------------
# Replace "your_file.xlsx" with your actual filename
raw_data <- read_excel("sample_data.xlsx")

# ----------------------------
# Step 2: Normalize x and y values (0–1)
# ----------------------------
normalize_x <- function(x) {
  x / tail(na.omit(x), 1)
}

normalize_y <- function(y) {
  y / max(y, na.rm = TRUE)
}

data_norm <- raw_data

for (i in seq(1, ncol(raw_data), by = 2)) {
  data_norm[[i]] <- normalize_x(raw_data[[i]])     # x columns
  data_norm[[i + 1]] <- normalize_y(raw_data[[i + 1]])  # y columns
}

# ----------------------------
# Step 3: Downsampling functions
# ----------------------------
count_points <- function(data) {
  n_samples <- ncol(data) / 2
  sapply(seq_len(n_samples), function(i) {
    x <- data[[2 * i - 1]]
    y <- data[[2 * i]]
    sum(!is.na(x) & !is.na(y))
  })
}

downsample_sample <- function(x, y, min_n) {
  valid <- !is.na(x) & !is.na(y)
  x_valid <- x[valid]
  y_valid <- y[valid]
  n <- length(x_valid)
  increment <- n / min_n
  indices <- floor(seq(1, n, by = increment))[1:min_n]
  data.frame(
    x = x_valid[indices],
    y = y_valid[indices]
  )
}

downsample_all <- function(data) {
  points_per_sample <- count_points(data)
  min_points <- min(points_per_sample)
  n_samples <- ncol(data) / 2
  downsampled <- vector("list", n_samples)
  
  for (i in seq_len(n_samples)) {
    x <- data[[2 * i - 1]]
    y <- data[[2 * i]]
    downsampled[[i]] <- downsample_sample(x, y, min_points)
  }
  
  # Combine into a single aligned data frame
  x_common <- downsampled[[1]]$x
  out <- data.frame(x = x_common)
  for (i in seq_len(n_samples)) {
    out[[paste0("sample_", i)]] <- downsampled[[i]]$y
  }
  out
}

# ----------------------------
# Step 4: Apply downsampling
# ----------------------------
normalised_list <- downsample_all(data_norm)

# ----------------------------
# Step 5: Export final dataset to Excel
# ----------------------------
write_xlsx(
  list(normalised_list = normalised_list),  # Sheet name
  path = "normalised_list.xlsx"             # File name
)


