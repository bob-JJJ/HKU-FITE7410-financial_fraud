# This is assignment 1 for HKU FITE7410.

# # Basic info -----------------------------------------------------------------

# Author: Jiang Feiyu
# UID: 3035770800
# Date: 26/02/2025

# # Preparation ----------------------------------------------------------------

# Install packages/load libraries
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("reshape2")) install.packages("reshape2")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

#install.packages(c("tidyverse", "naniar", "mice", "outliers", "car"))
library(tidyverse)
library(naniar)     # For missing value analysis
library(mice)       # For missing value imputation
library(outliers)   # For outlier detection
library(car)        # For data visualization
library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)


# Load the data
setwd("C:/Users/Bob/Desktop/FITE7410/Assign/assign1/materials") # This is the path of my working folder
data <- read.csv("A1_data.csv")

# Check if output folder exists, if not create it
if (!dir.exists("output")) {
  dir.create("output")
}

# # Statistics Summarizing -----------------------------------------------------

# Save structure information
sink("output/structure.txt")
str(data)
sink()

# Save summary statistics
sink("output/summary.txt")
summary(data)
sink()

# Save class labels
sink("output/class_labels.txt")
print("Fraud labels (0: Normal, 1: Fraud):")
print(table(data$isFraud))
sink()

# Save class percentages
sink("output/class_percentages.txt")
print("Percentage of transactions:")
percentages <- round(prop.table(table(data$isFraud)) * 100, 2)
print(paste("Normal transactions (0):", percentages[1], "%"))
print(paste("Fraudulent transactions (1):", percentages[2], "%"))
sink()

# # Data Handling --------------------------------------------------------------

## Missing Values Treatment --------------------------------------------

# Calculate missing value percentages for each column
missing_percentages <- colMeans(is.na(data)) * 100
high_missing_cols <- names(missing_percentages[missing_percentages > 50])

# Remove columns with more than 50% missing values
data_clean <- data[, !names(data) %in% high_missing_cols]

# For remaining numeric columns, impute missing values
numeric_cols <- names(data_clean)[sapply(data_clean, is.numeric)]
for(col in numeric_cols) {
  if(any(is.na(data_clean[[col]]))) {
    median_val <- median(data_clean[[col]], na.rm = TRUE)
    data_clean[[col]][is.na(data_clean[[col]])] <- median_val
  }
}

# For categorical columns, impute with mode
categorical_cols <- names(data_clean)[sapply(data_clean, is.character)]
for(col in categorical_cols) {
  if(any(is.na(data_clean[[col]]) | data_clean[[col]] == "")) {
    mode_val <- names(sort(table(data_clean[[col]]), decreasing = TRUE))[1]
    data_clean[[col]][is.na(data_clean[[col]]) | data_clean[[col]] == ""] <- mode_val
  }
}

## Outlier Detection (IQR + Truncation) -----------------------------------------------------

# truncation function
truncate_values <- function(x, lower_percentile = 0.01, upper_percentile = 0.99) {
  lower <- quantile(x, lower_percentile)
  upper <- quantile(x, upper_percentile)
  return(list(
    lower = lower,
    upper = upper,
    values = pmin(pmax(x, lower), upper)
  ))
}

for(col in numeric_cols) {
  # Skip ID and binary columns
  if(!grepl("ID$|^is", col, ignore.case = TRUE) && length(unique(data_clean[[col]])) > 2) {
    cat("\n=== Processing Variable:", col, "===\n")
    
    # 1. IQR Method
    cat("\n1. IQR Method Results:\n")
    # Calculate median and IQR
    M <- median(data_clean[[col]])
    Q1 <- quantile(data_clean[[col]], 0.25)
    Q3 <- quantile(data_clean[[col]], 0.75)
    IQR <- Q3 - Q1
    
    # Calculate bounds using the formula: M ± 3 x IQR/(2 x 0.6745)
    constant <- 3 * IQR/(2 * 0.6745)
    iqr_lower <- M - constant
    iqr_upper <- M + constant
    
    # 2. Truncation Method
    cat("\n2. Truncation Method Results:\n")
    trunc_result <- truncate_values(data_clean[[col]])
    
    # Print comparative summary
    cat("\nComparative Summary:")
    cat("\n------------------")
    cat("\nMedian:", M)
    cat("\nIQR:", IQR)
    cat("\n\nIQR Method Bounds:")
    cat("\n- Lower:", iqr_lower)
    cat("\n- Upper:", iqr_upper)
    cat("\n- Outliers:", sum(data_clean[[col]] < iqr_lower | data_clean[[col]] > iqr_upper))
    
    cat("\n\nTruncation Method Bounds (1% and 99%):")
    cat("\n- Lower:", trunc_result$lower)
    cat("\n- Upper:", trunc_result$upper)
    cat("\n- Outliers:", sum(data_clean[[col]] < trunc_result$lower | data_clean[[col]] > trunc_result$upper))
    
    # Combine both methods (using the more conservative bounds)
    final_lower <- max(iqr_lower, trunc_result$lower)
    final_upper <- min(iqr_upper, trunc_result$upper)
    
    # Apply final bounds
    data_clean[[col]] <- pmin(pmax(data_clean[[col]], final_lower), final_upper)
    
    cat("\n\nFinal Combined Bounds:")
    cat("\n- Lower:", final_lower)
    cat("\n- Upper:", final_upper)
    cat("\n- Total modified values:", sum(data_clean[[col]] == final_lower | data_clean[[col]] == final_upper))
    cat("\n==========================================\n")
    
  }
}

# Save the cleaned data
write.csv(data_clean, "output/cleaned_data_combined_methods.csv", row.names = FALSE)

# Create summary report
sink("output/outlier_treatment_report.txt")
cat("Outlier Treatment Summary Report\n")
cat("Date:", Sys.time(), "\n\n")

for(col in numeric_cols) {
  if(!grepl("ID$|^is", col, ignore.case = TRUE) && length(unique(data_clean[[col]])) > 2) {
    cat("\nVariable:", col)
    cat("\nBefore cleaning:\n")
    print(summary(data_clean[[col]]))
    cat("\nAfter cleaning:\n")
    print(summary(data_clean[[col]]))
    cat("\n-------------------\n")
  }
}
sink()

## Save cleaning summary -------------------------------------------------------
sink("output/cleaning_summary.txt")
cat("Data Cleaning Summary\n\n")
cat("1. Removed columns with >50% missing values:\n")
cat(paste(high_missing_cols, collapse = "\n"), "\n\n")
cat("2. Original dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
cat("3. Cleaned dimensions:", nrow(data_clean), "rows,", ncol(data_clean), "columns\n")
sink()



## Write your code here

#ggplot(...)
#hist(...)
#...

# 3. Bi-/Multi-variate Analysis
#ggplot(dataset, aes(...)) + geom_bar()
#corrplot(...)
#......


