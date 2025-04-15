# This is assignment 2 for HKU FITE7410.

# # Basic info -----------------------------------------------------------------

# Author: Jiang Feiyu
# UID: 3035770800
# Date: 14/04/2025

# 以下所有注释用英文

# Dataset Preparation Script ----------------------------------------------------

# Load required libraries
library(readr)        
library(dplyr)        
library(caret)        
library(tidyr)        
library(scales)        

# Read the dataset
dataset <- read.csv("A1_data.csv")

# Step 1: First verify the data is loaded correctly
print("Step 1: Verify data loading")
print("Dataset dimensions:")
print(dim(dataset))

# Step 2: Check original class distribution
print("\nStep 2: Original class distribution")
orig_table <- table(dataset$isFraud)
print(orig_table)

# Step 3: Calculate split sizes
print("\nStep 3: Calculate split sizes")
n_rows <- nrow(dataset)
n_train <- floor(0.8 * n_rows)
print(paste("Total rows:", n_rows))
print(paste("Training rows:", n_train))
print(paste("Testing rows:", n_rows - n_train))

# Let's stop here and verify everything is working correctly so far
