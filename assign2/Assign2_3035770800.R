# This is assignment 2 for HKU FITE7410.

# # Basic info -----------------------------------------------------------------

# Author: Jiang Feiyu
# UID: 3035770800
# Date: 14/04/2025

# Dataset Preparation Script ---------------------------------------------------

# Load required libraries
library(readr)        
library(dplyr)        
library(caret)        
library(tidyr)        
library(scales)   

# Install and load DMwR2
if (!require(DMwR2)) install.packages("DMwR2")
library(DMwR2)

# Install and load smotefamily
if (!require(smotefamily)) install.packages("smotefamily")
library(smotefamily)

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

# Step 4: Split the data
set.seed(123)
train_index <- sample(1:n_rows, n_train)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

# Step 5: Verify split results
print("\nStep 5: Verify split results")
print("Training set fraud distribution:")
print(table(train_data$isFraud))
print("\nTest set fraud distribution:")
print(table(test_data$isFraud))

# Step 6: Handle class imbalance using three methods
print("\nStep 6: Handle class imbalance")

# Convert isFraud to factor
train_data$isFraud <- as.factor(train_data$isFraud)

categorical_cols <- sapply(train_data, is.character)
for(col in names(train_data)[categorical_cols]) {
  train_data[[col]] <- as.numeric(as.factor(train_data[[col]]))
}

majority_class <- train_data[train_data$isFraud == "0", ]
minority_class <- train_data[train_data$isFraud == "1", ]

# Method 1: Undersampling
print("\nMethod 1: Undersampling")
set.seed(123)
undersampled_majority <- majority_class[sample(nrow(majority_class), nrow(minority_class)), ]
undersampled_data <- rbind(undersampled_majority, minority_class)
print("Distribution after undersampling:")
print(table(undersampled_data$isFraud))

# Method 2: Oversampling
print("\nMethod 2: Oversampling")
set.seed(123)
oversampled_minority <- minority_class[sample(nrow(minority_class), nrow(majority_class), replace = TRUE), ]
oversampled_data <- rbind(majority_class, oversampled_minority)
print("Distribution after oversampling:")
print(table(oversampled_data$isFraud))

# Method 3: SMOTE
print("\nMethod 3: SMOTE")
set.seed(123)

# Prepare data for SMOTE
X <- as.data.frame(train_data[, -which(names(train_data) == "isFraud")])  # Features
Y <- train_data$isFraud  # Target variable

# Ensure all features are numeric
X <- data.frame(sapply(X, as.numeric))

# Remove columns with all NA values
na_prop <- colMeans(is.na(X))
X <- X[, na_prop < 1]

# Handle remaining NA values
X <- data.frame(lapply(X, function(x) {
  if(is.numeric(x)) {
    med <- median(x, na.rm = TRUE)
    ifelse(is.na(x), med, x)
  } else {
    x
  }
}))

# Apply SMOTE with modified parameters
smote_result <- SMOTE(X, Y, K = 5, dup_size = 7)  # Increase dup_size to generate more synthetic samples
smote_data <- smote_result$data

print("Distribution after SMOTE:")
print(table(smote_data$class))

# Save balanced dataset
balanced_smote <- smote_data

print("\nFinal datasets dimensions:")
print("SMOTE dataset:")
print(dim(balanced_smote))