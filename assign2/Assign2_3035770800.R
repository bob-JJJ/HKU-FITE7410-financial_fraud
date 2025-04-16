# This is assignment 2 for HKU FITE7410.

# # Basic info -----------------------------------------------------------------

# Author: Jiang Feiyu
# UID: 3035770800
# Date: 14/04/2025

# Dataset Preparation Script ---------------------------------------------------

# Install and load DMwR2
if (!require(DMwR2)) install.packages("DMwR2")
library(DMwR2)

# Install and load smotefamily
if (!require(smotefamily)) install.packages("smotefamily")
library(smotefamily)

# Install and load DMwR2
if (!require(DMwR2)) install.packages("DMwR2")
library(DMwR2)

# Install and load smotefamily
if (!require(smotefamily)) install.packages("smotefamily")
library(smotefamily)

# Load required libraries
library(readr)        
library(dplyr)        
library(caret)        
library(tidyr)        
library(scales)
library(randomForest)
library(pROC)

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

## Method 1: Undersampling -----------------------------------------------------
print("\nMethod 1: Undersampling")
set.seed(123)
undersampled_majority <- majority_class[sample(nrow(majority_class), nrow(minority_class)), ]
undersampled_data <- rbind(undersampled_majority, minority_class)
print("Distribution after undersampling:")
print(table(undersampled_data$isFraud))

## Method 2: Oversampling ------------------------------------------------------
print("\nMethod 2: Oversampling")
set.seed(123)
oversampled_minority <- minority_class[sample(nrow(minority_class), nrow(majority_class), replace = TRUE), ]
oversampled_data <- rbind(majority_class, oversampled_minority)
print("Distribution after oversampling:")
print(table(oversampled_data$isFraud))

## Method 3: SMOTE -------------------------------------------------------------
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

# Apply SMOTE
smote_result <- SMOTE(X, Y, K = 5, dup_size = 7)
smote_data <- smote_result$data

# Rename class column to isFraud
names(smote_data)[names(smote_data) == "class"] <- "isFraud"

print("Distribution after SMOTE:")
print(table(smote_data$isFraud))

# Save balanced dataset
balanced_smote <- smote_data

# Model Building and Evaluation------------------------------------------------

# Function for model evaluation
evaluate_model <- function(predictions, actual, probabilities = NULL) {
  # Create confusion matrix
  conf_matrix <- confusionMatrix(predictions, actual)
  
  # Calculate metrics
  accuracy <- conf_matrix$overall["Accuracy"]
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Recall"]
  f1 <- conf_matrix$byClass["F1"]
  
  # Calculate ROC and AUC if probabilities are provided
  if (!is.null(probabilities)) {
    roc_obj <- roc(actual, probabilities)
    auc_value <- auc(roc_obj)
  } else {
    auc_value <- NA
  }
  
  return(list(
    confusion_matrix = conf_matrix,
    metrics = c(Accuracy = accuracy, 
                Precision = precision, 
                Recall = recall, 
                F1 = f1,
                AUC = auc_value)
  ))
}

# Prepare final training and testing sets using SMOTE balanced data
set.seed(123)
train_indices <- sample(1:nrow(balanced_smote), 0.8 * nrow(balanced_smote))
train_balanced <- balanced_smote[train_indices, ]
test_balanced <- balanced_smote[-train_indices, ]

# Convert target variable to factor
train_balanced$isFraud <- as.factor(train_balanced$isFraud)
test_balanced$isFraud <- as.factor(test_balanced$isFraud)

# Model 1: Random Forest-----------------------------------------------------
print("Training Random Forest Model...")

# Train Random Forest model
rf_model <- randomForest(isFraud ~ ., 
                         data = train_balanced,
                         ntree = 500,
                         mtry = sqrt(ncol(train_balanced)),
                         importance = TRUE)

# Make predictions
rf_pred <- predict(rf_model, test_balanced)
rf_prob <- predict(rf_model, test_balanced, type = "prob")[,2]

# Evaluate Random Forest
rf_evaluation <- evaluate_model(rf_pred, test_balanced$isFraud, rf_prob)

# Feature importance for Random Forest
rf_importance <- importance(rf_model)
print("\nRandom Forest Feature Importance:")
print(rf_importance)

# Plot feature importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# Model 2: Logistic Regression----------------------------------------------
print("\nTraining Logistic Regression Model...")

# Train Logistic Regression model
lr_model <- glm(isFraud ~ ., 
                data = train_balanced, 
                family = "binomial")

# Make predictions
lr_prob <- predict(lr_model, test_balanced, type = "response")
lr_pred <- as.factor(ifelse(lr_prob > 0.5, "1", "0"))

# Evaluate Logistic Regression
lr_evaluation <- evaluate_model(lr_pred, test_balanced$isFraud, lr_prob)

# Feature importance for Logistic Regression
lr_coefficients <- summary(lr_model)$coefficients
print("\nLogistic Regression Coefficients:")
print(lr_coefficients)

# Model Comparison---------------------------------------------------------
print("\nModel Comparison:")
comparison_df <- data.frame(
  Metric = names(rf_evaluation$metrics),
  RandomForest = rf_evaluation$metrics,
  LogisticRegression = lr_evaluation$metrics
)
print(comparison_df)

# Plot ROC curves for both models
plot(roc(test_balanced$isFraud, rf_prob), 
     main = "ROC Curves Comparison",
     col = "blue")
lines(roc(test_balanced$isFraud, lr_prob), 
      col = "red")
legend("bottomright", 
       legend = c("Random Forest", "Logistic Regression"),
       col = c("blue", "red"), 
       lwd = 2)

# Summary of findings-----------------------------------------------------
cat("\nModel Performance Summary:\n")
cat("1. Random Forest:\n")
cat("   - Accuracy:", round(rf_evaluation$metrics["Accuracy"], 4), "\n")
cat("   - F1 Score:", round(rf_evaluation$metrics["F1"], 4), "\n")
cat("   - AUC:", round(rf_evaluation$metrics["AUC"], 4), "\n")

cat("\n2. Logistic Regression:\n")
cat("   - Accuracy:", round(lr_evaluation$metrics["Accuracy"], 4), "\n")
cat("   - F1 Score:", round(lr_evaluation$metrics["F1"], 4), "\n")
cat("   - AUC:", round(lr_evaluation$metrics["AUC"], 4), "\n")