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
if (!require(corrplot)) install.packages("corrplot")

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
library(corrplot)

# Load the data
setwd("C:/Users/Bob/Desktop/FITE7410/Assign/assign1/materials") # This is the path of my working folder
data <- read.csv("A1_data.csv")

# Check if output folder exists, if not create it
if (!dir.exists("output")) {
  dir.create("output")
}

if (!dir.exists("output/summary")) {
  dir.create("output/summary")
}

# # Step 1: Distinguish Attributes ---------------------------------------------

# Save structure information
sink("output/summary/structure.txt")
str(data)
sink()

# Save summary statistics
sink("output/summary/summary.txt")
summary(data)
sink()

# Save class labels
sink("output/summary/class_labels.txt")
print("Fraud labels (0: Normal, 1: Fraud):")
print(table(data$isFraud))
sink()

# Save class percentages
sink("output/summary/class_percentages.txt")
print("Percentage of transactions:")
percentages <- round(prop.table(table(data$isFraud)) * 100, 2)
print(paste("Normal transactions (0):", percentages[1], "%"))
print(paste("Fraudulent transactions (1):", percentages[2], "%"))
sink()


# # Step 2: Univariate Analysis ------------------------------------------------

# Create output directory for plots if it doesn't exist
if (!dir.exists("./output/plot")) {
  dir.create("./output/plot", recursive = TRUE)
}

# Set theme for all plots
theme_set(theme_minimal() + 
            theme(plot.title = element_text(size = 12, face = "bold"),
                  axis.title = element_text(size = 10),
                  legend.position = "bottom"))

## 1. Fraud Distribution Analysis
p1 <- ggplot(data, aes(x = factor(isFraud), fill = factor(isFraud))) + 
  geom_bar() + 
  scale_fill_manual(values = c("0" = "skyblue", "1" = "coral")) +
  labs(title = "Distribution of Fraud Cases", 
       x = "Is Fraud", 
       y = "Count",
       fill = "Fraud Status") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), 
            vjust = -0.5)

fraud_summary <- data %>%
  group_by(isFraud) %>%
  summarise(
    count = n(),
    percentage = n() / nrow(data) * 100,
    avg_transaction = mean(TransactionAmt, na.rm = TRUE),
    median_transaction = median(TransactionAmt, na.rm = TRUE),
    min_transaction = min(TransactionAmt, na.rm = TRUE),
    max_transaction = max(TransactionAmt, na.rm = TRUE)
  )

print("fraud_summary:")
print(fraud_summary)

## 2. Transaction Amount Analysis
p2 <- ggplot(data, aes(x = TransactionAmt)) + 
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) + 
  scale_x_log10() +
  labs(title = "Distribution of Transaction Amounts (Log Scale)", 
       x = "Transaction Amount (log10)", 
       y = "Frequency")

transaction_stats <- data %>%
  group_by(isFraud) %>%
  summarise(
    mean_amt = mean(TransactionAmt, na.rm = TRUE),
    median_amt = median(TransactionAmt, na.rm = TRUE),
    q1_amt = quantile(TransactionAmt, 0.25, na.rm = TRUE),
    q3_amt = quantile(TransactionAmt, 0.75, na.rm = TRUE),
    std_amt = sd(TransactionAmt, na.rm = TRUE)
  )

print("transaction_stats:")
print(transaction_stats)

## 3. Transaction Amount by Fraud Status
p3 <- ggplot(data, aes(x = factor(isFraud), y = TransactionAmt, fill = factor(isFraud))) + 
  geom_boxplot() + 
  scale_y_log10() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "coral")) +
  labs(title = "Transaction Amount by Fraud Status", 
       x = "Is Fraud", 
       y = "Transaction Amount (log10)",
       fill = "Fraud Status")

## 4. Transaction Hour Analysis
p4 <- ggplot(data, aes(x = TransactionDT..Hour. %% 24)) + 
  geom_histogram(bins = 24, fill = "darkgreen", alpha = 0.7) + 
  labs(title = "Transaction Frequency by Hour of Day", 
       x = "Hour of Day", 
       y = "Frequency")

# Save the combined plot
combined_plot1 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave("./output/plot/fraud_transaction_overview.png", combined_plot1, width = 12, height = 10)

## 5. Card Information Analysis
card_vars <- data %>% select(card1, card2, card3, card5) %>% na.omit()

p5 <- ggplot(card_vars, aes(x = card1)) + 
  geom_histogram(bins = 30, fill = "purple", alpha = 0.7) + 
  labs(title = "Distribution of Card1 Values", 
       x = "Card1", 
       y = "Frequency")

p6 <- ggplot(card_vars, aes(x = card2)) + 
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) + 
  labs(title = "Distribution of Card2 Values", 
       x = "Card2", 
       y = "Frequency")

p7 <- ggplot(card_vars, aes(x = card3)) + 
  geom_histogram(bins = 30, fill = "brown", alpha = 0.7) + 
  labs(title = "Distribution of Card3 Values", 
       x = "Card3", 
       y = "Frequency")

p8 <- ggplot(card_vars, aes(x = card5)) + 
  geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) + 
  labs(title = "Distribution of Card5 Values", 
       x = "Card5", 
       y = "Frequency")

# Save the combined card plots
combined_plot2 <- grid.arrange(p5, p6, p7, p8, ncol = 2)
ggsave("./output/plot/card_features_distribution.png", combined_plot2, width = 12, height = 10)

## 6. Email Domain Analysis
# Extract top email domains from P_emaildomain
top_p_domains <- data %>% 
  filter(!is.na(P_emaildomain) & P_emaildomain != "") %>%
  count(P_emaildomain) %>% 
  arrange(desc(n)) %>% 
  head(10)

p9 <- ggplot(top_p_domains, aes(x = reorder(P_emaildomain, n), y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +
  labs(title = "Top 10 P_emaildomains", 
       x = "Email Domain", 
       y = "Count")

# Extract top email domains from R_emaildomain
top_r_domains <- data %>% 
  filter(!is.na(R_emaildomain) & R_emaildomain != "") %>%
  count(R_emaildomain) %>% 
  arrange(desc(n)) %>% 
  head(10)

p10 <- ggplot(top_r_domains, aes(x = reorder(R_emaildomain, n), y = n)) + 
  geom_bar(stat = "identity", fill = "coral") + 
  coord_flip() +
  labs(title = "Top 10 R_emaildomains", 
       x = "Email Domain", 
       y = "Count")

email_fraud_analysis <- data %>%
  filter(!is.na(P_emaildomain) & P_emaildomain != "") %>%
  group_by(P_emaildomain) %>%
  summarise(
    total_count = n(),
    fraud_count = sum(isFraud),
    fraud_rate = mean(isFraud) * 100
  ) %>%
  filter(total_count >= 50) %>% # 只分析有足够样本的域名
  arrange(desc(fraud_rate)) %>%
  head(10)

print("email_fraud_analysis:")
print(email_fraud_analysis)

## 7. Product Category Distribution
p11 <- ggplot(data, aes(x = ProductCD)) + 
  geom_bar(fill = "darkgreen") + 
  labs(title = "Distribution of Product Categories", 
       x = "Product Category", 
       y = "Count")

product_fraud_detailed <- data %>%
  group_by(ProductCD) %>%
  summarise(
    total_count = n(),
    fraud_count = sum(isFraud),
    fraud_rate = mean(isFraud) * 100,
    avg_transaction = mean(TransactionAmt, na.rm = TRUE)
  ) %>%
  arrange(desc(fraud_rate))

print("product_fraud:")
print(product_fraud_detailed)

## 8. Fraud Rate by Product Category
fraud_by_product <- data %>%
  group_by(ProductCD) %>%
  summarise(
    fraud_rate = mean(isFraud) * 100,
    count = n()
  )

p12 <- ggplot(fraud_by_product, aes(x = ProductCD, y = fraud_rate)) + 
  geom_bar(stat = "identity", fill = "darkred") + 
  labs(title = "Fraud Rate by Product Category", 
       x = "Product Category", 
       y = "Fraud Rate (%)") +
  geom_text(aes(label = sprintf("%.2f%%", fraud_rate)), vjust = -0.5)

# Save the combined email and product plots
combined_plot3 <- grid.arrange(p9, p10, p11, p12, ncol = 2)
ggsave("./output/plot/email_product_analysis.png", combined_plot3, width = 12, height = 10)

## 9. C1-C14 Features Distribution
# Select a subset of C columns for visualization
c_vars <- data %>% select(C1, C2, C3, C4, C5, C6)

# Create a melted dataframe for easier plotting
c_vars_melted <- melt(c_vars)

p13 <- ggplot(c_vars_melted, aes(x = value)) + 
  geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) + 
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Selected C Features", 
       x = "Value", 
       y = "Frequency") +
  theme(strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(face = "bold"))

c_stats <- data %>%
  select(C1, C2, C3, C4, C5, C6) %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    q25 = ~quantile(., 0.25, na.rm = TRUE),
    q75 = ~quantile(., 0.75, na.rm = TRUE)
  ))
print("C Variables Detailed Statistics:")
print(c_stats)

## 10. D1-D8 Features Distribution
# Select a subset of D columns for visualization
d_vars <- data %>% select(D1, D2, D3, D4, D5, D6) %>% na.omit()

# Create a melted dataframe for easier plotting
d_vars_melted <- melt(d_vars)

p14 <- ggplot(d_vars_melted, aes(x = value)) + 
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) + 
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Selected D Features", 
       x = "Value", 
       y = "Frequency") +
  theme(strip.background = element_rect(fill = "lightgreen"),
        strip.text = element_text(face = "bold"))

# Save the combined feature plots
combined_plot4 <- grid.arrange(p13, p14, ncol = 1)
ggsave("./output/plot/features_distribution.png", combined_plot4, width = 12, height = 12)

d_stats <- data %>%
  select(D1, D2, D3, D4, D5, D6) %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    q25 = ~quantile(., 0.25, na.rm = TRUE),
    q75 = ~quantile(., 0.75, na.rm = TRUE)
  ))
print("D Variables Detailed Statistics:")
print(d_stats)

## 11. Missing Value Analysis
# Calculate the percentage of missing values in each column
missing_values <- data %>%
  summarise_all(~sum(is.na(.))/n()*100) %>%
  gather(key = "variable", value = "percent_missing") %>%
  arrange(desc(percent_missing)) %>%
  filter(percent_missing > 0) %>%
  head(20)  # Top 20 variables with missing values

p15 <- ggplot(missing_values, aes(x = reorder(variable, percent_missing), y = percent_missing)) + 
  geom_bar(stat = "identity", fill = "firebrick") + 
  coord_flip() +
  labs(title = "Top 20 Variables with Missing Values", 
       x = "Variable", 
       y = "Percentage Missing (%)") +
  theme(axis.text.y = element_text(size = 8))

## 12. Correlation Analysis of Numeric Features
# Select a subset of numeric features for correlation analysis
numeric_vars <- data %>% 
  select(TransactionAmt, card1, card2, card3, card5, C1, C2, C3, C4, C5, C6) %>%
  na.omit()

# Calculate correlation matrix
corr_matrix <- cor(numeric_vars)

# Convert the correlation matrix to a data frame for ggplot2
corr_df <- melt(corr_matrix)

p16 <- ggplot(corr_df, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Matrix of Numeric Features",
       x = "", y = "", fill = "Correlation")

# Save the combined analysis plots
combined_plot5 <- grid.arrange(p15, p16, ncol = 1)
ggsave("./output/plot/missing_correlation_analysis.png", combined_plot5, width = 12, height = 12)

## 13. Device Type and Device Info Analysis
p17 <- ggplot(data %>% filter(!is.na(DeviceType)), aes(x = DeviceType)) + 
  geom_bar(fill = "darkblue") + 
  labs(title = "Distribution of Device Types", 
       x = "Device Type", 
       y = "Count")

# Get top 10 Device Info values
top_devices <- data %>% 
  filter(!is.na(DeviceInfo)) %>%
  count(DeviceInfo) %>% 
  arrange(desc(n)) %>% 
  head(10)

p18 <- ggplot(top_devices, aes(x = reorder(DeviceInfo, n), y = n)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  coord_flip() +
  labs(title = "Top 10 Device Info Values", 
       x = "Device Info", 
       y = "Count")

device_fraud_detailed <- data %>%
  filter(!is.na(DeviceType)) %>%
  group_by(DeviceType) %>%
  summarise(
    total_count = n(),
    fraud_count = sum(isFraud),
    fraud_rate = mean(isFraud) * 100,
    avg_transaction = mean(TransactionAmt, na.rm = TRUE)
  ) %>%
  arrange(desc(fraud_rate))

print("device_fraud:")
print(device_fraud_detailed)

## 14. Fraud Rate by Device Type
fraud_by_device <- data %>%
  filter(!is.na(DeviceType)) %>%
  group_by(DeviceType) %>%
  summarise(
    fraud_rate = mean(isFraud) * 100,
    count = n()
  )

p19 <- ggplot(fraud_by_device, aes(x = DeviceType, y = fraud_rate)) + 
  geom_bar(stat = "identity", fill = "darkred") + 
  labs(title = "Fraud Rate by Device Type", 
       x = "Device Type", 
       y = "Fraud Rate (%)") +
  geom_text(aes(label = sprintf("%.2f%%", fraud_rate)), vjust = -0.5)

## 15. Card6 Distribution
p20 <- ggplot(data %>% filter(!is.na(card6)), aes(x = card6)) + 
  geom_bar(fill = "purple") + 
  labs(title = "Distribution of Card6 Values", 
       x = "Card6", 
       y = "Count")

# Save the combined device and card plots
combined_plot6 <- grid.arrange(p17, p18, p19, p20, ncol = 2)
ggsave("./output/plot/device_card_analysis.png", combined_plot6, width = 12, height = 10)

## 16. Fraud Analysis by Transaction Hour
fraud_by_hour <- data %>%
  mutate(hour = TransactionDT..Hour. %% 24) %>%
  group_by(hour) %>%
  summarise(
    fraud_rate = mean(isFraud) * 100,
    count = n()
  )

p21 <- ggplot(fraud_by_hour, aes(x = hour, y = fraud_rate)) + 
  geom_line(color = "darkred", size = 1) + 
  geom_point(color = "darkred", size = 2) +
  labs(title = "Fraud Rate by Hour of Day", 
       x = "Hour of Day", 
       y = "Fraud Rate (%)") +
  scale_x_continuous(breaks = seq(0, 23, by = 2))

p22 <- ggplot(fraud_by_hour, aes(x = hour, y = count)) + 
  geom_line(color = "darkblue", size = 1) + 
  geom_point(color = "darkblue", size = 2) +
  labs(title = "Transaction Count by Hour of Day", 
       x = "Hour of Day", 
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 23, by = 2))

# Combined fraud time analysis
combined_plot7 <- grid.arrange(p21, p22, ncol = 1)
ggsave("./output/plot/fraud_by_time.png", combined_plot7, width = 10, height = 8)

high_risk_hours <- fraud_by_hour %>%
  arrange(desc(fraud_rate)) %>%
  head(5)

print(high_risk_hours)

# Final summary of univariate analysis
cat("Univariate analysis completed. All plots saved to ./output/plot/ directory.")


# # Step 3: Bi-/Multi-variate Analysis -----------------------------------------

# Create output folders for plots if they don't exist
if (!dir.exists("output/plot")) {
  dir.create("output/plot")
}

# Load necessary libraries if not already loaded
library(ggplot2)
library(corrplot)
library(reshape2)
library(gridExtra)
library(dplyr)

# Get only numeric columns
numeric_columns <- sapply(data, is.numeric)
numeric_column_names <- colnames(data)[numeric_columns]

# Print out numeric columns for verification
print("Numeric columns found:")
print(numeric_column_names)

# Check if required columns exist
if ("isFraud" %in% numeric_column_names) {
  # For feature selection, let's exclude columns with zero variance
  var_zero <- sapply(data[numeric_column_names], function(x) var(x, na.rm = TRUE) == 0)
  non_zero_var_columns <- numeric_column_names[!var_zero]
  
  cat("Found", length(non_zero_var_columns), "numeric columns with non-zero variance\n")
  
  # Subset data to these columns
  numeric_data <- data[, non_zero_var_columns]
  
  # Check for NA values
  na_count <- colSums(is.na(numeric_data))
  print("Columns with NA values:")
  print(na_count[na_count > 0])
  
  # Let's select columns with less than 30% missing values for correlation analysis
  na_percentage <- colSums(is.na(numeric_data)) / nrow(numeric_data)
  good_columns <- names(na_percentage[na_percentage < 0.3])
  
  cat("Using", length(good_columns), "columns with less than 30% missing values\n")
  
  # Focus on specific feature groups that are likely to be important
  # We'll look at C and D features based on your previous analysis
  c_cols <- grep("^C[0-9]", good_columns, value = TRUE)
  d_cols <- grep("^D[0-9]", good_columns, value = TRUE)
  
  # Add isFraud for target correlation
  analysis_cols <- c("isFraud", c_cols, d_cols)
  analysis_cols <- intersect(analysis_cols, good_columns) # Ensure all columns exist
  
  cat("Analyzing", length(analysis_cols), "columns: isFraud and C/D features\n")
  
  # Use complete.cases to get rows with no missing values
  analysis_data <- numeric_data[, analysis_cols]
  complete_rows <- complete.cases(analysis_data)
  analysis_data <- analysis_data[complete_rows, ]
  
  cat("Using", nrow(analysis_data), "complete rows for analysis\n")
  
  # Only proceed if we have sufficient data
  if (nrow(analysis_data) > 10 && ncol(analysis_data) > 1) {
    # Calculate correlation matrix
    correlation_matrix <- cor(analysis_data)
    
    # Save correlation with fraud to text file
    if ("isFraud" %in% colnames(correlation_matrix)) {
      fraud_correlations <- correlation_matrix[,"isFraud"]
      fraud_correlations <- sort(fraud_correlations, decreasing = TRUE)
      sink("output/summary/fraud_correlations.txt")
      print("Correlations with Fraud Label:")
      print(fraud_correlations)
      sink()
      
      # Visualize correlation matrix
      png("output/plot/correlation_matrix.png", width = 1200, height = 1000)
      corrplot(correlation_matrix, method = "color", type = "upper", 
               order = "hclust", tl.col = "black", tl.srt = 45,
               title = "Correlation Matrix of Key Features", 
               mar = c(0,0,2,0))
      dev.off()
      
      ## Bivariate analysis --------------------------------------------------------
      
      # Most correlated features with fraud (based on correlation matrix)
      # Get top 5 positively correlated and top 5 negatively correlated (exclude self-correlation)
      fraud_correlations_no_self <- fraud_correlations[names(fraud_correlations) != "isFraud"]
      top_pos_corr <- names(head(fraud_correlations_no_self[fraud_correlations_no_self > 0], 5))
      top_neg_corr <- names(tail(fraud_correlations_no_self, 5))
      
      # Create directory for boxplots if it doesn't exist
      if (!dir.exists("output/plot/boxplots")) {
        dir.create("output/plot/boxplots")
      }
      
      # Boxplots of top correlated features by fraud status
      for (feature in c(top_pos_corr, top_neg_corr)) {
        if (feature != "isFraud") {
          png(paste0("output/plot/boxplots/boxplot_", feature, "_by_fraud.png"), width = 800, height = 600)
          # Use the original data for boxplots, not just complete cases
          feature_data <- data.frame(
            feature_val = data[[feature]],
            isFraud = data$isFraud
          )
          feature_data <- na.omit(feature_data)
          
          boxplot <- ggplot(feature_data, aes(x = as.factor(isFraud), y = feature_val, fill = as.factor(isFraud))) + 
            geom_boxplot() +
            scale_fill_manual(values = c("0" = "green", "1" = "red"),
                              labels = c("0" = "Normal", "1" = "Fraud"),
                              name = "Transaction Type") +
            labs(title = paste("Distribution of", feature, "by Fraud Status"),
                 x = "Fraud Status",
                 y = feature) +
            theme_minimal()
          print(boxplot)
          dev.off()
        }
      }
      
      # Create directory for scatter plots if it doesn't exist
      if (!dir.exists("output/plot/scatterplots")) {
        dir.create("output/plot/scatterplots")
      }
      
      # Create scatter plots for pairs of highly correlated features
      # Get the top correlated pairs excluding self-correlations
      corr_matrix_no_diag <- correlation_matrix
      diag(corr_matrix_no_diag) <- 0
      corr_data <- melt(corr_matrix_no_diag)
      colnames(corr_data) <- c("Feature1", "Feature2", "Correlation")
      corr_data <- corr_data[order(abs(corr_data$Correlation), decreasing = TRUE), ]
      top_pairs <- head(corr_data, 10)
      
      # Create scatter plots for top pairs
      for (i in 1:min(10, nrow(top_pairs))) {
        feat1 <- as.character(top_pairs$Feature1[i])
        feat2 <- as.character(top_pairs$Feature2[i])
        if (feat1 != "isFraud" && feat2 != "isFraud") {
          png(paste0("output/plot/scatterplots/scatter_", feat1, "_vs_", feat2, ".png"), width = 800, height = 600)
          
          # Create data frame for scatter plot
          scatter_data <- data.frame(
            feat1_val = data[[feat1]],
            feat2_val = data[[feat2]],
            isFraud = data$isFraud
          )
          scatter_data <- na.omit(scatter_data)
          
          # If there are too many points, sample a subset
          if (nrow(scatter_data) > 5000) {
            set.seed(123) # for reproducibility
            scatter_data <- scatter_data[sample(nrow(scatter_data), 5000), ]
          }
          
          scatter <- ggplot(scatter_data, aes(x = feat1_val, y = feat2_val, color = as.factor(isFraud))) +
            geom_point(alpha = 0.5) +
            scale_color_manual(values = c("0" = "green", "1" = "red"),
                               labels = c("0" = "Normal", "1" = "Fraud"),
                               name = "Transaction Type") +
            labs(title = paste("Relationship between", feat1, "and", feat2),
                 subtitle = paste("Correlation:", round(top_pairs$Correlation[i], 3)),
                 x = feat1, y = feat2) +
            theme_minimal()
          print(scatter)
          dev.off()
        }
      }
      
      # Add this code just after calculating fraud_correlations
      print("Top positive correlations with fraud:")
      print(head(fraud_correlations[fraud_correlations < 1], 5))
      
      print("Top negative correlations with fraud:")
      print(tail(fraud_correlations, 5))
      
      print("Top correlated feature pairs:")
      print(head(top_pairs, 5))
      
      ## Multivariate analysis -----------------------------------------------------
      
      # PCA analysis to explore feature relationships
      # Select numeric features for PCA (exclude the target variable)
      pca_features <- analysis_data %>% select(-isFraud)
      
      # Only perform PCA if we have enough features
      if (ncol(pca_features) > 1) {
        # Perform PCA
        pca_result <- prcomp(pca_features, scale. = TRUE)
        
        # Save PCA summary to file
        sink("output/summary/pca_summary.txt")
        print(summary(pca_result))
        sink()
        
        # Create PCA biplot
        png("output/plot/pca_biplot.png", width = 1000, height = 800)
        biplot(pca_result, scale = 0, cex = 0.7,
               arrow.len = 0.1,
               main = "PCA Biplot of Transaction Features")
        dev.off()
        
        # Visualize PCA with fraud labels
        pca_data <- as.data.frame(pca_result$x[,1:2])
        pca_data$isFraud <- analysis_data$isFraud
        
        # If there are too many points, sample a subset for plotting
        if (nrow(pca_data) > 5000) {
          set.seed(123) # for reproducibility
          sample_indices <- sample(nrow(pca_data), 5000)
          pca_plot_data <- pca_data[sample_indices, ]
        } else {
          pca_plot_data <- pca_data
        }
        
        png("output/plot/pca_with_fraud.png", width = 800, height = 700)
        pca_plot <- ggplot(pca_plot_data, aes(x = PC1, y = PC2, color = as.factor(isFraud))) +
          geom_point(alpha = 0.5) +
          scale_color_manual(values = c("0" = "green", "1" = "red"),
                             labels = c("0" = "Normal", "1" = "Fraud"),
                             name = "Transaction Type") +
          labs(title = "PCA of Transaction Features",
               subtitle = "Colored by Fraud Status",
               x = paste0("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "% variance)"),
               y = paste0("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "% variance)")) +
          theme_minimal()
        print(pca_plot)
        dev.off()
        
        # Save PCA loadings for interpretation
        sink("output/summary/pca_loadings.txt")
        print("PCA Loadings (Feature Contributions to Principal Components):")
        # Print first 5 PCs or fewer if there aren't 5
        n_pcs <- min(5, ncol(pca_result$rotation))
        print(pca_result$rotation[,1:n_pcs])
        sink()
        
        # Identify potentially important features through PCA loadings
        top_loadings <- data.frame(
          feature = rownames(pca_result$rotation),
          PC1_loading = abs(pca_result$rotation[,1]),
          PC2_loading = abs(pca_result$rotation[,2])
        )
        top_loadings$total <- top_loadings$PC1_loading + top_loadings$PC2_loading
        top_loadings <- top_loadings[order(top_loadings$total, decreasing = TRUE),]
        
        sink("output/summary/top_pca_features.txt")
        print("Top Features Contributing to PC1 and PC2:")
        print(head(top_loadings, 10))
        sink()
        
        # Generate a report summary
        sink("output/summary/bivariate_multivariate_analysis.txt")
        cat("# Bivariate and Multivariate Analysis Summary\n\n")
        
        cat("## Top Features Correlated with Fraud\n")
        cat("Positive correlations:\n")
        for(feature in top_pos_corr) {
          cat(paste0("- ", feature, ": ", round(fraud_correlations[feature], 4), "\n"))
        }
        cat("\nNegative correlations:\n")
        for(feature in top_neg_corr) {
          cat(paste0("- ", feature, ": ", round(fraud_correlations[feature], 4), "\n"))
        }
        
        cat("\n## Top Correlated Feature Pairs\n")
        for(i in 1:min(10, nrow(top_pairs))) {
          if(top_pairs$Feature1[i] != "isFraud" && top_pairs$Feature2[i] != "isFraud") {
            cat(paste0("- ", top_pairs$Feature1[i], " & ", top_pairs$Feature2[i], 
                       ": ", round(top_pairs$Correlation[i], 4), "\n"))
          }
        }
        
        cat("\n## PCA Analysis\n")
        cat(paste0("- PC1 explains ", round(summary(pca_result)$importance[2,1] * 100, 1), "% of variance\n"))
        cat(paste0("- PC2 explains ", round(summary(pca_result)$importance[2,2] * 100, 1), "% of variance\n"))
        cat(paste0("- PC1 + PC2 explains ", round(sum(summary(pca_result)$importance[2,1:2]) * 100, 1), "% of total variance\n"))
        
        cat("\n## Top Features in PCA (by contribution to PC1 and PC2)\n")
        for(i in 1:min(5, nrow(top_loadings))) {
          cat(paste0("- ", top_loadings$feature[i], " (Total loading: ", round(top_loadings$total[i], 4), ")\n"))
        }
        
        cat("\n## Conclusions\n")
        cat("- The analysis identified key features strongly associated with fraudulent transactions\n")
        cat("- Several feature pairs show strong correlations, suggesting potential redundancy\n")
        cat("- PCA reveals underlying patterns and helps reduce dimensionality while preserving information\n")
        cat("- The identified top features should be prioritized in fraud detection model development\n")
        sink()
      } else {
        cat("Not enough features for PCA analysis (need at least 2 numeric features)\n")
      }
    } else {
      cat("isFraud not found in correlation matrix. Check your data.\n")
    }
  } else {
    cat("Not enough complete data for correlation analysis. Try imputing missing values or selecting different features.\n")
  }
} else {
  cat("isFraud not found in numeric columns. Check your data.\n")
}

print("Bivariate and Multivariate Analysis complete!")


# Step 4: Detect erroneous & missing values ------------------------------------
# This section focuses on identifying and handling missing values and outliers in the dataset

## Create necessary directories first
dir.create("output", showWarnings = FALSE)
dir.create("output/clean", showWarnings = FALSE, recursive = TRUE)

## Missing Values Treatment --------------------------------------------------
# Implementing systematic approaches to handle missing data based on column types

# Function to calculate mode - the most frequently occurring value
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to print missing value summary with detailed statistics
print_missing_summary <- function(data, title = "Missing Values Summary") {
  cat("\n", title, "\n", paste(rep("-", nchar(title)), collapse = ""), "\n")
  
  # Calculate missing values for each column
  missing_summary <- data.frame(
    Column = names(data),
    Missing_Count = colSums(is.na(data)),
    Missing_Percentage = round(colMeans(is.na(data)) * 100, 2)
  )
  
  # Sort by missing percentage for better visualization
  missing_summary <- missing_summary[order(-missing_summary$Missing_Percentage), ]
  
  # Print summary
  print(missing_summary)
  
  return(missing_summary)
}

# Initial missing values analysis to understand the scope of the problem
initial_missing <- print_missing_summary(data, "Initial Missing Values Analysis")

# Save initial missing values analysis for documentation
write.csv(initial_missing, "output/clean/initial_missing_analysis.csv", row.names = FALSE)

# Calculate missing value percentages for each column to determine removal candidates
missing_percentages <- colMeans(is.na(data)) * 100
high_missing_cols <- names(missing_percentages[missing_percentages > 50])

# Remove columns with more than 50% missing values - these are too sparse to be useful
data_clean <- data[, !names(data) %in% high_missing_cols]

# Identify column types for type-appropriate imputation strategies
numeric_cols <- names(data_clean)[sapply(data_clean, is.numeric)]
categorical_cols <- names(data_clean)[sapply(data_clean, is.character)]

# Create detailed missing value treatment report for documentation
sink("output/clean/missing_values_treatment.txt")
cat("Missing Values Treatment Report\n")
cat("============================\n\n")

cat("1. Columns removed (>50% missing):\n")
if(length(high_missing_cols) > 0) {
  for(col in high_missing_cols) {
    cat(sprintf("- %s (%.2f%% missing)\n", col, missing_percentages[col]))
  }
} else {
  cat("None\n")
}

cat("\n2. Numeric Columns Treatment:\n")
for(col in numeric_cols) {
  missing_count <- sum(is.na(data_clean[[col]]))
  if(missing_count > 0) {
    median_val <- median(data_clean[[col]], na.rm = TRUE)
    data_clean[[col]][is.na(data_clean[[col]])] <- median_val
    cat(sprintf("- %s: %d missing values imputed with median (%.2f)\n",
                col, missing_count, median_val))
  }
}

cat("\n3. Categorical Columns Treatment:\n")
for(col in categorical_cols) {
  missing_count <- sum(is.na(data_clean[[col]]) | data_clean[[col]] == "")
  if(missing_count > 0) {
    mode_val <- get_mode(data_clean[[col]][!is.na(data_clean[[col]]) & data_clean[[col]] != ""])
    data_clean[[col]][is.na(data_clean[[col]]) | data_clean[[col]] == ""] <- mode_val
    cat(sprintf("- %s: %d missing values imputed with mode (%s)\n",
                col, missing_count, mode_val))
  }
}

# Final missing values check to verify successful treatment
final_missing <- print_missing_summary(data_clean, "\n4. Final Missing Values Check")
sink()

# Additional data quality checks for more comprehensive cleaning
# Check for infinite values which can cause computational issues
inf_check <- sapply(data_clean[numeric_cols], function(x) sum(is.infinite(x)))
if(any(inf_check > 0)) {
  cat("\nWarning: Infinite values found in the following columns:\n")
  print(inf_check[inf_check > 0])
  
  # Replace infinite values with NA and then impute with median
  for(col in names(inf_check[inf_check > 0])) {
    data_clean[[col]][is.infinite(data_clean[[col]])] <- NA
    median_val <- median(data_clean[[col]], na.rm = TRUE)
    data_clean[[col]][is.na(data_clean[[col]])] <- median_val
  }
}

# Check for constant columns (zero variance) which provide no predictive value
constant_cols <- sapply(data_clean, function(x) length(unique(x)) == 1)
if(any(constant_cols)) {
  cat("\nWarning: The following columns have constant values:\n")
  print(names(data_clean)[constant_cols])
}

# Save cleaned data after missing value treatment
write.csv(data_clean, "output/clean/cleaned_data_missing_treated.csv", row.names = FALSE)

# Print summary of changes to quantify the cleaning impact
cat("\nData Cleaning Summary:\n")
cat("Original dimensions:", dim(data)[1], "rows,", dim(data)[2], "columns\n")
cat("Cleaned dimensions:", dim(data_clean)[1], "rows,", dim(data_clean)[2], "columns\n")

# Create visual summary of missing data and treatment process
library(ggplot2)
library(reshape2)

# 1. Generate visualizations to show missing data before treatment
# Create a dataframe for the missingness categories
missing_categories <- data.frame(
  Missing_Range = c("No Missing (0%)", "Low (0-10%)", "Medium (10-30%)", "High (30-50%)", "Very High (>50%)"),
  Count_Before = c(
    sum(missing_percentages == 0),
    sum(missing_percentages > 0 & missing_percentages <= 10),
    sum(missing_percentages > 10 & missing_percentages <= 30),
    sum(missing_percentages > 30 & missing_percentages <= 50),
    sum(missing_percentages > 50)
  )
)

# Calculate missing percentages after treatment
missing_percentages_after <- colMeans(is.na(data_clean)) * 100
missing_categories$Count_After <- c(
  sum(missing_percentages_after == 0),
  sum(missing_percentages_after > 0 & missing_percentages_after <= 10),
  sum(missing_percentages_after > 10 & missing_percentages_after <= 30),
  sum(missing_percentages_after > 30 & missing_percentages_after <= 50),
  sum(missing_percentages_after > 50)
)

# Convert to long format for easier plotting
missing_long <- melt(missing_categories, id.vars = "Missing_Range", 
                     variable.name = "Stage", value.name = "Count")
missing_long$Stage <- ifelse(missing_long$Stage == "Count_Before", "Before Treatment", "After Treatment")

# Create bar chart comparing missing values before and after treatment
plot_missing <- ggplot(missing_long, aes(x = Missing_Range, y = Count, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Missing Values Distribution Before and After Treatment",
       x = "Missing Value Percentage Range",
       y = "Number of Variables",
       fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("output/clean/missing_values_comparison.png", plot_missing, width = 10, height = 6)

# 2. Create pie chart showing the proportion of different imputation methods
imputation_methods <- data.frame(
  Method = c("Removed (>50% missing)", "Median Imputation", "Mode Imputation", "No Imputation Needed"),
  Count = c(
    length(high_missing_cols),
    sum(sapply(numeric_cols, function(col) sum(is.na(data[[col]])) > 0)),
    sum(sapply(categorical_cols, function(col) sum(is.na(data[[col]]) | data[[col]] == "") > 0)),
    ncol(data) - length(high_missing_cols) - 
      sum(sapply(numeric_cols, function(col) sum(is.na(data[[col]])) > 0)) -
      sum(sapply(categorical_cols, function(col) sum(is.na(data[[col]]) | data[[col]] == "") > 0))
  )
)

# Create pie chart of imputation methods
plot_imputation <- ggplot(imputation_methods, aes(x = "", y = Count, fill = Method)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribution of Missing Value Treatment Methods",
       fill = "Method") +
  geom_text(aes(label = paste0(Count, "\n(", round(Count/sum(Count)*100, 1), "%)")), 
            position = position_stack(vjust = 0.5))

# Save the plot
ggsave("output/clean/imputation_methods_pie.png", plot_imputation, width = 8, height = 6)

# Step 5: Outlier detection and treatment ------------------------------------
# This section identifies and handles extreme values that could distort analysis

# Function to truncate values based on percentiles
truncate_values <- function(x, lower_percentile = 0.01, upper_percentile = 0.99) {
  lower <- quantile(x, lower_percentile)
  upper <- quantile(x, upper_percentile)
  return(list(
    lower = lower,
    upper = upper,
    values = pmin(pmax(x, lower), upper)
  ))
}

# Create a dataframe to store outlier treatment results for visualization
outlier_summary <- data.frame(
  Variable = character(),
  Method = character(),
  Lower_Bound = numeric(),
  Upper_Bound = numeric(),
  Outliers_Count = numeric(),
  stringsAsFactors = FALSE
)

# Store before-after statistics for key variables
before_after_stats <- data.frame(
  Variable = character(),
  Before_Min = numeric(),
  Before_Max = numeric(),
  Before_Mean = numeric(),
  Before_SD = numeric(),
  After_Min = numeric(),
  After_Max = numeric(),
  After_Mean = numeric(),
  After_SD = numeric(),
  stringsAsFactors = FALSE
)

# Process each numeric column for outlier detection and treatment
for(col in numeric_cols) {
  # Skip ID and binary columns
  if(!grepl("ID$|^is", col, ignore.case = TRUE) && length(unique(data_clean[[col]])) > 2) {
    cat("\n=== Processing Variable:", col, "===\n")
    
    # Store original statistics for comparison
    before_stats <- c(
      col,
      min(data_clean[[col]], na.rm = TRUE),
      max(data_clean[[col]], na.rm = TRUE),
      mean(data_clean[[col]], na.rm = TRUE),
      sd(data_clean[[col]], na.rm = TRUE)
    )
    
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
    iqr_outliers <- sum(data_clean[[col]] < iqr_lower | data_clean[[col]] > iqr_upper)
    
    # Add to summary dataframe
    outlier_summary <- rbind(outlier_summary, data.frame(
      Variable = col,
      Method = "IQR Method",
      Lower_Bound = iqr_lower,
      Upper_Bound = iqr_upper,
      Outliers_Count = iqr_outliers
    ))
    
    # 2. Truncation Method
    cat("\n2. Truncation Method Results:\n")
    trunc_result <- truncate_values(data_clean[[col]])
    trunc_outliers <- sum(data_clean[[col]] < trunc_result$lower | data_clean[[col]] > trunc_result$upper)
    
    # Add to summary dataframe
    outlier_summary <- rbind(outlier_summary, data.frame(
      Variable = col,
      Method = "Percentile Method",
      Lower_Bound = trunc_result$lower,
      Upper_Bound = trunc_result$upper,
      Outliers_Count = trunc_outliers
    ))
    
    # Print comparative summary
    cat("\nComparative Summary:")
    cat("\n------------------")
    cat("\nMedian:", M)
    cat("\nIQR:", IQR)
    cat("\n\nIQR Method Bounds:")
    cat("\n- Lower:", iqr_lower)
    cat("\n- Upper:", iqr_upper)
    cat("\n- Outliers:", iqr_outliers)
    
    cat("\n\nTruncation Method Bounds (1% and 99%):")
    cat("\n- Lower:", trunc_result$lower)
    cat("\n- Upper:", trunc_result$upper)
    cat("\n- Outliers:", trunc_outliers)
    
    # Combine both methods (using the more conservative bounds)
    final_lower <- max(iqr_lower, trunc_result$lower)
    final_upper <- min(iqr_upper, trunc_result$upper)
    
    # Apply final bounds
    data_clean[[col]] <- pmin(pmax(data_clean[[col]], final_lower), final_upper)
    final_outliers <- sum(data_clean[[col]] == final_lower | data_clean[[col]] == final_upper)
    
    # Add final method to summary
    outlier_summary <- rbind(outlier_summary, data.frame(
      Variable = col,
      Method = "Combined Method",
      Lower_Bound = final_lower,
      Upper_Bound = final_upper,
      Outliers_Count = final_outliers
    ))
    
    cat("\n\nFinal Combined Bounds:")
    cat("\n- Lower:", final_lower)
    cat("\n- Upper:", final_upper)
    cat("\n- Total modified values:", final_outliers)
    cat("\n==========================================\n")
    
    # Store after-treatment statistics
    after_stats <- c(
      min(data_clean[[col]], na.rm = TRUE),
      max(data_clean[[col]], na.rm = TRUE),
      mean(data_clean[[col]], na.rm = TRUE),
      sd(data_clean[[col]], na.rm = TRUE)
    )
    
    # Combine before and after statistics
    before_after_stats <- rbind(before_after_stats, 
                                data.frame(
                                  Variable = before_stats[1],
                                  Before_Min = as.numeric(before_stats[2]),
                                  Before_Max = as.numeric(before_stats[3]),
                                  Before_Mean = as.numeric(before_stats[4]),
                                  Before_SD = as.numeric(before_stats[5]),
                                  After_Min = as.numeric(after_stats[1]),
                                  After_Max = as.numeric(after_stats[2]),
                                  After_Mean = as.numeric(after_stats[3]),
                                  After_SD = as.numeric(after_stats[4])
                                ))
  }
}

# Save the cleaned data after outlier treatment
write.csv(data_clean, "output/clean/cleaned_data_combined_methods.csv", row.names = FALSE)

# Save outlier treatment summary
write.csv(outlier_summary, "output/clean/outlier_treatment_summary.csv", row.names = FALSE)

# Save before-after statistics
write.csv(before_after_stats, "output/clean/variable_statistics_comparison.csv", row.names = FALSE)

# Create and save visualization of outlier treatment effects
# Select a few representative variables for visualization
top_variables <- names(sort(table(outlier_summary$Variable), decreasing = TRUE)[1:5])
outlier_subset <- outlier_summary[outlier_summary$Variable %in% top_variables,]

# Create bar chart of outlier counts by method
plot_outliers <- ggplot(outlier_subset, aes(x = Variable, y = Outliers_Count, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Outlier Detection Comparison by Method",
       x = "Variable",
       y = "Number of Outliers Detected",
       fill = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("output/clean/outlier_detection_comparison.png", plot_outliers, width = 10, height = 6)

# Create detailed outlier treatment report
sink("output/clean/outlier_treatment_report.txt")
cat("Outlier Treatment Summary Report\n")
cat("===============================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("1. Outlier Detection Methods:\n")
cat("   - IQR Method: Using robust statistics with formula M ± 3 × IQR/(2 × 0.6745)\n")
cat("   - Percentile Method: Truncating values below 1st or above 99th percentile\n")
cat("   - Combined Method: Using the more conservative bounds from both methods\n\n")

cat("2. Variables with Most Outliers:\n")
top_outliers <- outlier_summary[outlier_summary$Method == "Combined Method",]
top_outliers <- top_outliers[order(-top_outliers$Outliers_Count),][1:5,]
for(i in 1:nrow(top_outliers)) {
  cat(sprintf("   - %s: %d outliers (%.2f%% of data)\n", 
              top_outliers$Variable[i], 
              top_outliers$Outliers_Count[i],
              top_outliers$Outliers_Count[i]/nrow(data_clean)*100))
}

cat("\n3. Statistical Impact of Outlier Treatment:\n")
for(i in 1:nrow(before_after_stats)) {
  cat(sprintf("\n   %s:\n", before_after_stats$Variable[i]))
  cat(sprintf("   - Before: Range [%.2f to %.2f], Mean = %.2f, SD = %.2f\n",
              before_after_stats$Before_Min[i],
              before_after_stats$Before_Max[i],
              before_after_stats$Before_Mean[i],
              before_after_stats$Before_SD[i]))
  cat(sprintf("   - After:  Range [%.2f to %.2f], Mean = %.2f, SD = %.2f\n",
              before_after_stats$After_Min[i],
              before_after_stats$After_Max[i],
              before_after_stats$After_Mean[i],
              before_after_stats$After_SD[i]))
  
  # Calculate percent change in standard deviation
  sd_change <- (before_after_stats$After_SD[i] - before_after_stats$Before_SD[i]) / before_after_stats$Before_SD[i] * 100
  cat(sprintf("   - Impact: SD reduced by %.1f%%\n", abs(sd_change)))
}
sink()

# Create comprehensive cleaning summary with visualizations
sink("output/clean/cleaning_summary.txt")
cat("Data Cleaning Summary Report\n")
cat("==========================\n\n")

cat("1. Missing Values Treatment\n")
cat("-------------------------\n")
cat("- Removed", length(high_missing_cols), "columns with >50% missing values\n")
cat("- Applied median imputation to", sum(sapply(numeric_cols, function(col) sum(is.na(data[[col]])) > 0)), "numeric columns\n")
cat("- Applied mode imputation to", sum(sapply(categorical_cols, function(col) sum(is.na(data[[col]]) | data[[col]] == "") > 0)), "categorical columns\n")

cat("\n2. Outlier Treatment\n")
cat("------------------\n")
cat("- Applied combined IQR and percentile-based approach\n")
cat("- Treated outliers in", nrow(unique(outlier_summary[outlier_summary$Method == "Combined Method", "Variable"])), "numeric variables\n")
cat("- Total outliers modified:", sum(outlier_summary[outlier_summary$Method == "Combined Method", "Outliers_Count"]), "\n")

cat("\n3. Dataset Dimensions\n")
cat("------------------\n")
cat("- Original:", nrow(data), "rows,", ncol(data), "columns\n")
cat("- Final:", nrow(data_clean), "rows,", ncol(data_clean), "columns\n")
cat("- Features removed:", ncol(data) - ncol(data_clean), "\n")

cat("\n4. Data Quality Improvements\n")
cat("-------------------------\n")
cat("- Missing values reduced from", sum(is.na(data)), "to", sum(is.na(data_clean)), "\n")
cat("- Standard deviation reduction (average across treated variables):", 
    round(mean(abs((before_after_stats$After_SD - before_after_stats$Before_SD) / before_after_stats$Before_SD * 100)), 1), "%\n")
sink()

# Create before-after boxplots for key variables to visualize impact of outlier treatment
# First save backup of original data for key variables
key_vars <- unique(before_after_stats$Variable)[1:min(5, nrow(before_after_stats))]
compare_data <- data.frame(
  Variable = character(),
  Value = numeric(),
  Stage = character(),
  stringsAsFactors = FALSE
)

# Prepare data for before boxplots (using original data)
for(var in key_vars) {
  if(var %in% names(data)) {
    before_data <- data.frame(
      Variable = rep(var, length(data[[var]])),
      Value = data[[var]],
      Stage = rep("Before Treatment", length(data[[var]]))
    )
    compare_data <- rbind(compare_data, before_data)
  }
}

# Prepare data for after boxplots
for(var in key_vars) {
  if(var %in% names(data_clean)) {
    after_data <- data.frame(
      Variable = rep(var, length(data_clean[[var]])),
      Value = data_clean[[var]],
      Stage = rep("After Treatment", length(data_clean[[var]]))
    )
    compare_data <- rbind(compare_data, after_data)
  }
}

# Create boxplot comparison
for(var in key_vars) {
  var_data <- compare_data[compare_data$Variable == var,]
  
  # Boxplot comparison
  plot_box <- ggplot(var_data, aes(x = Stage, y = Value, fill = Stage)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Distribution Comparison for", var),
         x = "",
         y = "Value") +
    theme(legend.position = "none")
  
  # Save plot
  ggsave(paste0("output/clean/boxplot_", var, ".png"), plot_box, width = 8, height = 6)
  
  # Density plot comparison
  plot_density <- ggplot(var_data, aes(x = Value, fill = Stage)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    labs(title = paste("Density Comparison for", var),
         x = "Value",
         y = "Density") +
    theme(legend.position = "bottom")
  
  # Save plot
  ggsave(paste0("output/clean/density_", var, ".png"), plot_density, width = 8, height = 6)
}

# Calculate overall statistics
total_removed_cols <- length(high_missing_cols)
numeric_imputed <- sum(sapply(numeric_cols, function(col) sum(is.na(data[[col]])) > 0))
categorical_imputed <- sum(sapply(categorical_cols, function(col) sum(is.na(data[[col]]) | data[[col]] == "") > 0))
variables_with_outliers <- nrow(unique(outlier_summary[outlier_summary$Method == "Combined Method", "Variable"]))
total_outliers_modified <- sum(outlier_summary[outlier_summary$Method == "Combined Method", "Outliers_Count"])
avg_sd_reduction <- round(mean(abs((before_after_stats$After_SD - before_after_stats$Before_SD) / before_after_stats$Before_SD * 100)), 1)

# Print final summary to console
cat("\n\n=====================================================\n")
cat("DATA CLEANING PROCESS COMPLETED SUCCESSFULLY\n")
cat("=====================================================\n\n")
cat(sprintf("Original dataset: %d rows, %d columns\n", nrow(data), ncol(data)))
cat(sprintf("Cleaned dataset: %d rows, %d columns\n\n", nrow(data_clean), ncol(data_clean)))

cat("MISSING VALUES TREATMENT:\n")
cat(sprintf("- %d columns removed (>50%% missing)\n", total_removed_cols))
cat(sprintf("- %d numeric variables imputed with median\n", numeric_imputed))
cat(sprintf("- %d categorical variables imputed with mode\n\n", categorical_imputed))

cat("OUTLIER TREATMENT:\n")
cat(sprintf("- %d variables processed for outliers\n", variables_with_outliers))
cat(sprintf("- %d outlier values modified (combined method)\n", total_outliers_modified))
cat(sprintf("- %.1f%% average reduction in standard deviation\n\n", avg_sd_reduction))

cat("VISUALIZATIONS AND REPORTS SAVED TO:\n")
cat("- Comparison plots: output/clean/*.png\n")
cat("- Detailed reports: output/clean/*.txt\n")
cat("- Cleaned dataset: output/clean/cleaned_data_combined_methods.csv\n\n")

cat("=====================================================\n")

# # Step 6: Feature Engineering ------------------------------------------------

# Create new folder for feature engineering output
if (!dir.exists("output/feature_engineering")) {
  dir.create("output/feature_engineering")
}

library(dplyr)
library(lubridate)
library(ggplot2)

# Create a copy of the data for feature engineering
data_fe <- data

# Feature 1: Time-based features
cat("Creating time-based features...\n")

# Convert TransactionDT to seconds (assuming it represents seconds since a reference time)
data_fe$Transaction_Hour <- (data_fe$TransactionDT %% 86400) %/% 3600
data_fe$Transaction_DayOfWeek <- (data_fe$TransactionDT %/% 86400) %% 7 + 1  # 1-7 (Monday-Sunday)
data_fe$Transaction_Weekend <- ifelse(data_fe$Transaction_DayOfWeek %in% c(6, 7), 1, 0)

# Create period of day (1: Morning 6-12, 2: Afternoon 12-18, 3: Evening 18-24, 4: Night 0-6)
data_fe$DayPeriod <- cut(data_fe$Transaction_Hour, 
                         breaks = c(-1, 5, 11, 17, 23),
                         labels = c("Night", "Morning", "Afternoon", "Evening"))

# Feature 2: High-risk hour flag (based on your analysis that morning hours 7-10am have high fraud rates)
data_fe$HighRiskHour <- ifelse(data_fe$Transaction_Hour >= 7 & data_fe$Transaction_Hour <= 10, 1, 0)

# Feature 3: Amount-based risk features
cat("Creating amount-based features...\n")

# Log transformation of amount (to handle skewness)
data_fe$LogAmount <- log1p(data_fe$TransactionAmt)

# Amount percentile rank (to identify unusually large transactions)
data_fe$AmountPercentile <- percent_rank(data_fe$TransactionAmt)

# Amount deviation from product category median
# First calculate median amount by ProductCD
product_medians <- data_fe %>%
  group_by(ProductCD) %>%
  summarize(MedianAmt = median(TransactionAmt, na.rm = TRUE))

# Merge back to get deviations
data_fe <- data_fe %>%
  left_join(product_medians, by = "ProductCD") %>%
  mutate(AmountDeviation = TransactionAmt - MedianAmt,
         AmountDeviationRatio = TransactionAmt / MedianAmt)

# Feature 4: Email domain risk features
cat("Creating email domain risk features...\n")

# Extract email domains
data_fe$P_emaildomain_type <- case_when(
  grepl("gmail", data_fe$P_emaildomain, ignore.case = TRUE) ~ "Google",
  grepl("yahoo", data_fe$P_emaildomain, ignore.case = TRUE) ~ "Yahoo",
  grepl("hotmail|outlook|live|msn", data_fe$P_emaildomain, ignore.case = TRUE) ~ "Microsoft",
  grepl("aol", data_fe$P_emaildomain, ignore.case = TRUE) ~ "AOL",
  grepl("comcast|att|centurylink", data_fe$P_emaildomain, ignore.case = TRUE) ~ "ISP",
  !is.na(data_fe$P_emaildomain) ~ "Other",
  is.na(data_fe$P_emaildomain) ~ "Missing"
)

# Calculate email domain fraud rates
email_fraud_rates <- data_fe %>%
  group_by(P_emaildomain_type) %>%
  summarize(DomainFraudRate = mean(isFraud, na.rm = TRUE))

# Merge back to get domain risk scores
data_fe <- data_fe %>%
  left_join(email_fraud_rates, by = "P_emaildomain_type")

# Feature 5: Device risk indicators
cat("Creating device-based features...\n")

# Create device type risk indicator (mobile has higher fraud rate)
data_fe$Mobile_Flag <- ifelse(data_fe$DeviceType == "mobile", 1, 0)

# Calculate device info fraud rates
device_fraud_rates <- data_fe %>%
  group_by(DeviceInfo) %>%
  summarize(
    DeviceFraudRate = mean(isFraud, na.rm = TRUE),
    DeviceCount = n()
  ) %>%
  filter(DeviceCount >= 100)  # Only consider common devices

# Merge back to get device risk scores
data_fe <- data_fe %>%
  left_join(device_fraud_rates, by = "DeviceInfo") %>%
  mutate(DeviceFraudRate = ifelse(is.na(DeviceFraudRate), mean(isFraud, na.rm = TRUE), DeviceFraudRate))

# Feature 6: Transaction behavior features
cat("Creating transaction behavior features...\n")

# High C2/C1 value flag (based on correlation analysis)
c2_threshold <- quantile(data_fe$C2, 0.95, na.rm = TRUE)
c1_threshold <- quantile(data_fe$C1, 0.95, na.rm = TRUE)
data_fe$HighC2Flag <- ifelse(data_fe$C2 > c2_threshold, 1, 0)
data_fe$HighC1Flag <- ifelse(data_fe$C1 > c1_threshold, 1, 0)

# Combined risk score based on C2, C1, C12 (top positive fraud correlations)
data_fe$CRiskScore <- scale(data_fe$C2) + scale(data_fe$C1) + scale(data_fe$C12)

# Feature 7: Verification mismatch indicators
cat("Creating verification mismatch features...\n")

# Email domain mismatch between purchaser and recipient (potential fraud indicator)
data_fe$EmailMismatch <- ifelse(
  !is.na(data_fe$P_emaildomain) & !is.na(data_fe$R_emaildomain) & 
    data_fe$P_emaildomain != data_fe$R_emaildomain, 
  1, 0)

# Card verification mismatches (combining card-related flags)
data_fe$CardMismatchFlag <- rowSums(
  cbind(
    ifelse(data_fe$C3 > 0, 1, 0),
    ifelse(data_fe$C4 > median(data_fe$C4, na.rm=TRUE), 1, 0),
    ifelse(data_fe$C5 > 0, 1, 0),
    ifelse(data_fe$C6 > median(data_fe$C6, na.rm=TRUE), 1, 0)
  ), 
  na.rm = TRUE
)

# Feature 8: Aggregated behavior-based features
cat("Creating aggregated behavior features...\n")

# Card issuer (card1) fraud rates
card1_fraud_rates <- data_fe %>%
  group_by(card1) %>%
  summarize(
    Card1FraudRate = mean(isFraud, na.rm = TRUE),
    Card1Count = n()
  ) %>%
  filter(Card1Count >= 50)  # Minimum observations threshold

# Merge back
data_fe <- data_fe %>%
  left_join(card1_fraud_rates, by = "card1") %>%
  mutate(Card1FraudRate = ifelse(is.na(Card1FraudRate), mean(isFraud, na.rm = TRUE), Card1FraudRate))

# Calculate product category fraud rates
product_fraud_rates <- data_fe %>%
  group_by(ProductCD) %>%
  summarize(ProductFraudRate = mean(isFraud, na.rm = TRUE))

# Merge back
data_fe <- data_fe %>%
  left_join(product_fraud_rates, by = "ProductCD")

# Feature 9: Create a combined risk score
cat("Creating combined risk score...\n")

# Standardize individual risk factors
risk_cols <- c("DomainFraudRate", "DeviceFraudRate", "Card1FraudRate", 
               "ProductFraudRate", "HighRiskHour", "CRiskScore")

# Remove NA values from risk columns
for(col in risk_cols) {
  data_fe[[col]][is.na(data_fe[[col]])] <- median(data_fe[[col]], na.rm = TRUE)
}

# Calculate composite risk score
data_fe$CompositeRiskScore <- rowMeans(scale(data_fe[,risk_cols]), na.rm = TRUE)

# Feature 10: Create interaction features between high-risk indicators
cat("Creating interaction features...\n")

data_fe$HighRiskHour_Mobile <- data_fe$HighRiskHour * data_fe$Mobile_Flag
data_fe$HighAmount_HighRiskHour <- ifelse(data_fe$AmountPercentile > 0.9 & data_fe$HighRiskHour == 1, 1, 0)
data_fe$HighRisk_EmailMismatch <- data_fe$EmailMismatch * ifelse(data_fe$CompositeRiskScore > 0, 1, 0)

# Save feature engineering summary
sink("output/feature_engineering/features_summary.txt")
cat("Feature Engineering Summary\n")
cat("==========================\n\n")
cat("1. Time-based Features:\n")
cat("   - Transaction_Hour: Hour of the day (0-23)\n")
cat("   - Transaction_DayOfWeek: Day of week (1-7, Monday-Sunday)\n")
cat("   - Transaction_Weekend: Binary weekend indicator\n")
cat("   - DayPeriod: Categorized period of day (Night, Morning, Afternoon, Evening)\n")
cat("   - HighRiskHour: Flag for high-risk morning hours (7-10am) with fraud rates 29-35%\n\n")

cat("2. Amount-based Features:\n")
cat("   - LogAmount: Log-transformed transaction amount to handle skewness\n")
cat("   - AmountPercentile: Percentile rank of transaction amount\n")
cat("   - AmountDeviation: Deviation from product category median\n")
cat("   - AmountDeviationRatio: Ratio to product category median\n\n")

cat("3. Email Domain Features:\n")
cat("   - P_emaildomain_type: Categorized email provider\n")
cat("   - DomainFraudRate: Fraud rate associated with email domain\n\n")

cat("4. Device Features:\n")
cat("   - Mobile_Flag: Flag for mobile transactions (higher fraud risk)\n")
cat("   - DeviceFraudRate: Fraud rate associated with specific device\n\n")

cat("5. Transaction Behavior Features:\n")
cat("   - HighC2Flag/HighC1Flag: Flags for high-risk C values (top 5%)\n")
cat("   - CRiskScore: Combined risk score based on C2, C1, C12 correlations\n\n")

cat("6. Verification Mismatch Features:\n")
cat("   - EmailMismatch: Flag for mismatch between purchaser and recipient email domains\n")
cat("   - CardMismatchFlag: Aggregate of card-related verification flags\n\n")

cat("7. Aggregated Behavior Features:\n")
cat("   - Card1FraudRate: Fraud rate associated with card issuer\n")
cat("   - ProductFraudRate: Fraud rate associated with product category\n\n")

cat("8. Combined Risk Score:\n")
cat("   - CompositeRiskScore: Standardized composite of multiple risk indicators\n\n")

cat("9. Interaction Features:\n")
cat("   - HighRiskHour_Mobile: High-risk hour on mobile device\n")
cat("   - HighAmount_HighRiskHour: Large transaction during high-risk hours\n")
cat("   - HighRisk_EmailMismatch: Email mismatch for transactions with high composite risk\n\n")

cat("Feature Justification:\n")
cat("======================\n")
cat("1. Temporal patterns are critical for fraud detection as fraudsters often target specific times\n")
cat("   The morning hours (7-10am) showed dramatically elevated fraud rates (29-35%)\n\n")

cat("2. Amount-based features help identify unusual transaction patterns\n")
cat("   Fraudulent transactions showed slightly higher variability with more outliers\n\n")

cat("3. Email domains showed significant variation in fraud rates with Google having the highest rate\n")
cat("   among major providers, making this a valuable risk indicator\n\n")

cat("4. Mobile devices had substantially higher fraud rates (14.5%) than desktop (9.46%)\n\n")

cat("5. Variables C2, C1, and C12 had the highest positive correlations with fraud\n")
cat("   (0.047, 0.040, and 0.035 respectively)\n\n")

cat("6. Mismatches between verification data points are strong fraud indicators\n\n")

cat("7. Aggregated behavior features leverage historical patterns at various levels\n")
cat("   Product category 'key_LY' had the highest fraud rate (17.3%)\n\n")

cat("8. Combined risk scores provide a holistic view of transaction risk\n\n")

cat("9. Interaction features capture complex fraud patterns that individual features cannot\n")
sink()

# Save engineered feature dataset
write.csv(data_fe, "output/feature_engineering/engineered_data.csv", row.names = FALSE)

# Generate visualizations for new features
pdf("output/feature_engineering/engineered_features_plots.pdf", width = 12, height = 10)

# Plot 1: Composite Risk Score by Fraud Status
ggplot(data_fe, aes(x = factor(isFraud), y = CompositeRiskScore, fill = factor(isFraud))) +
  geom_boxplot() +
  labs(title = "Composite Risk Score by Fraud Status",
       x = "Fraud Status", y = "Risk Score") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("0" = "Legitimate", "1" = "Fraudulent")) +
  theme_minimal()

# Plot 2: Hour of Day by Fraud Rate
hour_fraud_rate <- data_fe %>%
  group_by(Transaction_Hour) %>%
  summarize(FraudRate = mean(isFraud) * 100)

ggplot(hour_fraud_rate, aes(x = factor(Transaction_Hour), y = FraudRate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(FraudRate, 1)), vjust = -0.5, size = 3) +
  geom_hline(yintercept = mean(data_fe$isFraud) * 100, linetype = "dashed", color = "red") +
  labs(title = "Fraud Rate by Hour of Day",
       x = "Hour of Day", y = "Fraud Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Mobile vs Desktop Fraud Rates
device_fraud <- data_fe %>%
  group_by(Mobile_Flag) %>%
  summarize(
    Count = n(),
    FraudRate = mean(isFraud) * 100
  )

ggplot(device_fraud, aes(x = factor(Mobile_Flag), y = FraudRate, fill = factor(Mobile_Flag))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(FraudRate, 2)), vjust = -0.5) +
  labs(title = "Fraud Rate by Device Type",
       x = "Device Type", y = "Fraud Rate (%)") +
  scale_x_discrete(labels = c("0" = "Desktop", "1" = "Mobile")) +
  scale_fill_manual(values = c("0" = "darkgreen", "1" = "darkred"),
                    labels = c("0" = "Desktop", "1" = "Mobile")) +
  theme_minimal()

# Plot 4: Email Domain Fraud Rates
email_fraud <- data_fe %>%
  group_by(P_emaildomain_type) %>%
  summarize(
    Count = n(),
    FraudRate = mean(isFraud) * 100
  )

ggplot(email_fraud, aes(x = reorder(P_emaildomain_type, -FraudRate), y = FraudRate)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = round(FraudRate, 1)), vjust = -0.5) +
  labs(title = "Fraud Rate by Email Domain Type",
       x = "Email Domain Type", y = "Fraud Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 5: Combined Risk Features
risk_importance <- data.frame(
  Feature = c("HighRiskHour", "Mobile_Flag", "EmailMismatch", "HighC2Flag", "HighC1Flag"),
  FraudRate = c(
    mean(data_fe$isFraud[data_fe$HighRiskHour == 1]) * 100,
    mean(data_fe$isFraud[data_fe$Mobile_Flag == 1]) * 100,
    mean(data_fe$isFraud[data_fe$EmailMismatch == 1]) * 100,
    mean(data_fe$isFraud[data_fe$HighC2Flag == 1]) * 100,
    mean(data_fe$isFraud[data_fe$HighC1Flag == 1]) * 100
  )
)

ggplot(risk_importance, aes(x = reorder(Feature, -FraudRate), y = FraudRate)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = round(FraudRate, 1)), vjust = -0.5) +
  labs(title = "Fraud Rate by Risk Factor",
       x = "Risk Factor", y = "Fraud Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

cat("Feature engineering complete! Results saved to output/feature_engineering/\n")
cat("- Generated", ncol(data_fe) - ncol(data), "new features\n")
cat("- Feature summary saved in features_summary.txt\n")
cat("- Visualizations saved in engineered_features_plots.pdf\n")


