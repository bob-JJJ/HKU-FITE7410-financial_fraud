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

## # visualization -------------------------------------------------------------

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

## # Bivariate analysis---------------------------------------------------------

## # Multivariate analysis------------------------------------------------------

# Correlation Analysis

# Step 4: Detect erroneous & missing values ------------------------------------

# ## Missing Values Treatment ----------------------------------------------------
# 
# # Function to calculate mode
# get_mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# # Function to print missing value summary
# print_missing_summary <- function(data, title = "Missing Values Summary") {
#   cat("\n", title, "\n", paste(rep("-", nchar(title)), collapse = ""), "\n")
#   
#   # Calculate missing values for each column
#   missing_summary <- data.frame(
#     Column = names(data),
#     Missing_Count = colSums(is.na(data)),
#     Missing_Percentage = round(colMeans(is.na(data)) * 100, 2)
#   )
#   
#   # Sort by missing percentage
#   missing_summary <- missing_summary[order(-missing_summary$Missing_Percentage), ]
#   
#   # Print summary
#   print(missing_summary)
#   
#   return(missing_summary)
# }
# 
# # Initial missing values analysis
# initial_missing <- print_missing_summary(data, "Initial Missing Values Analysis")
# 
# # Save initial missing values analysis
# write.csv(initial_missing, "output/initial_missing_analysis.csv", row.names = FALSE)
# 
# # Calculate missing value percentages for each column
# missing_percentages <- colMeans(is.na(data)) * 100
# high_missing_cols <- names(missing_percentages[missing_percentages > 50])
# 
# # Remove columns with more than 50% missing values
# data_clean <- data[, !names(data) %in% high_missing_cols]
# 
# # Identify column types
# numeric_cols <- names(data_clean)[sapply(data_clean, is.numeric)]
# categorical_cols <- names(data_clean)[sapply(data_clean, is.character)]
# 
# # Create missing value treatment report
# sink("output/missing_values_treatment.txt")
# cat("Missing Values Treatment Report\n")
# cat("============================\n\n")
# 
# cat("1. Columns removed (>50% missing):\n")
# if(length(high_missing_cols) > 0) {
#   for(col in high_missing_cols) {
#     cat(sprintf("- %s (%.2f%% missing)\n", col, missing_percentages[col]))
#   }
# } else {
#   cat("None\n")
# }
# 
# cat("\n2. Numeric Columns Treatment:\n")
# for(col in numeric_cols) {
#   missing_count <- sum(is.na(data_clean[[col]]))
#   if(missing_count > 0) {
#     median_val <- median(data_clean[[col]], na.rm = TRUE)
#     data_clean[[col]][is.na(data_clean[[col]])] <- median_val
#     cat(sprintf("- %s: %d missing values imputed with median (%.2f)\n", 
#                 col, missing_count, median_val))
#   }
# }
# 
# cat("\n3. Categorical Columns Treatment:\n")
# for(col in categorical_cols) {
#   missing_count <- sum(is.na(data_clean[[col]]) | data_clean[[col]] == "")
#   if(missing_count > 0) {
#     mode_val <- get_mode(data_clean[[col]][!is.na(data_clean[[col]]) & data_clean[[col]] != ""])
#     data_clean[[col]][is.na(data_clean[[col]]) | data_clean[[col]] == ""] <- mode_val
#     cat(sprintf("- %s: %d missing values imputed with mode (%s)\n", 
#                 col, missing_count, mode_val))
#   }
# }
# 
# # Final missing values check
# final_missing <- print_missing_summary(data_clean, "\n4. Final Missing Values Check")
# sink()
# 
# # Additional data quality checks
# # Check for infinite values
# inf_check <- sapply(data_clean[numeric_cols], function(x) sum(is.infinite(x)))
# if(any(inf_check > 0)) {
#   cat("\nWarning: Infinite values found in the following columns:\n")
#   print(inf_check[inf_check > 0])
#   
#   # Replace infinite values with NA and then impute
#   for(col in names(inf_check[inf_check > 0])) {
#     data_clean[[col]][is.infinite(data_clean[[col]])] <- NA
#     median_val <- median(data_clean[[col]], na.rm = TRUE)
#     data_clean[[col]][is.na(data_clean[[col]])] <- median_val
#   }
# }
# 
# # Check for constant columns (zero variance)
# constant_cols <- sapply(data_clean, function(x) length(unique(x)) == 1)
# if(any(constant_cols)) {
#   cat("\nWarning: The following columns have constant values:\n")
#   print(names(data_clean)[constant_cols])
# }
# 
# # Save cleaned data
# write.csv(data_clean, "output/cleaned_data_missing_treated.csv", row.names = FALSE)
# 
# # Print summary of changes
# cat("\nData Cleaning Summary:\n")
# cat("Original dimensions:", dim(data)[1], "rows,", dim(data)[2], "columns\n")
# cat("Cleaned dimensions:", dim(data_clean)[1], "rows,", dim(data_clean)[2], "columns\n")
# 
# 
# 
# # Step 5: Detect erroneous & missing values------------------------------------
# 
# # truncation function
# truncate_values <- function(x, lower_percentile = 0.01, upper_percentile = 0.99) {
#   lower <- quantile(x, lower_percentile)
#   upper <- quantile(x, upper_percentile)
#   return(list(
#     lower = lower,
#     upper = upper,
#     values = pmin(pmax(x, lower), upper)
#   ))
# }
# 
# for(col in numeric_cols) {
#   # Skip ID and binary columns
#   if(!grepl("ID$|^is", col, ignore.case = TRUE) && length(unique(data_clean[[col]])) > 2) {
#     cat("\n=== Processing Variable:", col, "===\n")
#     
#     # 1. IQR Method
#     cat("\n1. IQR Method Results:\n")
#     # Calculate median and IQR
#     M <- median(data_clean[[col]])
#     Q1 <- quantile(data_clean[[col]], 0.25)
#     Q3 <- quantile(data_clean[[col]], 0.75)
#     IQR <- Q3 - Q1
#     
#     # Calculate bounds using the formula: M ± 3 x IQR/(2 x 0.6745)
#     constant <- 3 * IQR/(2 * 0.6745)
#     iqr_lower <- M - constant
#     iqr_upper <- M + constant
#     
#     # 2. Truncation Method
#     cat("\n2. Truncation Method Results:\n")
#     trunc_result <- truncate_values(data_clean[[col]])
#     
#     # Print comparative summary
#     cat("\nComparative Summary:")
#     cat("\n------------------")
#     cat("\nMedian:", M)
#     cat("\nIQR:", IQR)
#     cat("\n\nIQR Method Bounds:")
#     cat("\n- Lower:", iqr_lower)
#     cat("\n- Upper:", iqr_upper)
#     cat("\n- Outliers:", sum(data_clean[[col]] < iqr_lower | data_clean[[col]] > iqr_upper))
#     
#     cat("\n\nTruncation Method Bounds (1% and 99%):")
#     cat("\n- Lower:", trunc_result$lower)
#     cat("\n- Upper:", trunc_result$upper)
#     cat("\n- Outliers:", sum(data_clean[[col]] < trunc_result$lower | data_clean[[col]] > trunc_result$upper))
#     
#     # Combine both methods (using the more conservative bounds)
#     final_lower <- max(iqr_lower, trunc_result$lower)
#     final_upper <- min(iqr_upper, trunc_result$upper)
#     
#     # Apply final bounds
#     data_clean[[col]] <- pmin(pmax(data_clean[[col]], final_lower), final_upper)
#     
#     cat("\n\nFinal Combined Bounds:")
#     cat("\n- Lower:", final_lower)
#     cat("\n- Upper:", final_upper)
#     cat("\n- Total modified values:", sum(data_clean[[col]] == final_lower | data_clean[[col]] == final_upper))
#     cat("\n==========================================\n")
#     
#   }
# }
# 
# # Save the cleaned data
# write.csv(data_clean, "output/cleaned_data_combined_methods.csv", row.names = FALSE)
# 
# # Create summary report
# sink("output/outlier_treatment_report.txt")
# cat("Outlier Treatment Summary Report\n")
# cat("Date:", Sys.time(), "\n\n")
# 
# for(col in numeric_cols) {
#   if(!grepl("ID$|^is", col, ignore.case = TRUE) && length(unique(data_clean[[col]])) > 2) {
#     cat("\nVariable:", col)
#     cat("\nBefore cleaning:\n")
#     print(summary(data_clean[[col]]))
#     cat("\nAfter cleaning:\n")
#     print(summary(data_clean[[col]]))
#     cat("\n-------------------\n")
#   }
# }
# sink()
# 
# 
# ## Save cleaning summary -------------------------------------------------------
# sink("output/cleaning_summary.txt")
# cat("Data Cleaning Summary\n\n")
# cat("1. Removed columns with >50% missing values:\n")
# cat(paste(high_missing_cols, collapse = "\n"), "\n\n")
# cat("2. Original dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
# cat("3. Cleaned dimensions:", nrow(data_clean), "rows,", ncol(data_clean), "columns\n")
# sink()


# # Step 6: Feature Engineering ------------------------------------------------

## Write your code here

#ggplot(...)
#hist(...)
#...

# 3. Bi-/Multi-variate Analysis
#ggplot(dataset, aes(...)) + geom_bar()
#corrplot(...)
#......


