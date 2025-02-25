# This is a sample R script for assignment 1.

# Author: Jiang Feiyu
# UID: 3035770800
# Date: 24/02/2025

# Install packages/load libraries
#install.packages(c("tidyverse", "naniar", "mice", "outliers", "car"))
library(tidyverse)
library(naniar)     # For missing value analysis
library(mice)       # For missing value imputation
library(outliers)   # For outlier detection
library(car)        # For data visualization


# Load the data
setwd("C:/Users/Bob/Desktop/FITE7410/Assign/assign1/materials")
print(getwd())
dataset = read.csv("A1_data.csv")

# Exploratory Data Analysis

# 1. Distinguish Attributes

#str(dataset)
#summary(dataset)

# 设置随机种子确保结果可重复
set.seed(123)

# 创建抽样数据集（比如抽取10%的数据）
sample_size <- 10000  # 抽取10000条记录
sample_data <- dataset %>% 
  slice_sample(n = sample_size)

# Save the summary to local
structure_info <- capture.output(str(dataset))
summary_info <- capture.output(summary(dataset))
writeLines(structure_info, "structure_info.txt")
writeLines(summary_info, "summary_info.txt")

#print(summary(dataset))
#...


# 2. Univariate Analysis
#ggplot(...)
#hist(...)
#...

fraud_dist <- table(dataset$isFraud)
fraud_prop <- prop.table(fraud_dist)

ggplot(dataset, aes(x = TransactionAmt)) + geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7)

ggplot(dataset, aes(x = ProductCD)) + geom_bar(fill = "steelblue")

sample_data <- dataset %>% slice_sample(n = 10000)

# 3. Bi-/Multi-variate Analysis
#ggplot(dataset, aes(...)) + geom_bar()
#corrplot(...)
#......


