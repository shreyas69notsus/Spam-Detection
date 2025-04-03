# Project Title: Social Media Sentiment Analysis
# Author: Shreyas
# Date: 2024-06-14
# Description: This script analyzes social media sentiment data, calculates descriptive statistics,visualizes data, and performs regression and hypothesis testing.

              
# --- Setup ---
# Install and load required libraries
if (!require(moments)) install.packages("moments")
if (!require(ggplot2)) install.packages("ggplot2")
library(moments)
library(ggplot2)

# --- Data Loading ---
# Load the dataset (use relative path for GitHub compatibility)
dataset <- read.csv("data/social_media_sentiment_dataset.csv")
head(dataset)

# Check if dataset loaded correctly
if (is.null(dataset)) stop("Dataset failed to load. Please check file path.")

# --- Data Preprocessing ---
# Convert Month column to a factor
dataset$Month <- as.factor(dataset$Month)

# --- Descriptive Statistics ---
# Calculate mean, median, and mode of retweet counts
mean_value <- mean(dataset$Retweets, na.rm = TRUE)
median_value <- median(dataset$Retweets, na.rm = TRUE)

# Define a custom function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_value <- get_mode(dataset$Retweets)

# Calculate variance and standard deviation of retweet counts
variance_value <- var(dataset$Retweets, na.rm = TRUE)
sd_value <- sd(dataset$Retweets, na.rm = TRUE)

# Calculate skewness and kurtosis of retweet counts
skewness_value <- skewness(dataset$Retweets, na.rm = TRUE)
kurtosis_value <- kurtosis(dataset$Retweets, na.rm = TRUE)
skewness_value
kurtosis_value

# --- Data Visualization ---
# Plot histogram of retweets
ggplot(dataset, aes(x = Retweets)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  theme_minimal() + 
  labs(title = "Histogram of Retweets", x = "Retweets", y = "Frequency")

# Plot histogram of likes
ggplot(dataset, aes(x = Likes)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  theme_minimal() + 
  labs(title = "Histogram of Likes", x = "Likes", y = "Frequency")

# Plot boxplot of retweets
ggplot(dataset, aes(y = Retweets)) + 
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Retweets", y = "Retweets")

# Plot scatter plot of retweets vs likes
ggplot(dataset, aes(x = Retweets, y = Likes)) + 
  geom_point() + 
  labs(title = "Scatter Plot of Likes vs Retweets", x = "Retweets", y = "Likes")

# --- Correlation ---
# Calculate correlation between retweets and likes
correlation <- cor(dataset$Retweets, dataset$Likes, use = "complete.obs")

# --- Regression Analysis ---
# Simple linear regression: retweets vs month
simple_model <- lm(Retweets ~ Month, data = dataset)
summary(simple_model)

# Plot simple regression of retweets vs month
ggplot(dataset, aes(x = Month, y = Retweets)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Simple Regression: Retweets vs Month", x = "Month", y = "Number of Retweets")

# Multiple linear regression: retweets vs likes and month
multiple_model <- lm(Retweets ~ Month + Likes, data = dataset)
summary(multiple_model)

# Plot multiple regression of retweets vs likes
ggplot(dataset, aes(x = Likes, y = Retweets)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Multiple Regression: Retweets vs Likes", x = "Likes", y = "Number of Retweets")

# --- Model Diagnostics ---
# Model diagnostics plots
plot(multiple_model, which = 1:4)

# --- Predictions ---
# Prediction based on new data
new_data <- data.frame(Month = factor(c(2, 12), levels = levels(dataset$Month)), Likes = c(20, 30))
predictions <- predict(multiple_model, newdata = new_data)

# --- Hypothesis Testing ---
# One-sample t-test for average retweet count
t_test_result <- t.test(dataset$Retweets, mu = 100)
print(t_test_result)

# Two-sample t-test comparing retweets in January and February
t_test_result_2 <- t.test(Retweets ~ Month, data = subset(dataset, Month %in% c("January", "February")))

# ANOVA test for differences in retweets across months
aov_model <- aov(Retweets ~ Month, data = dataset)
summary(aov_model)
