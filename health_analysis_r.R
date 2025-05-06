# Install required packages (run once)
install.packages("tidyverse")
install.packages("psych")
install.packages("moments")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("skimr")

# Load libraries
library(tidyverse)
library(psych)
library(moments)
library(gridExtra)
library(corrplot)
library(ggpubr)
library(skimr)

# Load the dataset
data <- read.csv("C:/Users/Kushi KKS/Downloads/archive (1)/cardio_data_processed.csv")

# Basic summary
cat("Structure of the dataset:\n")
str(data)

cat("\nSummary of the dataset:\n")
summary(data)

cat("\nDetailed skim summary:\n")
skim(data)

# Convert categorical variables to factors
data <- data %>%
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Female", "Male")),
    cholesterol = factor(cholesterol, levels = 1:3, labels = c("Normal", "Above Normal", "Well Above Normal")),
    gluc = factor(gluc, levels = 1:3, labels = c("Normal", "Above Normal", "Well Above Normal")),
    smoke = factor(smoke, levels = 0:1, labels = c("Non-smoker", "Smoker")),
    alco = factor(alco, levels = 0:1, labels = c("Non-drinker", "Drinker")),
    active = factor(active, levels = 0:1, labels = c("Not Active", "Active")),
    cardio = factor(cardio, levels = 0:1, labels = c("Absence", "Presence")),
    bp_category = factor(bp_category)
  )

# Select columns
numeric_cols <- c("age", "age_years", "height", "weight", "ap_hi", "ap_lo", "bmi")
categorical_cols <- c("gender", "cholesterol", "gluc", "smoke", "alco", "active", "cardio", "bp_category")

# --- UNIVARIATE ANALYSIS ---

# Function: Numeric stats
describe_numeric <- function(x) {
  stats <- data.frame(
    N = length(na.omit(x)),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE)
  )
  return(stats)
}

# Function: Categorical stats
describe_categorical <- function(x) {
  freq <- table(x)
  prop <- prop.table(freq)
  stats <- data.frame(
    Category = names(freq),
    Frequency = as.numeric(freq),
    Percentage = round(as.numeric(prop) * 100, 2)
  )
  return(stats)
}

# Descriptive stats for numeric variables
cat("\nDescriptive Statistics for Numeric Variables:\n")
numeric_stats <- lapply(data[numeric_cols], describe_numeric)
numeric_stats_df <- do.call(rbind, numeric_stats)
rownames(numeric_stats_df) <- numeric_cols
print(numeric_stats_df)

# Descriptive stats for categorical variables
cat("\nDescriptive Statistics for Categorical Variables:\n")
cat_stats <- lapply(data[categorical_cols], describe_categorical)
names(cat_stats) <- categorical_cols
print(cat_stats)

# Visualizations: numeric variables
for (col in numeric_cols) {
  p1 <- ggplot(data, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
    geom_density(alpha = 0.5, fill = "orange") +
    ggtitle(paste("Histogram of", col)) +
    theme_minimal()
  
  p2 <- ggplot(data, aes_string(y = col)) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    ggtitle(paste("Boxplot of", col)) +
    theme_minimal()
  
  p3 <- ggplot(data, aes_string(sample = col)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Q-Q Plot of", col)) +
    theme_minimal()
  
  grid.arrange(p1, p2, p3, ncol = 3)
}

# --- MULTIVARIATE ANALYSIS ---

# Correlation matrix
cor_data <- data %>% select(age, height, weight, ap_hi, ap_lo, bmi)
cor_matrix <- cor(cor_data, use = "complete.obs")

cat("\nCorrelation Matrix:\n")
print(cor_matrix)

# Correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")

# Scatterplot matrix
pairs(cor_data, main = "Scatterplot Matrix of Numeric Variables")

# --- DESCRIPTIVE STATS BY GROUP ---

cat("\nDescriptive Stats by Gender:\n")
gender_desc <- describeBy(data[numeric_cols], group = data$gender)
print(gender_desc)

cat("\nDescriptive Stats by Cardiovascular Disease (cardio):\n")
cardio_desc <- describeBy(data[numeric_cols], group = data$cardio)
print(cardio_desc)