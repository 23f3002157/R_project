library(tidyverse)
library(ggplot2)
library(MASS)
library(corrplot)
library(rcompanion)
library(dplyr)

data <- read.csv("(K)cardio_data_processed.CSV")

##Basics

summary(data)
head(data)
tail(data)
str(data)
dim(data)
names(data)

#*******************************************************************************

## Data Preprocessing
# Step 1: Read the data
data <- read.csv("(K)cardio_data_processed.csv")

# Step 2: Basic preprocessing
# Remove rows with NA values
data <- data %>% filter(complete.cases(.))

# Step 3: Convert categorical variables to factors
data$gender <- as.factor(data$gender)
data$cholesterol <- as.factor(data$cholesterol)
data$gluc <- as.factor(data$gluc)
data$smoke <- as.factor(data$smoke)
data$alco <- as.factor(data$alco)
data$active <- as.factor(data$active)
data$cardio <- as.factor(data$cardio)
data$bp_category <- as.factor(data$bp_category)
data$bp_category_encoded <- as.factor(data$bp_category_encoded)

# Step 4: Remove unnecessary or redundant columns
# Remove 'id' as it's not needed for analysis
# Remove 'bp_category_encoded' if it's identical to 'bp_category'
if (all(data$bp_category == data$bp_category_encoded)) {
  data <- data %>% select(-id, -bp_category_encoded)
} else {
  data <- data %>% select(-id)
}

# Step 5: Remove extreme outliers for numeric columns
# Define function to remove outliers based on IQR
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  lower_bound <- qnt[1] - 1.5 * iqr
  upper_bound <- qnt[2] + 1.5 * iqr
  x >= lower_bound & x <= upper_bound
}

# Apply outlier removal to numeric columns
numeric_cols <- c("age_years", "height", "weight", "ap_hi", "ap_lo", "bmi")
for (col in numeric_cols) {
  data <- data %>% filter(remove_outliers(!!sym(col)))
}

# Step 6: Additional plausibility checks
# Ensure blood pressure values are within realistic ranges
# ap_hi (systolic): 50 to 250 mmHg
# ap_lo (diastolic): 30 to 150 mmHg
data <- data %>% filter(ap_hi >= 50 & ap_hi <= 250 & ap_lo >= 30 & ap_lo <= 150)

# Ensure age_years is within a reasonable range (e.g., 20 to 100)
data <- data %>% filter(age_years >= 20 & age_years <= 100)

# Ensure BMI is within a reasonable range (e.g., 10 to 60)
data <- data %>% filter(bmi >= 10 & bmi <= 60)

# Ensure height and weight are reasonable
data <- data %>% filter(height >= 100 & height <= 250 & weight >= 30 & weight <= 200)

# Step 7: Summary of preprocessed data
summary(data)

# Step 8: Normalizing data
# Step 1: Define the min-max normalization function
min_max_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Step 2: Identify numeric columns to normalize
numeric_cols <- c("age_years", "height", "weight", "ap_hi", "ap_lo", "bmi")

# Step 3: Create a copy of the dataset
modified_data_normalized <- data

# Step 4: Apply normalization to numeric columns
for (col in numeric_cols) {
  modified_data_normalized[[col]] <- min_max_normalize(modified_data_normalized[[col]])
}

# Step 5: Verify the normalization
# Check that all numeric columns are scaled to [0, 1]
summary(modified_data_normalized[numeric_cols])

# Step 6: Output the normalized dataset
# modified_data_normalized is now ready for use
head(modified_data_normalized)

#*******************************************************************************

## Scatter Plots

# Scatter Plot of Age vs Cholesterol grouped by cardio
ggplot(data, aes(x = age_years, y = factor(cholesterol), color = factor(cardio))) +
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) +
  scale_y_discrete(labels = c("1" = "Normal", "2" = "Above Normal", "3" = "Well Above Normal")) +
  scale_color_manual(values = c("red", "blue"), labels = c("No Disease", "Has Disease")) +
  labs(title = "Cholesterol Levels Across Ages by Cardiovascular Disease",
       x = "Age (years)",
       y = "Cholesterol Level",
       color = "Cardio") +
  theme_minimal()

# Correlation between age and Cholesterol
cor(data$age_years, data$cholesterol, use = "complete.obs")

# Chi Square test between Age and Cholesterol
chisq.test(table(data$age_years, data$cholesterol))

# Logistic regression to assess the impact of age and BP on cardio risk
glm(cardio ~ age_years + cholesterol, data = data, family = "binomial") %>% summary()

#*******************************************************************************

# Scatter Plot of Age vs Glucose grouped by cardio
ggplot(data, aes(x = age_years, y = gluc, color = factor(cardio))) +
  geom_jitter(height = 0.2, width = 0, alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Glucose vs Age", x = "Age (years)", y = "Blood Glucose", color = "Cardio") +
  theme_minimal()

# Correlation between Age and Glucose
cor(data$age_years, data$gluc)

# Logistic regression to assess the impact of age and BP on cardio risk
glm(cardio ~ age_years + gluc, data = data, family = "binomial") %>% summary()

#*******************************************************************************

# Scatter Plot of Age vs Systolic Blood pressure grouped by cardio
ggplot(data, aes(x = age_years, y = ap_hi, color = factor(cardio))) +
  geom_point(alpha = 0.5) +
  labs(title = "Systolic BP vs Age", x = "Age (years)", y = "Systolic Blood Pressure", color = "Cardio") +
  theme_minimal()

# Correlation between age and systolic BP
cor(data$age_years, data$ap_hi, use = "complete.obs")

# Chi Square test between Age and systolic BP
chisq.test(table(data$age_years, data$ap_hi))

# Logistic regression to assess the impact of age and systolic BP on cardio risk
glm(cardio ~ age_years + ap_hi, data = data, family = "binomial") %>% summary()

#*******************************************************************************

# Scatter Plot of Cholesterol vs Blood Glucose grouped by cardio
ggplot(data, aes(x = cholesterol, y = gluc, color = factor(cardio))) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +  # Jitter avoids overlapping points
  labs(title = "Cholesterol vs Glucose Levels", x = "Cholesterol", y = "Glucose", color = "Cardio") +
  theme_minimal()

# Correlation between Cholesterol vs Blood Glucose
cor(data$age_years, data$ap_hi, use = "complete.obs")

# Cramer's test between Cholesterol vs Blood Glucose
cramerV(table(data$cholesterol, data$gluc))

# Chi Square test between Cholesterol vs Blood Glucose
chisq.test(table(data$cholesterol, data$gluc))

# Logistic regression to assess the impact of Cholesterol vs Blood Glucose on cardio risk
glm(cardio ~ age_years + cholesterol, data = data, family = "binomial") %>% summary()

#*******************************************************************************

#Scatter Plot of Systolic Blood Pressure vs Cholesterol grouped by cardio
ggplot(data, aes(x = ap_hi, y = factor(cholesterol), color = factor(cardio))) +
  geom_jitter(alpha = 0.4, width = 2, height = 0.2) +
  labs(
    title = "Systolic Blood Pressure vs Cholesterol",
    x = "Systolic Blood Pressure (ap_hi)",
    y = "Cholesterol Level",
    color = "Cardio"
  ) +
  scale_y_discrete(labels = c("1" = "Normal", "2" = "Above Normal", "3" = "Well Above")) +
  theme_minimal()

# Correlation between Systolic Blood Pressure vs Cholesterol
cor(data$ap_hi, data$cholesterol)

# Chi Square test between Systolic Blood Pressure vs Cholesterol
chisq.test(table(data$cholesterol, data$gluc))

# Logistic regression to assess the impact of Systolic Blood Pressure vs Cholesterol on cardio risk
glm(cardio ~ age_years + cholesterol, data = data, family = "binomial") %>% summary()

#*******************************************************************************

#Scatter Plot of Systolic Blood Pressure vs Diastolic Blood Pressure grouped by cardio
ggplot(data, aes(x = ap_hi, y = ap_lo, color = factor(cardio))) +
  geom_point(alpha = 0.5) +
  labs(title = "Ap_hi Vs AP_lo", x = "Ap_hi", y = "Ap_lo", color = "Cardio") +
  theme_minimal()

# Correlation between Systolic Blood Pressure vs Diastolic Blood Pressure
cor(data$ap_hi, data$ap_lo, use = "complete.obs")

# Chi Square test between Systolic Blood Pressure vs Diastolic Blood Pressure
chisq.test(table(data$ap_hi, data$ap_lo))

# Logistic regression to assess the impact of Systolic Blood Pressure vs Diastolic Blood Pressure on cardio risk
glm(cardio ~ ap_hi + ap_lo, data = data, family = "binomial") %>% summary()

#*******************************************************************************

#Scatter Plot of Age vs Systolic Blood Pressure grouped by cardio
ggplot(data, aes(x = age_years, y = ap_hi, color = factor(cardio))) +
  geom_point(alpha = 0.5) +
  labs(title = "Age Vs Systolic Blood Pressure", x = "Age", y = "Ap_hi", color = "Cardio") +
  theme_minimal()

# Correlation between Age vs Systolic Blood Pressure
cor(data$age, data$ap_hi, use = "complete.obs")

# Chi Square test between Age vs Systolic Blood Pressure
chisq.test(table(data$age, data$ap_hi))

# Logistic regression to assess the impact of Age vs Systolic Blood Pressure on cardio risk
glm(cardio ~ age + ap_hi, data = data, family = "binomial") %>% summary()

#*******************************************************************************

## Box Plots

# Box Plot of Weight grouped by Cardio
ggplot(data, aes(x = factor(cardio), y = weight, fill = factor(cardio))) +
  geom_boxplot() +
  labs(x = "Cardiovascular Disease", y = "Weight (kg)", title = "Weight Distribution by Cardio Status") +
  theme_minimal()

# Summary of weight for people without cardiovascular disease
summary(data$weight[data$cardio == 0])

# Summary of weight for people with cardiovascular disease
summary(data$weight[data$cardio == 1])

# Since the columns have extreme outliers
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x >= (Q1 - 1.5 * IQR) & x <= (Q3 + 1.5 * IQR)]
}
# Subset data
cardio_0 <- data[data$cardio == 0, ]
cardio_1 <- data[data$cardio == 1, ]

# Remove outliers from each group
cardio_0_clean <- cardio_0[cardio_0$weight %in% remove_outliers(cardio_0$weight), ]
cardio_1_clean <- cardio_1[cardio_1$weight %in% remove_outliers(cardio_1$weight), ]

# Combine cleaned data
data_clean <- rbind(cardio_0_clean, cardio_1_clean)

ggplot(data_clean, aes(x = factor(cardio), y = weight, fill = factor(cardio))) +
  geom_boxplot() +
  scale_fill_manual(values = c("salmon", "darkcyan")) +
  labs(title = "Weight Distribution by Cardio Status (Outliers Removed)",
       x = "Cardiovascular Disease",
       y = "Weight (kg)",
       fill = "Cardio") +
  theme_minimal()

summary(cardio_0_clean$weight)
summary(cardio_1_clean$weight)

cor(data_clean$weight, data_clean$cardio)

#*******************************************************************************

# Box Plot of Age grouped by Cardio
ggplot(data, aes(x = factor(cardio), y = age_years, fill = factor(cardio))) +
  geom_boxplot() +
  labs(x = "Cardiovascular Disease", y = "Age (years)", title = "Age Distribution by Cardio Status") +
  theme_minimal()

summary(data$age_years[data$cardio == 0])
summary(data$age_years[data$cardio == 1])

cor(data$age, data$cardio)

#*******************************************************************************

# Box Plot of Systolic Blood Pressure grouped by Cholesterol

ggplot(data, aes(x = factor(cholesterol), y = ap_hi, fill = factor(cholesterol))) +
  geom_boxplot() +
  labs(x = "Cholesterol Level", y = "Systolic BP", title = "Systolic Blood Pressure by Cholesterol Level") +
  theme_minimal()

summary(data$ap_hi[data$cholesterol == 1])
summary(data$ap_hi[data$cholesterol == 2])
summary(data$ap_hi[data$cholesterol == 3])

cor(data$ap_hi, data$cholesterol)

#*******************************************************************************

# Box Plot of Systolic Blood Pressure grouped by Glucose
ggplot(data, aes(x = factor(gluc), y = ap_hi, fill = factor(gluc))) +
  geom_boxplot() +
  labs(x = "Glucose Level", y = "Diastolic BP", title = "Diastolic Blood Pressure by Glucose Level") +
  theme_minimal()

summary(data$ap_hi[data$gluc == 1])
summary(data$ap_hi[data$gluc == 2])
summary(data$ap_hi[data$gluc == 3])

cor(data$ap_hi, data$gluc)

#*******************************************************************************

# Box Plot of Height grouped by Gender

ggplot(data, aes(x = factor(gender), y = height, fill = factor(gender))) +
  geom_boxplot() +
  labs(x = "Gender", y = "Height (cm)", title = "Height Distribution by Gender") +
  theme_minimal()

cor(data$height, data$gender)

#*******************************************************************************

# Box Plot of BMI grouped by Cardio

ggplot(data, aes(x = factor(cardio), y = bmi, fill = factor(cardio))) +
  geom_boxplot() +
  labs(x = "Cardiovascular Disease", y = "BMI", title = "BMI Distribution by Cardio Status") +
  theme_minimal()

summary(data$bmi[data$cardio == 0])
summary(data$bmi[data$cardio == 1])


# Filter the data to remove extreme BMI outliers
filtered_data <- subset(data, bmi >= 10 & bmi <= 60)

ggplot(filtered_data, aes(x = factor(cardio), y = bmi, fill = factor(cardio))) +
  geom_boxplot(outlier.shape = 1, outlier.size = 1.5) +
  labs(
    title = "BMI Distribution by Cardio Status",
    x = "Cardiovascular Disease",
    y = "BMI"
  ) +
  theme_minimal()

summary(filtered_data$bmi[filtered_data$cardio == 0])
summary(filtered_data$bmi[filtered_data$cardio == 1])

cor(filtered_data$bmi, filtered_data$cardio)

#*******************************************************************************

# Box Plot of BMI grouped by gender

ggplot(data, aes(x = factor(gender), y = bmi)) +
  geom_boxplot() +
  labs(x = "Cardiovascular Disease", y = "BMI", title = "BMI Distribution by Cardio Status") +
  theme_minimal()

summary(data$bmi[data$gender == 1])
summary(data$bmi[data$gender == 2])

cor(data$bmi, data$gender)

# Filter the data to remove extreme BMI outliers
filtered_data <- subset(data, bmi >= 10 & bmi <= 60)

ggplot(filtered_data, aes(x = factor(gender), y = bmi, fill = factor(gender))) +
  geom_boxplot(outlier.shape = 1, outlier.size = 1.5) +
  labs(
    title = "BMI Distribution by  Gender",
    x = "Gender",
    y = "BMI"
  ) +
  theme_minimal()

summary(filtered_data$bmi[filtered_data$gender == 1])
summary(filtered_data$bmi[filtered_data$gender == 2])

cor(filtered_data$bmi, filtered_data$gender)

#*******************************************************************************

# Box Plot of Height grouped by Cardio

ggplot(data, aes(x = factor(cardio), y = height, fill = factor(cardio))) +
  geom_boxplot() +
  labs(x = "Cardio", y = "Height (cm)", title = "Height Distribution by Gender") +
  theme_minimal()

summary(data$height[data$cardio == 0])
summary(data$height[data$cardio == 1])

cor(data$height, data$cardio)

