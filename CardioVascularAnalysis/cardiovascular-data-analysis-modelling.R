#Install and Load the packages
pacman::p_load(
  tidyverse, psych, moments, gridExtra, corrplot, ggpubr, skimr,
  ggplot2, dplyr, caret, randomForest, class, ROSE, glmnet, pROC,
  rcompanion, httr, DBI, lubridate, plotly, rio,MASS,corrplot,
  shiny, stringr, tidyr, ggcorrplot
)

#Load the dataset
data <- read.csv("cardio_data_processed.csv")
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
data <- read.csv("cardio_data_processed.csv")

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
  data <- data %>% dplyr::select(-c(id, bp_category_encoded))
} else {
  data <- data %>% dplyr::select(-id)
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
# Data visualisation using R
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
data$age_years <- as.numeric(as.character(data$age_years)) 
data$cholesterol <- as.numeric(as.character(data$cholesterol))
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
data$age_years <- as.numeric(as.character(data$age_years))  
data$ap_hi <- as.numeric(as.character(data$ap_hi))
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
 
data_clean$cardio <- as.numeric(as.character(data_clean$cardio))
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

data$gluc <- as.numeric(as.character(data$gluc))
cor(data$ap_hi, data$gluc)

#*******************************************************************************

# Box Plot of Height grouped by Gender

ggplot(data, aes(x = factor(gender), y = height, fill = factor(gender))) +
  geom_boxplot() +
  labs(x = "Gender", y = "Height (cm)", title = "Height Distribution by Gender") +
  theme_minimal()

data$gender <- as.numeric(as.character(data$gender))
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

#*******************************************************************************

#Descriptive statistics in R
data <- read.csv("cardio_data_processed.csv")

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


#*******************************************************************************
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

#calculate the mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

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


#*******************************************************************************
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

#*******************************************************************************

# Hypotesting

# Importing the dataset
data <- read.csv("cardio_data_processed.csv")

# 1. T-test: Compare BMI between patients with and without cardiovascular disease
# Subsetting data for cardio = 0 and cardio = 1
bmi_cardio_0 <- data$bmi[data$cardio == 0]
bmi_cardio_1 <- data$bmi[data$cardio == 1]

# Performing the T-test
t_test_result <- t.test(bmi_cardio_0, bmi_cardio_1, var.equal = FALSE)
print("T-test Result (BMI vs Cardio):")
print(t_test_result)

# Plotting boxplot for T-test
data$cardio_factor <- factor(data$cardio, labels = c("No Cardio", "Cardio"))
ggplot(data, aes(x = cardio_factor, y = bmi, fill = cardio_factor)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Cardiovascular Disease Status",
       x = "Cardiovascular Disease",
       y = "BMI") +
  theme_minimal()
ggsave("bmi_vs_cardio_boxplot.png")

#*******************************************************************************

# 2. Paired T-test: Comparing ap_hi and ap_lo (demonstration purposes)
# Note: This isn't a true paired test scenario since ap_hi and ap_lo aren't before/after measures
paired_t_test_result <- t.test(data$ap_hi, data$ap_lo, paired = TRUE)
print("Paired T-test Result (ap_hi vs ap_lo):")
print(paired_t_test_result)


#*******************************************************************************

# 3. Correlation: Between BMI and ap_hi (systolic blood pressure)
cor_test_result <- cor.test(data$bmi, data$ap_hi, method = "pearson")
print("Correlation Test Result (BMI vs ap_hi):")
print(cor_test_result)

# Plotting scatterplot for correlation
ggplot(data, aes(x = bmi, y = ap_hi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatterplot of BMI vs Systolic Blood Pressure (ap_hi)",
       x = "BMI",
       y = "Systolic BP (ap_hi)") +
  theme_minimal()
ggsave("bmi_vs_aphi_scatterplot.png")

#*******************************************************************************

# 4. Chi-Square Test: Association between smoking and cardiovascular disease
# Creating a contingency table
contingency_table <- table(data$smoke, data$cardio)
rownames(contingency_table) <- c("Non-Smoker", "Smoker")
colnames(contingency_table) <- c("No Cardio", "Cardio")

# Performing Chi-Square test
chi_square_result <- chisq.test(contingency_table)
print("Chi-Square Test Result (Smoke vs Cardio):")
print(chi_square_result)

# Plotting bar plot for Chi-Square test
smoke_cardio_df <- as.data.frame(contingency_table)
colnames(smoke_cardio_df) <- c("Smoke", "Cardio", "Count")
smoke_cardio_df$Smoke <- factor(smoke_cardio_df$Smoke, labels = c("Non-Smoker", "Smoker"))
smoke_cardio_df$Cardio <- factor(smoke_cardio_df$Cardio, labels = c("No Cardio", "Cardio"))

ggplot(smoke_cardio_df, aes(x = Smoke, y = Count, fill = Cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Smoking Status vs Cardiovascular Disease",
       x = "Smoking Status",
       y = "Count") +
  theme_minimal()
ggsave("smoke_vs_cardio_barplot.png")

#*******************************************************************************

# 5. ANOVA: Compare BMI across cholesterol levels
# Converting cholesterol to factor
data$cholesterol_factor <- factor(data$cholesterol, labels = c("Normal", "Above Normal", "High"))

# Performing ANOVA
anova_result <- aov(bmi ~ cholesterol_factor, data = data)
print("ANOVA Result (BMI vs Cholesterol):")
print(summary(anova_result))

# Plotting boxplot for ANOVA
ggplot(data, aes(x = cholesterol_factor, y = bmi, fill = cholesterol_factor)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Cholesterol Level",
       x = "Cholesterol Level",
       y = "BMI") +
  theme_minimal()
#ggsave("bmi_vs_cholesterol_boxplot.png")

#*******************************************************************************

#Machine Learning
# Set seed for reproducibility
set.seed(123)

# Importing the dataset
data <- read.csv("cardio_data_processed.csv")

# Selecting features and target
features <- c("age_years", "bmi", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active", "gender")
data_model <- data[, c(features, "cardio")]

# Data Preprocessing: Handle outliers using IQR
outlier_filter <- function(x) {
  iqr <- IQR(x)
  q <- quantile(x, probs = c(0.25, 0.75))
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  x[x < lower | x > upper] <- NA
  return(x)
}
data_model$ap_hi <- outlier_filter(data_model$ap_hi)
data_model$ap_lo <- outlier_filter(data_model$ap_lo)
data_model$bmi <- outlier_filter(data_model$bmi)
data_model <- na.omit(data_model)

# Convert categorical variables to factors
data_model$cholesterol <- as.factor(data_model$cholesterol)
data_model$gluc <- as.factor(data_model$gluc)
data_model$smoke <- as.factor(data_model$smoke)
data_model$alco <- as.factor(data_model$alco)
data_model$active <- as.factor(data_model$active)
data_model$gender <- as.factor(data_model$gender)
data_model$cardio <- as.factor(data_model$cardio)

# Rename factor levels of cardio to valid R variable names
levels(data_model$cardio) <- c("NoCardio", "Cardio")

# Train-test split (70% train, 30% test)
trainIndex <- createDataPartition(data_model$cardio, p = 0.7, list = FALSE)
train_data <- data_model[trainIndex, ]
test_data <- data_model[-trainIndex, ]

# Address class imbalance using ROSE (SMOTE-like oversampling/undersampling)
train_data <- ovun.sample(cardio ~ ., data = train_data, method = "both", 
                          p = 0.5, seed = 123)$data

#*******************************************************************************

# Feature Selection using Random Forest (initial model to get importance)
initial_rf <- randomForest(cardio ~ ., data = train_data, ntree = 50)
importance_scores <- importance(initial_rf)
important_features <- names(importance_scores[order(importance_scores, decreasing = TRUE), ][1:8])
print("Top 8 Important Features:")
print(important_features)

# Prepare data with selected features and interaction
# Rename the interaction term to a valid R variable name
train_data_subset <- train_data[, c(important_features, "cardio")]
train_data_subset$bmi_cholesterol <- train_data$bmi * as.numeric(train_data$cholesterol)
test_data_subset <- test_data[, c(important_features, "cardio")]
test_data_subset$bmi_cholesterol <- test_data$bmi * as.numeric(test_data$cholesterol)

# Update features list to include the renamed interaction term
features <- c(important_features, "bmi_cholesterol")

# Total number of test predictions (for percentage calculation)
total_test <- nrow(test_data)

# Define cross-validation control
train_control <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)


#*******************************************************************************
# 1. Logistic Regression with glmnet (regularization) and threshold optimization
x_train <- model.matrix(cardio ~ ., data = train_data_subset)[, -1]
x_test <- model.matrix(cardio ~ ., data = test_data_subset)[, -1]
y_train <- train_data_subset$cardio
log_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, nfolds = 5)
log_pred_prob <- predict(log_model, newx = x_test, type = "response", s = "lambda.min")
# Fix for roc(): Convert matrix to numeric vector
log_pred_prob_vec <- as.numeric(log_pred_prob)
roc_obj <- roc(test_data_subset$cardio, log_pred_prob_vec)
best_threshold <- coords(roc_obj, "best", ret = "threshold")$threshold
log_pred <- ifelse(log_pred_prob_vec > best_threshold, "Cardio", "NoCardio")
log_pred <- factor(log_pred, levels = c("NoCardio", "Cardio"))
log_accuracy <- mean(log_pred == test_data_subset$cardio)
log_f1 <- confusionMatrix(log_pred, test_data_subset$cardio)$byClass["F1"]
log_auc <- auc(roc_obj)
print("Logistic Regression Metrics:")
print(paste("Accuracy:", round(log_accuracy, 3)))
print(paste("F1-Score:", round(log_f1, 3)))
print(paste("AUC-ROC:", round(log_auc, 3)))

# Confusion Matrix for Logistic Regression with percentages
log_cm <- confusionMatrix(log_pred, test_data_subset$cardio)
print("Logistic Regression Confusion Matrix (Counts and Percentages):")
log_cm_table <- log_cm$table
log_cm_percent <- round((log_cm_table / total_test) * 100, 2)
log_cm_combined <- matrix(paste0(log_cm_table, " (", log_cm_percent, "%)"), nrow = 2)
dimnames(log_cm_combined) <- dimnames(log_cm_table)
print(log_cm_combined)

# Plotting Confusion Matrix for Logistic Regression
log_cm_df <- as.data.frame(as.table(log_cm$table))
colnames(log_cm_df) <- c("Predicted", "Actual", "Freq")
p1 <- ggplot(log_cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix: Logistic Regression",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()
print(p1)

#*******************************************************************************

# 2. KNN with tuned k
# Scale numerical features for KNN
numerical_cols <- c("age_years", "bmi", "ap_hi", "ap_lo", "bmi_cholesterol")
preProc <- preProcess(train_data_subset[, numerical_cols], method = c("center", "scale"))
train_scaled <- train_data_subset
test_scaled <- test_data_subset
train_scaled[, numerical_cols] <- predict(preProc, train_data_subset[, numerical_cols])
test_scaled[, numerical_cols] <- predict(preProc, test_data_subset[, numerical_cols])

# Tune k using a simple loop
k_values <- seq(1, 15, by = 2)
accuracies <- sapply(k_values, function(k) {
  pred <- knn(train = train_scaled[, features], 
              test = test_scaled[, features], 
              cl = train_scaled$cardio, 
              k = k)
  mean(pred == test_scaled$cardio)
})
best_k <- k_values[which.max(accuracies)]
print("Best k for KNN:")
print(best_k)

# Train KNN with best k
knn_pred <- knn(train = train_scaled[, features], 
                test = test_scaled[, features], 
                cl = train_scaled$cardio, 
                k = best_k)
knn_accuracy <- mean(knn_pred == test_data_subset$cardio)
knn_cm <- confusionMatrix(knn_pred, test_data_subset$cardio)
knn_f1 <- knn_cm$byClass["F1"]
knn_roc <- roc(as.numeric(test_data_subset$cardio == "Cardio"), as.numeric(knn_pred == "Cardio"))
knn_auc <- auc(knn_roc)
print("KNN Metrics:")
print(paste("Accuracy:", round(knn_accuracy, 3)))
print(paste("F1-Score:", round(knn_f1, 3)))
print(paste("AUC-ROC:", round(knn_auc, 3)))

# Confusion Matrix for KNN with percentages
print("KNN Confusion Matrix (Counts and Percentages):")
knn_cm_table <- knn_cm$table
knn_cm_percent <- round((knn_cm_table / total_test) * 100, 2)
knn_cm_combined <- matrix(paste0(knn_cm_table, " (", knn_cm_percent, "%)"), nrow = 2)
dimnames(knn_cm_combined) <- dimnames(knn_cm_table)
print(knn_cm_combined)

# Plotting Confusion Matrix for KNN
knn_cm_df <- as.data.frame(as.table(knn_cm$table))
colnames(knn_cm_df) <- c("Predicted", "Actual", "Freq")
p2 <- ggplot(knn_cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix: KNN",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()
print(p2)

#*******************************************************************************

# 3. Random Forest with tuned mtry
# Fix: Set metric = "ROC" to match summaryFunction
rf_tuned <- train(cardio ~ ., data = train_data_subset, 
                  method = "rf", 
                  trControl = train_control, 
                  tuneGrid = expand.grid(mtry = 2:5),
                  metric = "ROC")
rf_model <- randomForest(cardio ~ ., data = train_data_subset, ntree = 100, mtry = rf_tuned$bestTune$mtry)
rf_pred <- predict(rf_model, test_data_subset)
rf_accuracy <- mean(rf_pred == test_data_subset$cardio)
rf_cm <- confusionMatrix(rf_pred, test_data_subset$cardio)
rf_f1 <- rf_cm$byClass["F1"]
rf_roc <- roc(as.numeric(test_data_subset$cardio == "Cardio"), as.numeric(rf_pred == "Cardio"))
rf_auc <- auc(rf_roc)
print("Random Forest Metrics:")
print(paste("Accuracy:", round(rf_accuracy, 3)))
print(paste("F1-Score:", round(rf_f1, 3)))
print(paste("AUC-ROC:", round(rf_auc, 3)))

# Confusion Matrix for Random Forest with percentages
print("Random Forest Confusion Matrix (Counts and Percentages):")
rf_cm_table <- rf_cm$table
rf_cm_percent <- round((rf_cm_table / total_test) * 100, 2)
rf_cm_combined <- matrix(paste0(rf_cm_table, " (", rf_cm_percent, "%)"), nrow = 2)
dimnames(rf_cm_combined) <- dimnames(rf_cm_table)
print(rf_cm_combined)

# Plotting Confusion Matrix for Random Forest
rf_cm_df <- as.data.frame(as.table(rf_cm$table))
colnames(rf_cm_df) <- c("Predicted", "Actual", "Freq")
p3 <- ggplot(rf_cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix: Random Forest",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()
print(p3)

#*******************************************************************************

# Bar Chart Comparing Model Accuracies
accuracy_df <- data.frame(
  Model = c("Logistic Regression", "KNN", "Random Forest"),
  Accuracy = c(log_accuracy, knn_accuracy, rf_accuracy)
)

p4 <- ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
  labs(title = "Model Accuracy Comparison",
       x = "Model",
       y = "Accuracy") +
  theme_minimal()
print(p4)

#*******************************************************************************
#SQL
#Connection established
db_password <- Sys.getenv("DB_PASSWORD")
con <- dbConnect(odbc::odbc(), 
                 .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};",
                 Server="localhost", Database="rdataset", UID="root", PWD=db_password,
                 Port=3306)

#import dataset
df <- import("cardio_data_processed.csv")
df <- df %>% select(-c(age,bp_category))
summary(df)
str(df)

#Remove the outliers
Q1 <- quantile(df$bmi, 0.25)
Q3 <- quantile(df$bmi, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
#Number of outliers
sum(df$bmi < lower_bound | df$bmi > upper_bound,na.rm=TRUE) 
df <- df[df$bmi >= lower_bound & df$bmi <= upper_bound, ]
hist(df$bmi, main = "Histogram of BMI", xlab = "BMI", col='skyblue')

#change the id to be in sequence
df$id <- seq_len(nrow(df))

#change types
df$smoke <- as.logical(df$smoke)
df$alco <- as.logical(df$alco)
df$active <- as.logical(df$active)
df$cardio <- as.logical(df$cardio)
df <- as.data.frame(df)

#*******************************************************************************

#loading to sql
dbWriteTable(con, "cardioVascular", df, overwrite = TRUE, row.names = FALSE)

#querying from sql
res <- dbSendQuery(con, "SELECT * FROM cardioVascular WHERE id <= 10")
res <- dbFetch(res)

#Better approach : dbGetQuery()
res <- dbGetQuery(con, "SELECT * FROM cardioVascular WHERE id <= 10") 
print(res)

#list the column names
dbListFields(con, 'cardiovascular')

#more queries
#add a column to the database
dbExecute(con, "ALTER TABLE cardiovascular ADD COLUMN bmi_category VARCHAR(20);")
dbExecute(con, "
  UPDATE cardiovascular
  SET bmi_category = CASE
    WHEN bmi < 18.5 THEN 'Underweight'
    WHEN bmi >= 18.5 AND bmi < 25 THEN 'Normal'
    WHEN bmi >= 25 AND bmi < 30 THEN 'Overweight'
    ELSE 'Obese'
  END
  WHERE bmi IS NOT NULL;
")

# top 10 highest bmi gender, age, bmi
query <- "
SELECT id, age_years, gender, bmi
FROM cardiovascular
ORDER BY bmi DESC
LIMIT 10;
"
dbGetQuery(con, query)

#Get minimum and maximum systolic (ap_hi) and (ap_lo) blood pressure
query <- "
SELECT MIN(ap_hi) AS min_ap_hi, MAX(ap_hi) AS max_ap_hi
FROM cardiovascular;
"
dbGetQuery(con, query)

query <- "
SELECT MIN(ap_lo) AS min_ap_low, MAX(ap_lo) AS max_ap_low
FROM cardiovascular;
"
dbGetQuery(con,query) 

#List active smokers over 50 years old
query <- "
SELECT id, age_years, smoke
FROM cardiovascular
WHERE smoke =1 AND age_years > 50
ORDER BY age_years DESC LIMIT 10;
"
dbGetQuery(con, query)

#Find patients with 'Hypertension stage 2' in their blood pressure category label
get_bp_category_data <- function(con, category) {
  category <- DBI::dbQuoteString(con, category)
  
  query <- sprintf("
    SELECT id, ap_hi, ap_lo, bp_category_encoded
    FROM cardiovascular
    WHERE bp_category_encoded = %s
    LIMIT 10;", category)
  
  result <- dbGetQuery(con, query)
  return(result)
}
# Get rows for Hypertension Stage 2
get_bp_category_data(con, "Hypertension Stage 2")
# Get rows for Normal
get_bp_category_data(con, "Normal")

#Show cholesterol level distribution
query <- "
SELECT cholesterol, COUNT(*) AS patient_count
FROM cardiovascular
GROUP BY cholesterol
ORDER BY cholesterol;
"
cholestrol_dist = dbGetQuery(con, query)

# ploting graph to see the distribution
ggplot(cholestrol_dist, aes(x = factor(cholesterol), y = patient_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Cholesterol Level Distribution",
    x = "Cholesterol Level",
    y = "Number of Patients"
  ) +
  theme_minimal()

#Average BMI grouped by age group
query <- "
SELECT
  CASE
    WHEN age_years < 40 THEN 'Under 40'
    WHEN age_years BETWEEN 40 AND 59 THEN '40–59'
    ELSE '60+'
  END AS age_group,
  ROUND(AVG(bmi), 2) AS avg_bmi
FROM cardiovascular
GROUP BY age_group;
"
dbGetQuery(con, query)

#Correlation Between Smoking and Cardiovascular Disease
query <- "
SELECT smoke, cardio, COUNT(*) AS total_patients
FROM cardiovascular
GROUP BY smoke, cardio
ORDER BY smoke, cardio;
"
smoke_vs_cardio <- dbGetQuery(con, query)
print(smoke_vs_cardio)
smoke_vs_cardio$smoke <- factor(smoke_vs_cardio$smoke, labels = c("Non-Smoker", "Smoker"))
smoke_vs_cardio$cardio <- factor(smoke_vs_cardio$cardio, labels = c("No Disease", "Disease"))

ggplot(smoke_vs_cardio, aes(x = smoke, y = total_patients, fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Smoking vs Cardiovascular Disease",
    x = "Smoking Status",
    y = "Number of Patients",
    fill = "Cardiovascular Disease"
  ) +
  theme_minimal()

#Average BMI by Cardiovascular Risk and Gender
query <- "
SELECT gender, cardio, ROUND(AVG(bmi), 2) AS avg_bmi
FROM cardiovascular
GROUP BY gender, cardio
ORDER BY gender, cardio;
"
bmi_by_gender_cardio <- dbGetQuery(con, query)
print(bmi_by_gender_cardio)
bmi_by_gender_cardio$gender <- factor(bmi_by_gender_cardio$gender, labels = c("Female", "Male"))
bmi_by_gender_cardio$cardio <- factor(bmi_by_gender_cardio$cardio, labels = c("No Disease", "Disease"))

ggplot(bmi_by_gender_cardio, aes(x=gender, y=avg_bmi,fill=cardio))+
  geom_bar(stat="identity", position="dodge")+
  labs(
    title = "Avg_BMI distribution among gender wrt CardioVascular disease",
    x = "Gender",
    y = "Average BMI",
    fill = "Cardiovascular Disease"
  )+
  theme_minimal()

#Age vs. Cardiovascular Disease
query <- "
SELECT cardio, ROUND(AVG(age_years), 1) AS avg_age, COUNT(*) AS total
FROM cardiovascular
GROUP BY cardio;
"
age_vs_cardio <- dbGetQuery(con, query)
print(age_vs_cardio)

#High Blood Pressure and Cardio Risk
query <- "
SELECT COUNT(*) AS high_bp_and_cardio_cases
FROM cardiovascular
WHERE ap_hi > 140 AND ap_lo > 90 AND cardio = 1;
"
high_bp_cardio <- dbGetQuery(con, query)
print(high_bp_cardio)

#Cholesterol Level Impact on Heart Disease
query <- "
SELECT cholesterol, cardio, COUNT(*) AS total_patients
FROM cardiovascular
GROUP BY cholesterol, cardio
ORDER BY cholesterol, cardio;
"
chol_vs_cardio <- dbGetQuery(con, query)
print(chol_vs_cardio)

chol_vs_cardio$cholesterol <- factor(chol_vs_cardio$cholesterol, labels = c("Normal", "Above Normal", "Well Above Normal"))
chol_vs_cardio$cardio <- factor(chol_vs_cardio$cardio, labels = c("No Disease", "Disease"))

ggplot(chol_vs_cardio, aes(x=cholesterol, y=total_patients,fill=cardio))+
  geom_bar(stat="identity", position="dodge")+
  labs(
    title = "Cholesterol Level Impact on Heart Disease",
    x = "Cholestrol Level",
    y = "Total Patients",
    fill = "Cardiovascular Disease"
  )+
  theme_minimal()

#Active Lifestyle vs. Cardio
query <- "
SELECT active, cardio, COUNT(*) AS count
FROM cardiovascular
GROUP BY active, cardio;
"
active_vs_cardio <- dbGetQuery(con, query)
print(active_vs_cardio)

active_vs_cardio$active <- factor(
  active_vs_cardio$active,
  levels = c(0, 1),
  labels = c("Inactive", "Active")
)
active_vs_cardio$cardio <- factor(
  active_vs_cardio$cardio,
  levels = c(0, 1),
  labels = c("No Disease", "Disease")
)

ggplot(active_vs_cardio, aes(x = active, y = count, fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Cardiovascular Disease by Activity Level",
    x = "Physical Activity",
    y = "Number of Patients",
    fill = "Cardiovascular Disease"
  ) +
  theme_minimal()

#BMI Categories vs. Cardio Risk
query <- "
SELECT
  CASE
    WHEN bmi < 18.5 THEN 'Underweight'
    WHEN bmi BETWEEN 18.5 AND 24.9 THEN 'Normal'
    WHEN bmi BETWEEN 25 AND 29.9 THEN 'Overweight'
    ELSE 'Obese'
  END AS bmi_category,
  cardio,
  COUNT(*) AS total_patients
FROM cardiovascular
GROUP BY bmi_category, cardio
ORDER BY bmi_category, cardio;
"
bmi_cat_vs_cardio <- dbGetQuery(con, query)
print(bmi_cat_vs_cardio)

bmi_cat_vs_cardio$cardio <- factor(
  bmi_cat_vs_cardio$cardio,
  levels = c(0, 1),
  labels = c("No Disease", "Disease")
)
bmi_cat_vs_cardio$bmi_category <- factor(
  bmi_cat_vs_cardio$bmi_category,
  levels = c("Underweight", "Normal", "Overweight", "Obese")
)

ggplot(bmi_cat_vs_cardio, aes(x = bmi_category, y = total_patients, fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Cardiovascular Risk by BMI Category",
    x = "BMI Category",
    y = "Number of Patients",
    fill = "Cardiovascular Disease"
  ) +
  theme_minimal()

#summary of the dataset
cols <- dbListFields(con, "cardiovascular")

summary_parts <- unlist(lapply(cols, function(col) {
  paste0(
    "MIN(", col, ") AS min_", col, ", ",
    "MAX(", col, ") AS max_", col, ", ",
    "AVG(", col, ") AS avg_", col, ", ",
    "STDDEV(", col, ") AS sd_", col
  )
}))

summary_query <- paste("SELECT ", paste(summary_parts, collapse = ", "), " FROM cardiovascular")
summary_stats <- dbGetQuery(con, summary_query)
print(summary_stats)

#Correlation matrix 
num_df <- dbGetQuery(con, "
SELECT height, weight, ap_hi, ap_lo, cholesterol, gluc, bmi, age_years 
FROM cardiovascular
")
cor_matrix <- cor(num_df, use = "complete.obs")
ggcorrplot(cor_matrix,
           method = "square",
           type = "lower",
           lab = TRUE,               # Add correlation coefficients
           lab_size = 3,
           colors = c("blue", "white", "red"),
           title = "Correlation Matrix of Cardiovascular Features",
           ggtheme = ggplot2::theme_minimal())

# Fetch the full data (excluding 'age' and 'bp_category')
query <- "SELECT gender, height, weight, ap_hi, ap_lo, cholesterol, gluc, smoke, alco, active, cardio FROM cardiovascular;"
df <- dbGetQuery(con, query)

# Logistic regression to predict 'cardio' (heart disease)
model <- glm(cardio ~ height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active, data = df, family = binomial)
summary(model)

#command to drop the table if necessary
#dbExecute(con, "DROP TABLE IF EXISTS cardiovascular;")

#disconnect
dbDisconnect(con)

#*******************************************************************************