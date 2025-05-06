# Installing required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

# Loading the libraries
library(ggplot2)
library(dplyr)

# Importing the dataset
data <- read.csv("Documents/RIT/R_Project/cardio_data_processed.csv")

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

# 2. Paired T-test: Comparing ap_hi and ap_lo (demonstration purposes)
# Note: This isn't a true paired test scenario since ap_hi and ap_lo aren't before/after measures
paired_t_test_result <- t.test(data$ap_hi, data$ap_lo, paired = TRUE)
print("Paired T-test Result (ap_hi vs ap_lo):")
print(paired_t_test_result)

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