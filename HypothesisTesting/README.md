# Cardiovascular Data Analysis in R

## Overview
This project analyzes the `cardio_data_processed.csv` dataset to explore relationships between health metrics and cardiovascular disease using statistical tests in R. The dataset contains 70,000 records with variables like BMI, blood pressure, cholesterol, smoking status, and cardiovascular disease presence.

## Prerequisites
- **R Environment**: Ensure R is installed.
- **Packages**: The script installs `ggplot2` (for plotting) and `dplyr` (for data manipulation) if not already present.

## Analysis Performed

### 1. T-test: BMI vs Cardiovascular Disease
- **Objective**: Test if BMI differs between patients with and without cardiovascular disease.
- **Hypothesis**:
  - Null (H0): Mean BMI is the same for both groups.
  - Alternative (H1): Mean BMI differs.
- **Method**: Two-sample T-test (Welch's, assuming unequal variances).
- **Result**: The T-test yields a p-value (to be computed at runtime). If p < 0.05, we reject H0, indicating a significant difference in BMI between the groups.
- **Visualization**: A boxplot (`bmi_vs_cardio_boxplot.png`) shows the BMI distribution for patients with and without cardiovascular disease.

### 2. Paired T-test: Systolic vs Diastolic Blood Pressure
- **Objective**: Compare `ap_hi` (systolic BP) and `ap_lo` (diastolic BP) within individuals.
- **Note**: This dataset doesn't have true paired data (e.g., before/after). This test is for demonstration, comparing two related measures.
- **Hypothesis**:
  - Null (H0): Mean difference between `ap_hi` and `ap_lo` is zero.
  - Alternative (H1): Mean difference is not zero.
- **Method**: Paired T-test.
- **Result**: The p-value (computed at runtime) determines significance. A small p-value (< 0.05) suggests a significant difference between systolic and diastolic BP.
- **Visualization**: No plot for this test due to the nature of the comparison.

### 3. Correlation: BMI vs Systolic Blood Pressure
- **Objective**: Assess the relationship between BMI and systolic blood pressure (`ap_hi`).
- **Hypothesis**:
  - Null (H0): No correlation (correlation coefficient = 0).
  - Alternative (H1): There is a correlation.
- **Method**: Pearson correlation test.
- **Result**: The correlation coefficient and p-value (computed at runtime) indicate the strength and significance of the relationship. A positive coefficient with p < 0.05 suggests higher BMI is associated with higher systolic BP.
- **Visualization**: A scatterplot with a regression line (`bmi_vs_aphi_scatterplot.png`) visualizes the relationship.

### 4. Chi-Square Test: Smoking vs Cardiovascular Disease
- **Objective**: Test if smoking status is associated with cardiovascular disease.
- **Hypothesis**:
  - Null (H0): Smoking and cardiovascular disease are independent.
  - Alternative (H1): There is an association.
- **Method**: Chi-Square test of independence.
- **Result**: A p-value < 0.05 (computed at runtime) indicates a significant association between smoking and cardiovascular disease.
- **Visualization**: A bar plot (`smoke_vs_cardio_barplot.png`) shows the counts of smokers and non-smokers with and without cardiovascular disease.

### 5. ANOVA: BMI vs Cholesterol Levels
- **Objective**: Compare BMI across different cholesterol levels (Normal, Above Normal, High).
- **Hypothesis**:
  - Null (H0): Mean BMI is the same across all cholesterol levels.
  - Alternative (H1): At least one group differs.
- **Method**: One-way ANOVA.
- **Result**: A p-value < 0.05 (computed at runtime) suggests that BMI differs significantly across cholesterol levels.
- **Visualization**: A boxplot (`bmi_vs_cholesterol_boxplot.png`) displays BMI distribution across cholesterol levels.

## How to Run
1. Place `cardio_data_processed.csv` and `cardio_analysis.R` in the same directory.
2. Open R or RStudio.
3. Set the working directory to the folder containing the files:
   ```R
   setwd("path/to/directory")
   ```
4. Run the script:
   ```R
   source("cardio_analysis.R")
   ```
5. Check the console for test results and the directory for generated plots.

## Outputs
- **Console Output**: Results of T-test, Paired T-test, Correlation, Chi-Square test, and ANOVA.
- **Plots**:
  - `bmi_vs_cardio_boxplot.png`
  - `bmi_vs_aphi_scatterplot.png`
  - `smoke_vs_cardio_barplot.png`
  - `bmi_vs_cholesterol_boxplot.png`

## Notes
- The Paired T-test is a demonstration since the dataset lacks true paired data.
- Results depend on the data; p-values and coefficients will be computed at runtime.
- Visualizations help interpret the statistical findings, showing distributions and relationships clearly.