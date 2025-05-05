# Cardiovascular Disease Analysis

This repository contains R code for analyzing a cardiovascular disease dataset, `(K)cardio_data_processed.csv`. The analysis includes data preprocessing, normalization, visualizations (scatter and box plots), correlation analysis, chi-square tests, and logistic regression to explore relationships between variables such as age, systolic blood pressure (`ap_hi`), cholesterol, glucose, BMI, and the presence of cardiovascular disease (`cardio`).

## Dataset
The dataset `(K)cardio_data_processed.csv` includes health-related variables for patients, such as:
- `id`: Unique patient identifier
- `age`: Age in days
- `age_years`: Age in years
- `gender`: Gender (1 or 2)
- `height`: Height in cm
- `weight`: Weight in kg
- `ap_hi`: Systolic blood pressure (mmHg)
- `ap_lo`: Diastolic blood pressure (mmHg)
- `cholesterol`: Cholesterol level (1 = normal, 2 = above normal, 3 = well above normal)
- `gluc`: Glucose level (1 = normal, 2 = above normal, 3 = well above normal)
- `smoke`: Smoking status (0 = no, 1 = yes)
- `alco`: Alcohol consumption (0 = no, 1 = yes)
- `active`: Physical activity (0 = no, 1 = yes)
- `cardio`: Cardiovascular disease presence (0 = no, 1 = yes)
- `bmi`: Body Mass Index
- `bp_category`: Blood pressure category
- `bp_category_encoded`: Encoded blood pressure category

## Requirements
To run the code, install the following R packages:
```R
install.packages(c("tidyverse", "ggplot2", "MASS", "corrplot", "rcompanion", "dplyr"))
```
- **R version**: 4.0 or higher recommended.
- **Dataset**: Place `(K)cardio_data_processed.csv` in the working directory.

## File Structure
- `analysis.R`: Main R script containing all preprocessing, normalization, visualization, and statistical analysis.
- `(K)cardio_data_processed.csv`: Input dataset (not included in the repository; user must provide).
- `README.md`: This file.

## Instructions
1. **Clone the Repository**:
   ```bash
   git clone https://github.com/yourusername/cardio-analysis.git
   ```
2. **Set Working Directory**:
   Place `(K)cardio_data_processed.csv` in the project directory and set it as the R working directory:
   ```R
   setwd("path/to/cardio-analysis")
   ```
3. **Install Dependencies**:
   Run the package installation command above in R.
4. **Run the Script**:
   Execute `analysis.R` in R or RStudio:
   ```R
   source("analysis.R")
   ```
5. **View Outputs**:
   - Visualizations (scatter and box plots) will appear in the R plot window.
   - Statistical results (correlations, chi-square tests, logistic regression summaries) will print to the console.

## Code Overview
The R script performs the following tasks:
1. **Data Loading and Exploration**:
   - Loads the dataset and provides basic summaries (`summary()`, `head()`, `str()`, etc.).
2. **Preprocessing**:
   - Removes rows with missing values.
   - Converts categorical variables (`gender`, `cholesterol`, etc.) to factors.
   - Removes unnecessary columns (`id`, `bp_category_encoded` if redundant).
   - Removes outliers for numeric columns (`age_years`, `height`, `weight`, `ap_hi`, `ap_lo`, `bmi`) using the IQR method.
   - Applies plausibility checks (e.g., realistic blood pressure ranges).
3. **Normalization**:
   - Normalizes numeric columns to [0, 1] using min-max normalization.
4. **Visualizations**:
   - **Scatter Plots**: Relationships between `age_years`, `ap_hi`, `cholesterol`, `gluc`, etc., colored by `cardio`.
   - **Box Plots**: Distributions of `weight`, `age_years`, `ap_hi`, `bmi`, `height` by `cardio`, `cholesterol`, `gluc`, or `gender`.
5. **Statistical Analysis**:
   - **Correlation**: Pearson correlations between numeric and categorical variables.
   - **Chi-Square Tests**: Associations between categorical variables (e.g., `cholesterol` vs. `gluc`).
   - **Cramer’s V**: Strength of association for categorical variables.
   - **Logistic Regression**: Models `cardio` as a function of predictors like `age_years`, `ap_hi`, `cholesterol`, `gluc`, etc.

## Outputs
- **Visualizations**:
  - Scatter plots showing relationships (e.g., `age_years` vs. `ap_hi`, colored by `cardio`).
  - Box plots comparing distributions (e.g., `bmi` by `cardio`, `height` by `gender`).
- **Statistical Results**:
  - Correlation coefficients (e.g., `cor(data$age_years, data$ap_hi)`).
  - Chi-square test p-values and Cramer’s V for categorical associations.
  - Logistic regression summaries with coefficients, p-values, and model fit statistics (e.g., AIC, deviance).
- **Normalized Data**: A dataset (`modified_data_normalized`) with numeric columns scaled to [0, 1].

## Notes
- Ensure `(K)cardio_data_processed.csv` is in the correct format and location.
- Some chi-square tests may produce warnings if table cells have low counts; results should be interpreted cautiously.
- Outlier removal and normalization are tailored to the dataset’s characteristics (e.g., realistic ranges for `ap_hi`: 50–250 mmHg).
- The code assumes `gender` is coded as 1 or 2; adjust labels if needed (e.g., 1 = Female, 2 = Male).

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact
For questions or contributions, open an issue or submit a pull request on GitHub.