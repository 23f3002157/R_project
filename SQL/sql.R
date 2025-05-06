#load packages
pacman::p_load(pacman,dplyr,ggplot2,GGally,ggthemes,ggvis,httr,DBI,
               lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr, ggcorrplot)

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