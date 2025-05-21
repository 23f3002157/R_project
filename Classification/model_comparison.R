# Installing required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
if (!require("class")) install.packages("class")
if (!require("ROSE")) install.packages("ROSE")
if (!require("glmnet")) install.packages("glmnet")
if (!require("pROC")) install.packages("pROC")
if (!require("rpart")) install.packages("rpart")
if (!require("PRROC")) install.packages("PRROC")
if (!require("cowplot")) install.packages("cowplot")
if (!require("xgboost")) install.packages("xgboost")
if (!require("catboost")) install.packages("catboost")

# Loading the libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(class)
library(ROSE)
library(glmnet)
library(pROC)
library(rpart)
library(PRROC)
library(cowplot)
library(xgboost)
library(catboost)

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

# Address class imbalance using ROSE
train_data <- ovun.sample(cardio ~ ., data = train_data, method = "both", 
                          p = 0.5, seed = 123)$data

# Feature Selection using Random Forest
initial_rf <- randomForest(cardio ~ ., data = train_data, ntree = 50)
importance_scores <- importance(initial_rf)
important_features <- names(importance_scores[order(importance_scores, decreasing = TRUE), ][1:8])
cat("Top 8 Important Features:\n")
print(important_features)

# Prepare data with selected features and interaction
train_data_subset <- train_data[, c(important_features, "cardio")]
train_data_subset$bmi_cholesterol <- train_data$bmi * as.numeric(train_data$cholesterol)
test_data_subset <- test_data[, c(important_features, "cardio")]
test_data_subset$bmi_cholesterol <- test_data$bmi * as.numeric(test_data$cholesterol)

# Update features list
features <- c(important_features, "bmi_cholesterol")

# Total number of test predictions
total_test <- nrow(test_data)

# Define cross-validation control
train_control <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)

# 1. Logistic Regression with glmnet
x_train <- model.matrix(cardio ~ ., data = train_data_subset)[, -1]
x_test <- model.matrix(cardio ~ ., data = test_data_subset)[, -1]
y_train <- train_data_subset$cardio
log_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, nfolds = 5)
log_pred_prob <- predict(log_model, newx = x_test, type = "response", s = "lambda.min")
log_pred_prob_vec <- as.numeric(log_pred_prob)
roc_obj <- roc(test_data_subset$cardio, log_pred_prob_vec, quiet = TRUE)
best_threshold <- coords(roc_obj, "best", ret = "threshold")$threshold
log_pred <- ifelse(log_pred_prob_vec > best_threshold, "Cardio", "NoCardio")
log_pred <- factor(log_pred, levels = c("NoCardio", "Cardio"))
log_cm <- confusionMatrix(log_pred, test_data_subset$cardio, positive = "Cardio")
log_accuracy <- mean(log_pred == test_data_subset$cardio)
log_precision <- log_cm$byClass["Pos Pred Value"]
log_recall <- log_cm$byClass["Sensitivity"]
log_f1 <- log_cm$byClass["F1"]
log_auc <- auc(roc_obj)
cat("Logistic Regression Metrics:\n")
cat("Accuracy:", round(log_accuracy, 3), "\n")
cat("Precision:", round(log_precision, 3), "\n")
cat("Recall:", round(log_recall, 3), "\n")
cat("F1-Score:", round(log_f1, 3), "\n")
cat("AUC-ROC:", round(log_auc, 3), "\n")

# Confusion Matrix for Logistic Regression
cat("Logistic Regression Confusion Matrix (Counts and Percentages):\n")
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

# 2. KNN
numerical_cols <- c("age_years", "bmi", "ap_hi", "ap_lo", "bmi_cholesterol")
preProc <- preProcess(train_data_subset[, numerical_cols], method = c("center", "scale"))
train_scaled <- train_data_subset
test_scaled <- test_data_subset
train_scaled[, numerical_cols] <- predict(preProc, train_data_subset[, numerical_cols])
test_scaled[, numerical_cols] <- predict(preProc, test_data_subset[, numerical_cols])
k_values <- seq(1, 15, by = 2)
accuracies <- sapply(k_values, function(k) {
  pred <- knn(train = train_scaled[, features], 
              test = test_scaled[, features], 
              cl = train_scaled$cardio, 
              k = k)
  mean(pred == test_scaled$cardio)
})
best_k <- k_values[which.max(accuracies)]
cat("Best k for KNN:", best_k, "\n")
knn_pred <- knn(train = train_scaled[, features], 
                test = test_scaled[, features], 
                cl = train_scaled$cardio, 
                k = best_k)
# Use knn.probability to get pseudo-probabilities
knn_prob <- attr(knn(train = train_scaled[, features], 
                     test = test_scaled[, features], 
                     cl = train_scaled$cardio, 
                     k = best_k, prob = TRUE), "prob")
knn_pred_prob <- ifelse(knn_pred == "Cardio", knn_prob, 1 - knn_prob)  # Probability for "Cardio"
knn_cm <- confusionMatrix(knn_pred, test_data_subset$cardio, positive = "Cardio")
knn_accuracy <- mean(knn_pred == test_data_subset$cardio)
knn_precision <- knn_cm$byClass["Pos Pred Value"]
knn_recall <- knn_cm$byClass["Sensitivity"]
knn_f1 <- knn_cm$byClass["F1"]
knn_roc <- roc(as.numeric(test_data_subset$cardio == "Cardio"), knn_pred_prob, quiet = TRUE)
knn_auc <- auc(knn_roc)
cat("KNN Metrics:\n")
cat("Accuracy:", round(knn_accuracy, 3), "\n")
cat("Precision:", round(knn_precision, 3), "\n")
cat("Recall:", round(knn_recall, 3), "\n")
cat("F1-Score:", round(knn_f1, 3), "\n")
cat("AUC-ROC:", round(knn_auc, 3), "\n")

# Confusion Matrix for KNN
cat("KNN Confusion Matrix (Counts and Percentages):\n")
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

# 3. Random Forest
rf_tuned <- train(cardio ~ ., data = train_data_subset, 
                  method = "rf", 
                  trControl = train_control, 
                  tuneGrid = expand.grid(mtry = 2:5),
                  metric = "ROC")
rf_model <- randomForest(cardio ~ ., data = train_data_subset, ntree = 100, mtry = rf_tuned$bestTune$mtry)
rf_pred <- predict(rf_model, test_data_subset)
rf_pred_prob <- predict(rf_model, test_data_subset, type = "prob")[, "Cardio"]
rf_cm <- confusionMatrix(rf_pred, test_data_subset$cardio, positive = "Cardio")
rf_accuracy <- mean(rf_pred == test_data_subset$cardio)
rf_precision <- rf_cm$byClass["Pos Pred Value"]
rf_recall <- rf_cm$byClass["Sensitivity"]
rf_f1 <- rf_cm$byClass["F1"]
rf_roc <- roc(as.numeric(test_data_subset$cardio == "Cardio"), rf_pred_prob, quiet = TRUE)
rf_auc <- auc(rf_roc)
cat("Random Forest Metrics:\n")
cat("Accuracy:", round(rf_accuracy, 3), "\n")
cat("Precision:", round(rf_precision, 3), "\n")
cat("Recall:", round(rf_recall, 3), "\n")
cat("F1-Score:", round(rf_f1, 3), "\n")
cat("AUC-ROC:", round(rf_auc, 3), "\n")

# Confusion Matrix for Random Forest
cat("Random Forest Confusion Matrix (Counts and Percentages):\n")
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

# 4. Decision Tree
dt_grid <- expand.grid(cp = c(0.001, 0.01, 0.1))
dt_model <- train(
  cardio ~ .,
  data = train_data_subset,
  method = "rpart",
  trControl = train_control,
  tuneGrid = dt_grid,
  metric = "ROC"
)
dt_pred_prob <- predict(dt_model, newdata = test_data_subset, type = "prob")[, "Cardio"]
roc_obj_dt <- roc(test_data_subset$cardio, dt_pred_prob, quiet = TRUE)
best_threshold_dt <- coords(roc_obj_dt, "best", ret = "threshold")$threshold
dt_pred <- ifelse(dt_pred_prob > best_threshold_dt, "Cardio", "NoCardio")
dt_pred <- factor(dt_pred, levels = c("NoCardio", "Cardio"))
dt_cm <- confusionMatrix(dt_pred, test_data_subset$cardio, positive = "Cardio")
dt_accuracy <- mean(dt_pred == test_data_subset$cardio)
dt_precision <- dt_cm$byClass["Pos Pred Value"]
dt_recall <- dt_cm$byClass["Sensitivity"]
dt_f1 <- dt_cm$byClass["F1"]
dt_auc <- auc(roc_obj_dt)
cat("Decision Tree Metrics:\n")
cat("Accuracy:", round(dt_accuracy, 3), "\n")
cat("Precision:", round(dt_precision, 3), "\n")
cat("Recall:", round(dt_recall, 3), "\n")
cat("F1-Score:", round(dt_f1, 3), "\n")
cat("AUC-ROC:", round(dt_auc, 3), "\n")

# Confusion Matrix for Decision Tree
cat("Decision Tree Confusion Matrix (Counts and Percentages):\n")
dt_cm_table <- dt_cm$table
dt_cm_percent <- round((dt_cm_table / total_test) * 100, 2)
dt_cm_combined <- matrix(paste0(dt_cm_table, " (", dt_cm_percent, "%)"), nrow = 2)
dimnames(dt_cm_combined) <- dimnames(dt_cm_table)
print(dt_cm_combined)

# Plotting Confusion Matrix for Decision Tree
dt_cm_df <- as.data.frame(as.table(dt_cm$table))
colnames(dt_cm_df) <- c("Predicted", "Actual", "Freq")
p4 <- ggplot(dt_cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix: Decision Tree",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()
print(p4)

# 5. XGBoost
# Prepare data for XGBoost
xgb_train_data <- xgb.DMatrix(data = x_train, label = as.numeric(y_train == "Cardio"))
xgb_test_data <- xgb.DMatrix(data = x_test, label = as.numeric(test_data_subset$cardio == "Cardio"))
xgb_params <- list(
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 6,
  nrounds = 100
)
xgb_model <- xgboost(data = xgb_train_data, params = xgb_params, nrounds = 100, verbose = 0)
xgb_pred_prob <- predict(xgb_model, xgb_test_data)
xgb_roc <- roc(as.numeric(test_data_subset$cardio == "Cardio"), xgb_pred_prob, quiet = TRUE)
xgb_best_threshold <- coords(xgb_roc, "best", ret = "threshold")$threshold
xgb_pred <- ifelse(xgb_pred_prob > xgb_best_threshold, "Cardio", "NoCardio")
xgb_pred <- factor(xgb_pred, levels = c("NoCardio", "Cardio"))
xgb_cm <- confusionMatrix(xgb_pred, test_data_subset$cardio, positive = "Cardio")
xgb_accuracy <- mean(xgb_pred == test_data_subset$cardio)
xgb_precision <- xgb_cm$byClass["Pos Pred Value"]
xgb_recall <- xgb_cm$byClass["Sensitivity"]
xgb_f1 <- xgb_cm$byClass["F1"]
xgb_auc <- auc(xgb_roc)
cat("XGBoost Metrics:\n")
cat("Accuracy:", round(xgb_accuracy, 3), "\n")
cat("Precision:", round(xgb_precision, 3), "\n")
cat("Recall:", round(xgb_recall, 3), "\n")
cat("F1-Score:", round(xgb_f1, 3), "\n")
cat("AUC-ROC:", round(xgb_auc, 3), "\n")

# Confusion Matrix for XGBoost
cat("XGBoost Confusion Matrix (Counts and Percentages):\n")
xgb_cm_table <- xgb_cm$table
xgb_cm_percent <- round((xgb_cm_table / total_test) * 100, 2)
xgb_cm_combined <- matrix(paste0(xgb_cm_table, " (", xgb_cm_percent, "%)"), nrow = 2)
dimnames(xgb_cm_combined) <- dimnames(xgb_cm_table)
print(xgb_cm_combined)

# Plotting Confusion Matrix for XGBoost
xgb_cm_df <- as.data.frame(as.table(xgb_cm$table))
colnames(xgb_cm_df) <- c("Predicted", "Actual", "Freq")
p5 <- ggplot(xgb_cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix: XGBoost",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()
print(p5)

# 6. CatBoost
# Prepare data for CatBoost
cat_train_pool <- catboost.load_pool(data = as.data.frame(x_train), label = as.numeric(y_train == "Cardio"))
cat_test_pool <- catboost.load_pool(data = as.data.frame(x_test), label = as.numeric(test_data_subset$cardio == "Cardio"))
cat_params <- list(
  iterations = 100,
  depth = 6,
  learning_rate = 0.1,
  loss_function = "Logloss",
  verbose = 0
)
cat_model <- catboost.train(cat_train_pool, params = cat_params)
catboost_pred_prob <- catboost.predict(cat_model, cat_test_pool, prediction_type = "Probability")
cat_roc <- roc(as.numeric(test_data_subset$cardio == "Cardio"), catboost_pred_prob, quiet = TRUE)
cat_best_threshold <- coords(cat_roc, "best", ret = "threshold")$threshold
cat_pred <- ifelse(catboost_pred_prob > cat_best_threshold, "Cardio", "NoCardio")
cat_pred <- factor(cat_pred, levels = c("NoCardio", "Cardio"))
cat_cm <- confusionMatrix(cat_pred, test_data_subset$cardio, positive = "Cardio")
cat_accuracy <- mean(cat_pred == test_data_subset$cardio)
cat_precision <- cat_cm$byClass["Pos Pred Value"]
cat_recall <- cat_cm$byClass["Sensitivity"]
cat_f1 <- cat_cm$byClass["F1"]
cat_auc <- auc(cat_roc)
cat("CatBoost Metrics:\n")
cat("Accuracy:", round(cat_accuracy, 3), "\n")
cat("Precision:", round(cat_precision, 3), "\n")
cat("Recall:", round(cat_recall, 3), "\n")
cat("F1-Score:", round(cat_f1, 3), "\n")
cat("AUC-ROC:", round(cat_auc, 3), "\n")

# Confusion Matrix for CatBoost
cat("CatBoost Confusion Matrix (Counts and Percentages):\n")
cat_cm_table <- cat_cm$table
cat_cm_percent <- round((cat_cm_table / total_test) * 100, 2)
cat_cm_combined <- matrix(paste0(cat_cm_table, " (", cat_cm_percent, "%)"), nrow = 2)
dimnames(cat_cm_combined) <- dimnames(cat_cm_table)
print(cat_cm_combined)

# Plotting Confusion Matrix for CatBoost
cat_cm_df <- as.data.frame(as.table(cat_cm$table))
colnames(cat_cm_df) <- c("Predicted", "Actual", "Freq")
p6 <- ggplot(cat_cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix: CatBoost",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()
print(p6)

# Calibration Curve Plot with ECE and Brier Score
# Function to compute calibration data manually
compute_calibration <- function(pred_prob, actual, bins = 10) {
  pred_prob <- pmin(pmax(pred_prob, 0), 1)  # Clamp probabilities between 0 and 1
  actual_binary <- as.numeric(actual == "Cardio")
  breaks <- seq(0, 1, length.out = bins + 1)
  bin_indices <- cut(pred_prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  calibration_df <- data.frame(
    Midpoint = (breaks[-1] + breaks[-length(breaks)]) / 2,
    Predicted = numeric(bins),
    Observed = numeric(bins),
    Count = numeric(bins)
  )
  for (i in 1:bins) {
    bin_mask <- bin_indices == i
    if (sum(bin_mask) > 0) {
      calibration_df$Predicted[i] <- mean(pred_prob[bin_mask])
      calibration_df$Observed[i] <- mean(actual_binary[bin_mask])
      calibration_df$Count[i] <- sum(bin_mask)
    } else {
      calibration_df$Predicted[i] <- calibration_df$Midpoint[i]
      calibration_df$Observed[i] <- 0
      calibration_df$Count[i] <- 0
    }
  }
  return(calibration_df)
}

# Define a custom compute_ece function
compute_ece <- function(pred_prob, actual, bins = 10) {
  pred_prob <- pmin(pmax(pred_prob, 0), 1)
  actual_binary <- as.numeric(actual == "Cardio")
  breaks <- seq(0, 1, length.out = bins + 1)
  bin_indices <- cut(pred_prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  ece <- 0
  n <- length(pred_prob)
  for (i in 1:bins) {
    bin_mask <- bin_indices == i
    if (sum(bin_mask) > 0) {
      bin_pred <- mean(pred_prob[bin_mask])
      bin_actual <- mean(actual_binary[bin_mask])
      ece <- ece + (sum(bin_mask) / n) * abs(bin_pred - bin_actual)
    }
  }
  return(ece)
}

# Prepare calibration data for all models
cal_log <- compute_calibration(log_pred_prob_vec, test_data_subset$cardio)
cal_log$Model <- "Logistic Regression"
cal_knn <- compute_calibration(knn_pred_prob, test_data_subset$cardio)
cal_knn$Model <- "KNN"
cal_rf <- compute_calibration(rf_pred_prob, test_data_subset$cardio)
cal_rf$Model <- "Random Forest"
cal_dt <- compute_calibration(dt_pred_prob, test_data_subset$cardio)
cal_dt$Model <- "Decision Tree"
cal_xgb <- compute_calibration(xgb_pred_prob, test_data_subset$cardio)
cal_xgb$Model <- "XGBoost"
cal_cat <- compute_calibration(catboost_pred_prob, test_data_subset$cardio)
cal_cat$Model <- "CatBoost"

# Combine calibration data
calibration_df <- rbind(cal_log, cal_knn, cal_rf, cal_dt, cal_xgb, cal_cat)

# Plot calibration curves
cal_plot <- ggplot(calibration_df, aes(x = Predicted, y = Observed, color = Model)) +
  geom_line(size = 1) +
  geom_point(aes(size = Count), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Calibration Curves for All Models",
       x = "Predicted Probability (Cardio)",
       y = "Observed Proportion (Cardio)",
       color = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("CatBoost" = "red", "KNN" = "blue", "Logistic Regression" = "green", 
                                "Random Forest" = "purple", "XGBoost" = "orange", "Decision Tree" = "black")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
print(cal_plot)

# Calculate ECE and Brier Score
actual_binary <- as.numeric(test_data_subset$cardio == "Cardio")
model_names <- c("Logistic Regression", "KNN", "Random Forest", "Decision Tree", "XGBoost", "CatBoost")
ece_scores <- numeric(length(model_names))
brier_scores <- numeric(length(model_names))

# Logistic Regression
ece_scores[1] <- compute_ece(log_pred_prob_vec, test_data_subset$cardio)
brier_scores[1] <- mean((log_pred_prob_vec - as.numeric(test_data_subset$cardio == "Cardio"))^2)

# KNN
ece_scores[2] <- compute_ece(knn_pred_prob, test_data_subset$cardio)
brier_scores[2] <- mean((knn_pred_prob - as.numeric(test_data_subset$cardio == "Cardio"))^2)

# Random Forest
ece_scores[3] <- compute_ece(rf_pred_prob, test_data_subset$cardio)
brier_scores[3] <- mean((rf_pred_prob - as.numeric(test_data_subset$cardio == "Cardio"))^2)

# Decision Tree
ece_scores[4] <- compute_ece(dt_pred_prob, test_data_subset$cardio)
brier_scores[4] <- mean((dt_pred_prob - as.numeric(test_data_subset$cardio == "Cardio"))^2)

# XGBoost
ece_scores[5] <- compute_ece(xgb_pred_prob, test_data_subset$cardio)
brier_scores[5] <- mean((xgb_pred_prob - as.numeric(test_data_subset$cardio == "Cardio"))^2)

# CatBoost
ece_scores[6] <- compute_ece(catboost_pred_prob, test_data_subset$cardio)
brier_scores[6] <- mean((catboost_pred_prob - as.numeric(test_data_subset$cardio == "Cardio"))^2)

# Output calibration metrics table
calibration_metrics <- data.frame(
  Model = model_names,
  ECE = round(ece_scores, 4),
  Brier_Score = round(brier_scores, 4)
)
cat("Calibration Metrics for All Models:\n")
print(calibration_metrics)

# Bar Chart Comparing Model Accuracies
accuracy_df <- data.frame(
  Model = model_names,
  Accuracy = c(log_accuracy, knn_accuracy, rf_accuracy, dt_accuracy, xgb_accuracy, cat_accuracy)
)

p7 <- ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
  labs(title = "Model Accuracy Comparison",
       x = "Model",
       y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7)

# Additional Plots: ROC Curve, Precision-Recall Curve, Residual Plot, Predicted vs Actual Plot
# Validate inputs
cat("Length of actual_binary:", length(actual_binary), "\n")
cat("Unique values in actual_binary:", unique(actual_binary), "\n")
cat("NA count in actual_binary:", sum(is.na(actual_binary)), "\n")

# Check predicted probabilities
pred_probs <- list(
  "Logistic Regression" = log_pred_prob_vec,
  "KNN" = knn_pred_prob,
  "Random Forest" = rf_pred_prob,
  "Decision Tree" = dt_pred_prob,
  "XGBoost" = xgb_pred_prob,
  "CatBoost" = catboost_pred_prob
)

for (model in model_names) {
  cat("Checking", model, ":\n")
  cat("  Length:", length(pred_probs[[model]]), "\n")
  cat("  NA count:", sum(is.na(pred_probs[[model]])), "\n")
  cat("  Unique values:", length(unique(pred_probs[[model]])), "\n")
}

# 1. ROC Curve
roc_list <- list()
auc_values <- numeric(length(model_names))
valid_models <- c()
valid_colors <- c()

for (i in seq_along(model_names)) {
  tryCatch({
    roc_obj <- roc(actual_binary, pred_probs[[model_names[i]]], quiet = TRUE)
    roc_list[[model_names[i]]] <- roc_obj
    auc_values[i] <- auc(roc_obj)
    valid_models <- c(valid_models, model_names[i])
    valid_colors <- c(valid_colors, c("CatBoost" = "red", "KNN" = "blue", "Logistic Regression" = "green", 
                                      "Random Forest" = "purple", "XGBoost" = "orange", "Decision Tree" = "black")[model_names[i]])
  }, error = function(e) {
    cat("Warning: ROC computation failed for", model_names[i], ":", e$message, "\n")
  })
}

# Check if we have any valid ROC curves
if (length(roc_list) == 0) {
  stop("No valid ROC curves could be computed. Check your predicted probabilities and actual labels.")
}

# Create ROC data frame
roc_df <- data.frame(FPR = numeric(), TPR = numeric(), Model = character())
for (model in valid_models) {
  roc_obj <- roc_list[[model]]
  roc_df <- rbind(roc_df, data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    Model = model
  ))
}

roc_plot <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "ROC Curves for All Models",
       x = "False Positive Rate (FPR)",
       y = "True Positive Rate (TPR)",
       color = "Model") +
  theme_minimal() +
  scale_color_manual(values = valid_colors)

# Add AUC annotations
y_pos <- 0.2
for (i in seq_along(valid_models)) {
  roc_plot <- roc_plot + annotate("text", x = 0.6, y = y_pos, 
                                  label = paste("AUC (", valid_models[i], "):", round(auc_values[i], 3)), 
                                  color = valid_colors[i])
  y_pos <- y_pos - 0.05
}

# Save ROC plot to PNG
ggsave("roc_plot.png", roc_plot, width = 8, height = 6)

# 2. Precision-Recall Curve
pr_list <- list()
valid_models_pr <- c()
valid_colors_pr <- c()

for (i in seq_along(model_names)) {
  tryCatch({
    pr_obj <- pr.curve(scores.class0 = pred_probs[[model_names[i]]], weights.class0 = actual_binary, curve = TRUE)
    pr_list[[model_names[i]]] <- pr_obj
    valid_models_pr <- c(valid_models_pr, model_names[i])
    valid_colors_pr <- c(valid_colors_pr, c("CatBoost" = "red", "KNN" = "blue", "Logistic Regression" = "green", 
                                            "Random Forest" = "purple", "XGBoost" = "orange", "Decision Tree" = "black")[model_names[i]])
  }, error = function(e) {
    cat("Warning: Precision-Recall computation failed for", model_names[i], ":", e$message, "\n")
  })
}

# Create Precision-Recall data frame
pr_df <- data.frame(Recall = numeric(), Precision = numeric(), Model = character())
for (model in valid_models_pr) {
  pr_obj <- pr_list[[model]]
  pr_df <- rbind(pr_df, data.frame(
    Recall = pr_obj$curve[, 1],
    Precision = pr_obj$curve[, 2],
    Model = model
  ))
}

pr_plot <- ggplot(pr_df, aes(x = Recall, y = Precision, color = Model)) +
  geom_line(size = 1) +
  labs(title = "Precision-Recall Curves for All Models",
       x = "Recall",
       y = "Precision",
       color = "Model") +
  theme_minimal() +
  scale_color_manual(values = valid_colors_pr)

# Save Precision-Recall plot to PNG
ggsave("precision_recall_plot.png", pr_plot, width = 8, height = 6)

# 3. Residual Plot
# Compute residuals: actual_binary - predicted_probability
residual_df <- data.frame(
  Predicted = numeric(length(actual_binary) * length(model_names)),
  Residual = numeric(length(actual_binary) * length(model_names)),
  Model = character(length(actual_binary) * length(model_names))
)

start_idx <- 1
for (model in model_names) {
  end_idx <- start_idx + length(actual_binary) - 1
  if (is.null(pred_probs[[model]]) || length(pred_probs[[model]]) == 0) {
    stop(paste("Predicted probabilities for", model, "are empty or NULL. Check your model predictions."))
  }
  if (length(pred_probs[[model]]) != length(actual_binary)) {
    stop(paste("Length mismatch for", model, ": Predicted probabilities have length", 
               length(pred_probs[[model]]), "but actual_binary has length", length(actual_binary)))
  }
  residual_df$Predicted[start_idx:end_idx] <- pred_probs[[model]]
  residual_df$Residual[start_idx:end_idx] <- actual_binary - pred_probs[[model]]
  residual_df$Model[start_idx:end_idx] <- model
  start_idx <- end_idx + 1
}

residual_plot <- ggplot(residual_df, aes(x = Predicted, y = Residual, color = Model)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Residual Plot for All Models",
       x = "Predicted Probability (Cardio)",
       y = "Residual (Actual - Predicted)",
       color = "Model") +
  theme_minimal() +
  scale_color_manual(values = c("CatBoost" = "red", "KNN" = "blue", "Logistic Regression" = "green", 
                                "Random Forest" = "purple", "XGBoost" = "orange", "Decision Tree" = "black"))

# Save Residual plot to PNG
ggsave("residual_plot.png", residual_plot, width = 8, height = 6)

# 4. Predicted vs Actual Plot
pred_vs_actual_df <- data.frame(
  Actual = rep(actual_binary, length(model_names)),
  Predicted = numeric(length(actual_binary) * length(model_names)),
  Model = character(length(actual_binary) * length(model_names))
)

start_idx <- 1
for (model in model_names) {
  end_idx <- start_idx + length(actual_binary) - 1
  if (is.null(pred_probs[[model]]) || length(pred_probs[[model]]) == 0) {
    stop(paste("Predicted probabilities for", model, "are empty or NULL. Check your model predictions."))
  }
  if (length(pred_probs[[model]]) != length(actual_binary)) {
    stop(paste("Length mismatch for", model, ": Predicted probabilities have length", 
               length(pred_probs[[model]]), "but actual_binary has length", length(actual_binary)))
  }
  pred_vs_actual_df$Predicted[start_idx:end_idx] <- pred_probs[[model]]
  pred_vs_actual_df$Model[start_idx:end_idx] <- model
  start_idx <- end_idx + 1
}

pred_vs_actual_plot <- ggplot(pred_vs_actual_df, aes(x = Predicted, y = Actual, color = Model)) +
  geom_jitter(alpha = 0.3, width = 0.02, height = 0.02) +
  labs(title = "Predicted vs Actual for All Models",
       x = "Predicted Probability (Cardio)",
       y = "Actual (0 = NoCardio, 1 = Cardio)",
       color = "Model") +
  theme_minimal() +
  scale_color_manual(values = c("CatBoost" = "red", "KNN" = "blue", "Logistic Regression" = "green", 
                                "Random Forest" = "purple", "XGBoost" = "orange", "Decision Tree" = "black"))

# Save Predicted vs Actual plot to PNG
ggsave("predicted_vs_actual_plot.png", pred_vs_actual_plot, width = 8, height = 6)