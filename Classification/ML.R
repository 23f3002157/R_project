# Installing required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
if (!require("class")) install.packages("class")
if (!require("ROSE")) install.packages("ROSE")
if (!require("glmnet")) install.packages("glmnet")
if (!require("pROC")) install.packages("pROC")

# Loading the libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(class)
library(ROSE)
library(glmnet)
library(pROC)

# Set seed for reproducibility
set.seed(123)

# Importing the dataset
data <- read.csv("Documents/RIT/R_Project/cardio_data_processed.csv")

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