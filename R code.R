library(tidyverse) # For data manipulation and visualisation
library(readxl) # For reading Excel files
library(janitor) # For cleaning data
library(skimr) # For summarising data
library(kableExtra) # For creating well-formatted tables
library(ggplot2) # For creating graphics
library(dplyr) # For data manipulation
library(reshape2) # For data manipulation
library(corrplot) # For creating correlation matrix
library(caret) # For training and plotting classification and regression
models
library(e1071) # For SVM modelling
library(rpart) # For recursive partioning
library(rpart.plot) # For plotting
# Read imported excel file, skips 1st row
read_file <- read_excel("default of credit card clients.xls", skip = 1)
# Save dataframe as RDS object
saveRDS(read_file, file = "read_file.RDS")
# Load dataset
cc_defaults <- readRDS("read_file.RDS")
# Summarise dataset
skim(cc_defaults)
#########################################################
################## Data Exploration #####################
#########################################################
# Explore relationships between predictor/predicted columns
# Correlation Analysis for numerical variables
numerical_vars <- cc_defaults %>% select_if(is.numeric)
correlation_matrix <- cor(numerical_vars)
corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.cex = 0.6,
number.cex = 0.7)
# Category-wise Distribution for categorical variables
# Gender Distribution by Result
ggplot(cc_defaults, aes(x = SEX, fill = as.factor(`default payment next month`))) +
 geom_bar(position = "dodge") +
 labs(title = "Gender Distribution by Result", x = "Gender", fill = "Result") +
 theme_minimal()
# Education Distribution by Result
ggplot(cc_defaults, aes(x = EDUCATION, fill = as.factor(`default payment next
month`))) +
 geom_bar(position = "dodge") +
 labs(title = "Education Distribution by Result", x = "Education", fill =
"Result") +
 theme_minimal()
# Marriage Distribution by Result
ggplot(cc_defaults, aes(x = MARRIAGE, fill = as.factor(`default payment next
month`))) +
 geom_bar(position = "dodge") +
 labs(title = "Marriage Distribution by Result", x = "Marriage", fill = "Result")
+
 theme_minimal()
# Box Plots for Continuous Variables
# Amount of Given Credit by Result
ggplot(cc_defaults, aes(x = as.factor(`default payment next month`), y =
LIMIT_BAL)) +
 geom_boxplot() +
 labs(title = "Amount of Given Credit by Result", x = "Result", y = "Amount of
Given Credit") +
 theme_minimal()
# Factor variables could be used to segment dataset using colors
# Scatter plot of Age vs. Amount of Given Credit, colored by Gender
ggplot(cc_defaults, aes(x = AGE, y = LIMIT_BAL, color = SEX)) +
 geom_point() +
 labs(title = "Scatter Plot of Age vs. Amount of Given Credit Colored by Gender",
x = "Age", y = "Amount of Given Credit") +
 theme_minimal()
#########################################################
################# Data Preparation ######################
#########################################################
# Drop ID column, only used for indexing
cc_defaults <- cc_defaults%>%
 select(-ID)
# Check for missing values
missing_values <- sapply(cc_defaults, function(x) sum(is.na(x)))
print(missing_values)
# Check for duplicates, using all columns as a composite key
duplicates <- cc_defaults %>% janitor::get_dupes()
print(duplicates)
# Remove duplicates, keeps 1st unique row
cc_default_unique <- cc_defaults %>%
 distinct(.keep_all = TRUE)
# Show results
print(cc_default_unique)
# Original column names
colnames(cc_default_unique)
# Clean column names
cc_default_unique <- clean_names(cc_default_unique)
# Load cleaned column names
colnames(cc_default_unique)
#########################################################
############### Data Transformation #####################
#########################################################
# Transform 'sex/gender' column
cc_default_unique <- cc_default_unique |>
 mutate(sex = factor(sex)) |>
 mutate(sex = fct_recode(sex, male = "1", female = "2"))
# Count values in education column
cc_default_unique|>
 tabyl(education) |>
 adorn_pct_formatting() |>
 kbl() |>
 kable_styling(full_width = FALSE)
# Transform 'education' column
# Missing value definitions for '0', '5', and '6'
cc_default_unique <- cc_default_unique |>
 mutate(education = factor(education)) |>
 mutate(education = fct_recode(education, graduate = "1", university = "2",
high_school = "3", others = "4", unknown = "0", unknown = "5", unknown = "6"))
# New 'education' counting levels
cc_default_unique |>
 tabyl(education) |>
 adorn_pct_formatting() |>
 kbl() |>
 kable_styling(full_width = FALSE)
# Count marriage column
cc_default_unique |>
 tabyl(marriage) |>
 adorn_pct_formatting() |>
 kbl() |>
 kable_styling(full_width = FALSE)
# Transform marriage
# Missing value definitions for '0' and '3'
cc_default_unique <- cc_default_unique |>
 mutate(marriage = factor(marriage)) |>
 mutate(marriage = fct_recode(marriage, unknown = "0", unknown = "3", married =
"1", single = "2"))
# Count new marriage columns
cc_default_unique |>
 tabyl(marriage) |>
 adorn_pct_formatting() |>
 kbl() |>
 kable_styling(full_width = FALSE)
# Transform 'default payment next month' into yes/no
cc_default_unique <- cc_default_unique |>
 rename("default" = default_payment_next_month) |>
 mutate(default = factor(default)) |>
 mutate(default = fct_relevel(default, "1")) |>
 mutate(default = fct_recode(default, yes = "1", "no" = "0"))
# Count 'default payment next month'
cc_default_unique |>
 tabyl(default) |>
 adorn_pct_formatting() |>
 kbl() |>
 kable_styling(full_width = FALSE)
# Rename all entries
cc_default_unique <- cc_default_unique |>
 rename("pay_sep" = pay_0,
 "pay_aug" = pay_2,
 "pay_jul" = pay_3,
 "pay_jun" = pay_4,
 "pay_may" = pay_5,
 "pay_apr" = pay_6,
 "bill_amt_sep" = bill_amt1,
 "bill_amt_aug" = bill_amt2,
 "bill_amt_jul" = bill_amt3,
 "bill_amt_jun" = bill_amt4,
 "bill_amt_may" = bill_amt5,
 "bill_amt_apr" = bill_amt6,
 "pay_amt_sep" = pay_amt1,
 "pay_amt_aug" = pay_amt2,
 "pay_amt_jul" = pay_amt3,
 "pay_amt_jun" = pay_amt4,
 "pay_amt_may" = pay_amt5,
 "pay_amt_apr" = pay_amt6)
# Count 'pay_sep', change tabyl function to view other columns
cc_default_unique |>
 tabyl(pay_sep) |>
 adorn_pct_formatting() |>
 kbl() |>
 kable_styling(full_width = FALSE)
#########################################################
################ Data Normalisation ####################
#########################################################
normalise <- function(x) {
 return((x - mean(x)) / sd(x))
}
cc_default_unique$bill_amt_sep <- normalise(cc_default_unique$bill_amt_sep)
cc_default_unique$bill_amt_aug <- normalise(cc_default_unique$bill_amt_aug)
cc_default_unique$bill_amt_jul <- normalise(cc_default_unique$bill_amt_jul)
cc_default_unique$bill_amt_jun <- normalise(cc_default_unique$bill_amt_jun)
cc_default_unique$bill_amt_may <- normalise(cc_default_unique$bill_amt_may)
cc_default_unique$bill_amt_apr <- normalise(cc_default_unique$bill_amt_apr)
cc_default_unique$pay_amt_sep <- normalise(cc_default_unique$pay_amt_sep)
cc_default_unique$pay_amt_aug <- normalise(cc_default_unique$pay_amt_aug)
cc_default_unique$pay_amt_jul <- normalise(cc_default_unique$pay_amt_jul)
cc_default_unique$pay_amt_jun <- normalise(cc_default_unique$pay_amt_jun)
cc_default_unique$pay_amt_may <- normalise(cc_default_unique$pay_amt_may)
cc_default_unique$pay_amt_apr <- normalise(cc_default_unique$pay_amt_apr)
############################################################
################### Outlier Handling #######################
############################################################
# Generate boxplots before capping for bill amounts
par(mfrow = c(2, 3))
boxplot(cc_default_unique$bill_amt_sep, main = "Bill Amount Sep - Before Capping")
boxplot(cc_default_unique$bill_amt_aug, main = "Bill Amount Aug - Before Capping")
boxplot(cc_default_unique$bill_amt_jul, main = "Bill Amount Jul - Before Capping")
boxplot(cc_default_unique$bill_amt_jun, main = "Bill Amount Jun - Before Capping")
boxplot(cc_default_unique$bill_amt_may, main = "Bill Amount May - Before Capping")
boxplot(cc_default_unique$bill_amt_apr, main = "Bill Amount Apr - Before Capping")
# Generate boxplots before capping for pay amounts
par(mfrow = c(2, 3))
boxplot(cc_default_unique$pay_amt_sep, main = "Pay Amount Sep - Before Capping")
boxplot(cc_default_unique$pay_amt_aug, main = "Pay Amount Aug - Before Capping")
boxplot(cc_default_unique$pay_amt_jul, main = "Pay Amount Jul - Before Capping")
boxplot(cc_default_unique$pay_amt_jun, main = "Pay Amount Jun - Before Capping")
boxplot(cc_default_unique$pay_amt_may, main = "Pay Amount May - Before Capping")
boxplot(cc_default_unique$pay_amt_apr, main = "Pay Amount Apr - Before Capping")
# Function to cap outliers
cap_outliers <- function(x) {
 Q1 <- quantile(x, 0.25, na.rm = TRUE)
 Q3 <- quantile(x, 0.75, na.rm = TRUE)
 IQR <- Q3 - Q1
 lower_bound <- Q1 - 1.5 * IQR
 upper_bound <- Q3 + 1.5 * IQR
 x <- ifelse(x < lower_bound, lower_bound, x)
 x <- ifelse(x > upper_bound, upper_bound, x)
 return(x)
}
# Apply capping function to the relevant columns
columns_to_cap <- c("bill_amt_sep", "bill_amt_aug", "bill_amt_jul",
 "bill_amt_jun", "bill_amt_may", "bill_amt_apr",
 "pay_amt_sep", "pay_amt_aug", "pay_amt_jul",
 "pay_amt_jun", "pay_amt_may", "pay_amt_apr")
cc_default_unique[columns_to_cap] <- lapply(cc_default_unique[columns_to_cap],
cap_outliers)
# Generate boxplots after capping and normalisation for bill amounts
par(mfrow = c(2, 3))
boxplot(cc_default_unique$bill_amt_sep, main = "Bill Amount Sep - After Capping")
boxplot(cc_default_unique$bill_amt_aug, main = "Bill Amount Aug - After Capping")
boxplot(cc_default_unique$bill_amt_jul, main = "Bill Amount Jul - After Capping")
boxplot(cc_default_unique$bill_amt_jun, main = "Bill Amount Jun - After Capping")
boxplot(cc_default_unique$bill_amt_may, main = "Bill Amount May - After Capping")
boxplot(cc_default_unique$bill_amt_apr, main = "Bill Amount Apr - After Capping")
# Generate boxplots after capping and normalisation for pay amounts
par(mfrow = c(2, 3))
boxplot(cc_default_unique$pay_amt_sep, main = "Pay Amount Sep - After Capping")
boxplot(cc_default_unique$pay_amt_aug, main = "Pay Amount Aug - After Capping")
boxplot(cc_default_unique$pay_amt_jul, main = "Pay Amount Jul - After Capping")
boxplot(cc_default_unique$pay_amt_jun, main = "Pay Amount Jun - After Capping")
boxplot(cc_default_unique$pay_amt_may, main = "Pay Amount May - After Capping")
boxplot(cc_default_unique$pay_amt_apr, main = "Pay Amount Apr - After Capping")
#########################################################
################### Data Sampling #######################
#########################################################
# Perform Stratified Sampling
# Set a random seed for reproducibility
set.seed(1232)
# Create a stratified partition (70% training, 30% testing)
indexes <- createDataPartition(cc_default_unique$default,times = 1,p = 0.7,list =
FALSE)
# Split the dataset into training and testing sets
cc_default_unique.train <- cc_default_unique[indexes,]
cc_default_unique.test <- cc_default_unique[-indexes,]
# Verify the split
cat("Training Set Size:", nrow(cc_default_unique.train), "\n")
cat("Test Set Size:", nrow(cc_default_unique.test), "\n")
# Check the distribution of the target variable in both sets
prop.table(table(cc_default_unique.train$default))
prop.table(table(cc_default_unique.test$default))
########################################################################
################## Model Building and Evaluation #######################
########################################################################
#########################################################
# Model 1: Decision Tree
library(rpart)
library(caret)
# Build the decision tree model with cross-validation for hyperparameter tuning
train_control <- trainControl(method = "cv", number = 5)
decision_tree_grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))
# Train the decision tree model
decision_tree_model <- train(
 default ~ .,
 data = cc_default_unique.train[, 6:24],
 method = "rpart",
 trControl = train_control,
 tuneGrid = decision_tree_grid
)
# Print the best model
print(decision_tree_model$bestTune)
print(decision_tree_model)
# Make predictions on the test dataset
predictions5 <- predict(decision_tree_model, newdata = cc_default_unique.test[,
6:24])
print(predictions5)
# Evaluate the model performance
conf_matrix <- confusionMatrix(predictions5, cc_default_unique.test$default)
print(conf_matrix)
# Extract and print performance metrics
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
F1_score <- 2 * (precision * recall) / (precision + recall)
cat("Decision Tree Model Performance Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", F1_score, "\n")
#########################################################
# Model 2: Random Forest
library(doParallel)
library(randomForest)
library(caret)
# Set up parallel processing
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)
# Define train control for cross-validation
train_control <- trainControl(method = "cv", number = 5)
# Define a grid of hyperparameters to tune
tune_grid <- expand.grid(mtry = seq(2, 10, by = 2))
# Train the Random Forest model with cross-validation
set.seed(123) # For reproducibility
random_forest_model <- train(
 default ~ .,
 data = cc_default_unique.train[, 6:24],
 method = "rf",
 trControl = train_control,
 tuneGrid = tune_grid,
 ntree = 500,
 importance = TRUE
)
# Stop parallel processing
stopCluster(cl)
# Print the best model
print(random_forest_model$bestTune)
print(random_forest_model)
# Assess feature importance
importance <- varImp(random_forest_model)
print(importance)
plot(importance)
# Make predictions on the test dataset
rf_predictions <- predict(random_forest_model, cc_default_unique.test)
# Evaluate the model performance
rf_conf_matrix <- confusionMatrix(rf_predictions, cc_default_unique.test$default)
print(rf_conf_matrix)
# Extract and print performance metrics
rf_accuracy <- rf_conf_matrix$overall['Accuracy']
rf_precision <- rf_conf_matrix$byClass['Pos Pred Value']
rf_recall <- rf_conf_matrix$byClass['Sensitivity']
rf_F1_score <- 2 * (rf_precision * rf_recall) / (rf_precision + rf_recall)
cat("Random Forest Model Performance Metrics:\n")
cat("Accuracy:", rf_accuracy, "\n")
cat("Precision:", rf_precision, "\n")
cat("Recall:", rf_recall, "\n")
cat("F1 Score:", rf_F1_score, "\n")
#########################################################
# Model 3: SVM
library(e1071)
library(caret)
# Build the SVM model
svm_model <- svm(default ~ ., data = cc_default_unique.train[, 6:24],
 kernel = "radial",
 cost = 10,
 gamma = 0.1)
# Print summary of trained model
print(svm_model)
summary(svm_model)
# Make predictions on the test dataset
predictions_svm <- predict(svm_model, newdata = cc_default_unique.test[, 6:24])
# Evaluate the model performance
conf_matrix_svm <- confusionMatrix(predictions_svm, cc_default_unique.test$default)
print(conf_matrix_svm)
# Extract and print performance metrics
svm_accuracy <- conf_matrix_svm$overall['Accuracy']
svm_precision <- conf_matrix_svm$byClass['Pos Pred Value']
svm_recall <- conf_matrix_svm$byClass['Sensitivity']
svm_F1_score <- 2 * (svm_precision * svm_recall) / (svm_precision + svm_recall)
cat("SVM Model Performance Metrics:\n")
cat("Accuracy:", svm_accuracy, "\n")
cat("Precision:", svm_precision, "\n")
cat("Recall:", svm_recall, "\n")
cat("F1 Score:", svm_F1_score, "\n")
#########################################################
# Model 4: ANN
library(neuralnet)
library(caret)
# Build the ANN model
ann_model <- neuralnet(default ~ ., data = cc_default_unique.train[, 6:24], hidden
= 2, act.fct = "logistic", linear.output = FALSE)
# Print the summary of the trained model
print(ann_model)
summary(ann_model)
# Prediction 1
predicted_probabilites <- predictions2[,2]
predicted_class <- ifelse(predicted_probabilites >= 0.5, 1, 0)
# Prediction 2
predictions2 <- predict(nn_model, newdata = cc_default_unique.test[,6:24], type =
"response")
print(predictions2)
# Extract the second column for the probability of the positive class
predicted_probabilites <- predictions2[,2]
predicted_class <- ifelse(predicted_probabilites >= 0.5, 1, 0)
# Convert 'yes'/'no' to 1/0
cc_default_unique.test$default <- ifelse(cc_default_unique.test$default
=="yes",1,0)
# Ensure the lengths match
length(predicted_class) == length(cc_default_unique.test$default)
# Create a contingency table
conf_table_ann <- table(predicted_class, cc_default_unique.test$default)
# Evaluate the model performance
conf_matrix_ann <- confusionMatrix(conf_table_ann)
print(conf_matrix_ann)
# Extract and print performance metrics
accuracy_ann <- conf_matrix_ann$overall['Accuracy']
precision_ann <- conf_matrix_ann$byClass['Pos Pred Value']
recall_ann <- conf_matrix_ann$byClass['Sensitivity']
F1_score_ann <- 2 * (precision_ann * recall_ann) / (precision_ann + recall_ann)
cat("ANN Model Performance Metrics:\n")
cat("Accuracy:", accuracy_ann, "\n")
cat("Precision:", precision_ann, "\n")
cat("Recall:", recall_ann, "\n")
cat("F1 Score:", F1_score_ann, "\n")
#########################################################
# Model 5: KNN
# Ensure that the default column is a factor with the same levels in both train and
test datasets
cc_default_unique.train$default <- factor(cc_default_unique.train$default)
cc_default_unique.test$default <- factor(cc_default_unique.test$default, levels =
levels(cc_default_unique.train$default))
# Build the KNN model
knn_model <- train(
 default ~.,
 data =cc_default_unique.train[,6:24] ,
 method = "knn",
 trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
 preProcess = c("center", "scale"), # Standardize predictors
 tuneGrid = expand.grid(k = 1:10) # Test k values from 1 to 10
)
# Print the summary of the trained model
print(knn_model)
summary(knn_model)
# Make predictions on the test dataset
predictions3 <- predict(knn_model, newdata = cc_default_unique.test)
actuals <- cc_default_unique.test$default
# Evaluate the model performance
conf_matrix_knn <- confusionMatrix(table(predictions3, actuals))
print(conf_matrix_knn)
# Extract and print performance metrics
precision_knn <- conf_matrix_knn$byClass["Precision"]
recall_knn <- conf_matrix_knn$byClass["Recall"]
accuracy_knn <- conf_matrix_knn$overall["Accuracy"]
F1_score_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)
cat("KNN Model Performance Metrics:\n")
cat("Accuracy:", accuracy_knn, "\n")
cat("Precision:", precision_knn, "\n")
cat("Recall:", recall_knn, "\n")
cat("F1 Score:", F1_score_knn, "\n")
