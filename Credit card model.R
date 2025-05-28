library(tidyverse)    # For data manipulation and visualisation
library(readxl)       # For reading Excel files
library(janitor)      # For cleaning data
library(skimr)        # For summarising data
library(kableExtra)   # For creating well-formatted tables
library(ggplot2)      # For creating graphics
library(dplyr)        # For data manipulation
library(reshape2)     # For data manipulation
library(corrplot)     # For creating correlation matrix
library(caret)        # For training and plotting classification and regression models
library(e1071)        # For SVM modelling
library(rpart)        # For recursive partition
library(rpart.plot)   # For plotting


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
corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.cex = 0.6, number.cex = 0.7)

# Category-wise Distribution for categorical variables

# Gender Distribution by Result
ggplot(cc_defaults, aes(x = SEX, fill = as.factor(`default payment next month`))) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution by Result", x = "Gender", fill = "Result") +
  theme_minimal()

# Education Distribution by Result
ggplot(cc_defaults, aes(x = EDUCATION, fill = as.factor(`default payment next month`))) +
  geom_bar(position = "dodge") +
  labs(title = "Education Distribution by Result", x = "Education", fill = "Result") +
  theme_minimal()

# Marriage Distribution by Result
ggplot(cc_defaults, aes(x = MARRIAGE, fill = as.factor(`default payment next month`))) +
  geom_bar(position = "dodge") +
  labs(title = "Marriage Distribution by Result", x = "Marriage", fill = "Result") +
  theme_minimal()

# Box Plots for Continuous Variables

# Amount of Given Credit by Result
ggplot(cc_defaults, aes(x = as.factor(`default payment next month`), y = LIMIT_BAL)) +
  geom_boxplot() +
  labs(title = "Amount of Given Credit by Result", x = "Result", y = "Amount of Given Credit") +
  theme_minimal()


# Factor variables could be used to segment dataset using colors
# Scatter plot of Age vs. Amount of Given Credit, colored by Gender
ggplot(cc_defaults, aes(x = AGE, y = LIMIT_BAL, color = SEX)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Amount of Given Credit Colored by Gender", x = "Age", y = "Amount of Given Credit") +
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
  mutate(education = fct_recode(education, graduate = "1", university = "2", high_school = "3", others = "4", unknown = "0", unknown = "5", unknown = "6"))

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
  mutate(marriage = fct_recode(marriage, unknown = "0", unknown = "3", married = "1", single = "2"))

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
################ Data Normalisation  ####################
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


# Function to cap outliers at the 1st and 99th percentiles
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

cc_default_unique[columns_to_cap] <- lapply(cc_default_unique[columns_to_cap], cap_outliers)


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
indexes <- createDataPartition(cc_default_unique$default,times = 1,p = 0.7,list = FALSE)

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

# Build the decision tree model
decision_tree_model <- rpart(default ~ ., data = cc_default_unique.test[,6:24], method = "class")

# Make predictions on the test dataset
predictions5 <- predict(decision_tree_model, cc_default_unique.test[,6:24], type = "class")
print(predictions5)

# Evaluate the model performance
conf_matrix <- confusionMatrix(predictions5, cc_default_unique.test$default)
print(conf_matrix)

# Extract and print performance metrics
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Recall']
F1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", F1_score, "\n")

#########################################################

# Model 2: Random Forest

library(randomForest)
cc_default_unique.rf.model <- randomForest(default ~ ., data=cc_default_unique.train[,6:24], importance=TRUE)
print(cc_default_unique.rf.model)

importance(cc_default_unique.rf.model)
varImpPlot(cc_default_unique.rf.model)

#building the model
#Random Forest Model
RF<- randomForest( x= cc_default_unique.train[,6:23], y=cc_default_unique.train[,6:24]$default, data=cc_default_unique.train , ntree=5)
print(RF)
summary(RF)


#predictions 1
predictions <- predict(RF, data =cc_default_unique.test)
print(predictions)

predictions <- predict(RF, newdata = cc_default_unique.test)  
actuals <- cc_default_unique.test$default 
# Create the confusion matrix
#Evaluation 1(will remain same for all models just change the numbers in front with the models respectively )
conf_matrix1 <- confusionMatrix(table(predictions, actuals))
precision1 <- conf_matrix1$byClass["Precision"]
recall1 <- conf_matrix1$byClass["Recall"]
accuracy1 <- conf_matrix1$overall["Accuracy"]
F1_score1 <- 2*recall1*precision1/(recall1+precision1)

print(paste("Precision:", precision1))
print(paste("Recall:", recall1))
print(paste("Accuracy:", accuracy1))
print(paste("F1_score1:", F1_score1))

#########################################################

# Model 3: SVM

svm_model <- svm(default ~ ., data = cc_default_unique.train[,6:24], 
                 kernel = "radial", 
                 cost = 10, #
                 gamma = 0.1) #

#Print summary of trained model
print(svm_model)
summary(svm_model)

# Make predictions on the test dataset
predictions_svm <- predict(svm_model, newdata = cc_default_unique.test[,6:24])
print(predictions_svm)

#Evaluating Model
conf_matrix_svm <- confusionMatrix(table(predictions_svm, actuals))
precision_svm <- conf_matrix_svm$byClass["Precision"]
recall_svm <- conf_matrix_svm$byClass["Recall"]
accuracy_svm <- conf_matrix_svm$overall["Accuracy"]
F1_score_svm <- 2 * recall_svm * precision_svm / (recall_svm + precision_svm)

#Printing SVM evaluation metrics
print(paste("Precision:", precision_svm))
print(paste("Recall:", recall_svm))
print(paste("Accuracy:", accuracy_svm))
print(paste("F1 Score:", F1_score_svm))

#plot model
#Subsequent months affect liklihood of defaulting.
plot(svm_model, data = cc_default_unique.train, formula = pay_apr ~ pay_may)
plot(svm_model, data = cc_default_unique.train, formula = pay_may ~ pay_jun) 
plot(svm_model, data = cc_default_unique.train, formula = pay_jun ~ pay_jul)
plot(svm_model, data = cc_default_unique.train, formula = pay_jul ~ pay_aug)
plot(svm_model, data = cc_default_unique.train, formula = pay_aug ~ pay_sep)

#########################################################

# Model 4: ANN

library(neuralnet)
nn_model<-neuralnet(default ~.,data=cc_default_unique.train[,6:24], hidden=2,act.fct = "logistic",
                    linear.output = FALSE)
#print the summary of the trained model
print(nn_model)
summary(nn_model)
plot(nn_model)

#Prediction 2
predictions2 <- predict(nn_model, newdata = cc_default_unique.test[,6:24], type = "response")
print(predictions2)
#extract the second column for  the probability of the positive class

predicted_probabilites <- predictions2[,2]
predicted_class <- ifelse(predicted_probabilites >= 0.5, 1, 0)
#In console we checked table(predicted_class) and table(cc_default_unique.test$default)
#converting yes and no to "0" and "1" 
cc_default_unique.test$default <- ifelse(cc_default_unique.test$default =="yes",1,0)

# Ensure the lengths match
#print("Length of predicted class:")
#print(length(predicted_class))
#print("Length of test set default column:")
#print(length(cc_default_unique.test$default))

# Ensure the lengths match
length(predicted_class) == length(cc_default_unique.test$default)
#evaluation 2
library(caret)
#conf_matrix_svm <- confusionMatrix(table(predictions_svm, cc_default_unique.test$default))
(table(predicted_class, cc_default_unique.test$default))
conf_matrix2 <- confusionMatrix(table(predicted_class, cc_default_unique.test$default))
precision2 <- conf_matrix2$byClass["Precision"]
recall2 <- conf_matrix2$byClass["Recall"]
accuracy2 <- conf_matrix2$overall["Accuracy"]
F1_score2 <- 2*recall2*precision2/(recall2+precision2)

print(paste("Precision:", precision2))
print(paste("Recall:", recall2))
print(paste("Accuracy:", accuracy2))
print(paste("F1_score1:", F1_score2))

#########################################################

# Model 5: KNN

# Ensure that the default column is a factor with the same levels in both train and test datasets
cc_default_unique.train$default <- factor(cc_default_unique.train$default)
cc_default_unique.test$default <- factor(cc_default_unique.test$default, levels = levels(cc_default_unique.train$default))

# Build the KNN model
knn_model <- train(
  default ~.,
  data =cc_default_unique.train[,6:24] ,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),  # 5-fold cross-validation
  preProcess = c("center", "scale"),  # Standardize predictors
  tuneGrid = expand.grid(k = 1:10)  # Test k values from 1 to 10
)
# Print the summary of the trained model
print(knn_model)
summary(knn_model)
plot (knn_model)

#Prediction 3
predictions3 <- predict(knn_model, newdata = cc_default_unique.test)
actuals <- cc_default_unique.test$default
print(predictions3)

#evaluation 3
conf_matrix3 <- confusionMatrix(table(predictions3, actuals))
precision3 <- conf_matrix3$byClass["Precision"]
recall3 <- conf_matrix3$byClass["Recall"]
accuracy3 <- conf_matrix3$overall["Accuracy"]
F1_score3 <- 2*recall3*precision3/(recall3+precision3)

print(paste("Precision:", precision3))
print(paste("Recall:", recall3))
print(paste("Accuracy:", accuracy3))
print(paste("F1_score1:", F1_score3))


