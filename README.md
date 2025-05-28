README

Credit Card Default Prediction
This project builds and evaluates multiple machine learning models to predict whether a customer will default 
on their credit card payment the following month. 

The dataset used is sourced from the UCI Machine Learning Repository ("Default of Credit Card Clients Dataset").
Available here:
https://www.kaggle.com/datasets/uciml/default-of-credit-card-clients-dataset

Project Structure:
  Data Exploration: Correlation analysis, categorical distributions, box plots, and scatter plots.
  Data Preparation: Cleaning column names, transforming categorical variables, handling missing values and duplicates.
  Feature Engineering: Recoding factors, renaming variables, and standardizing continuous features.
  Outlier Handling: Capping extreme values based on IQR.
  Data Splitting: Stratified sampling into training and test datasets.
  
  Model Building: Training and evaluating five different models:
      Decision Tree
      Random Forest
      Support Vector Machine (SVM)
      Artificial Neural Network (ANN)
      K-Nearest Neighbors (KNN)


  Libraries Used:
  tidyverse, readxl, janitor, skimr, kableExtra, ggplot2, dplyr, reshape2, corrplot
  Modeling: caret, e1071, rpart, randomForest, neuralnet

Exploratory Data Analysis
  Correlation matrix for numerical predictors
  Distribution visualizations for SEX, EDUCATION, and MARRIAGE vs default outcome
  Credit amount distribution and scatter plots colored by demographic factors

Data Processing
  Dropped non-informative ID column
  Cleaned column names and recoded categorical variables
  Normalized bill_amt* and pay_amt* features
  Handled outliers with IQR-based capping

Model Evaluation Metrics:
Accuracy
Precision
Recall
F1 Score
Confusion Matrix

Models & Techniques
Decision Tree:
  Built with rpart
  Visualized and evaluated with confusion matrix
  
Random Forest:
  Built with randomForest
  Variable importance plotted

SVM:
Radial kernel used
Gamma and cost tuned
Visualized key feature relationships

Artificial Neural Network (ANN):
  Built with neuralnet
  2 hidden nodes and logistic activation

K-Nearest Neighbors (KNN):
  Tuned with cross-validation
  Standardized input features

Sample Output
Each model outputs predicted values and evaluation metrics printed to the console, including comparisons between 
predicted and actual classes.

Dataset Info
Source: UCI Machine Learning Repository
Records: 30,000 clients
Target Variable: default_payment_next_month (0 = no, 1 = yes)
Features: Demographic, credit, and historical repayment behavior

Reproducibility
Random seed set to ensure consistent stratified sampling.
Model reproducibility ensured with consistent pre-processing.

Future Work (To-do)
Hyperparameter tuning for all models
Incorporating feature selection
Deployment with Shiny for interactive exploration
Recreation of all models graphed out in d3.js/tableau/powerBI
