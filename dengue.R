# Dengue Dataset
# Author: Jennifer Cheng
# Created on Nov 21, 2024



# Install all needed libraries if it is not present

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(randomForest)) install.packages("randomForest")
if (!require("xgboost")) install.packages("xgboost")
if (!require("lightgbm")) install.packages("lightgbm")


# Loading all needed libraries

library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(gridExtra)
library(ggcorrplot)


## Loading the dataset

dengue_labels_train <- read.csv("dengue_labels_train.csv")
dengue_features_train <- read.csv("dengue_features_train.csv")

# Check dimensions

dengue_summary <- data.frame("Length" = nrow(dengue_labels_train), "Columns" = ncol(dengue_labels_train))
print(dengue_summary)

feature_summary <- data.frame("Length" = nrow(dengue_features_train), "Columns" = ncol(dengue_features_train))
print(feature_summary)

head(dengue_labels_train)

head(dengue_features_train)


# PLOT

# Group by year and aggregate total cases
year_max_cases <- dengue_labels_train %>%
  group_by(year) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE))

# Create the bar plot

ggplot(year_max_cases, aes(x = as.factor(year), y = total_cases)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(title = "Total Dengue Cases by Year", x = "Year", y = "Total Cases") +
  scale_y_continuous(breaks = seq(0, 7500, by = 500)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 2))


# Group by city and aggregate total cases
city_affected <- dengue_labels_train %>%
  group_by(city) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE))

# Create the bar plot

ggplot(city_affected, aes(x = city, y = total_cases)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Total Cases of Dengue by City", x = "City", y = "Total Cases of Dengue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  # Center the title

str(dengue_labels_train)
str(dengue_features_train)

# PRE-PROCESSING

# Missing values by column
missing_values_by_column <- dengue_features_train %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "MissingCount") %>%
  filter(MissingCount > 0)

print("Missing values by column:")
print(missing_values_by_column)

# Rows with missing values
rows_with_missing <- dengue_features_train %>%
  filter(if_any(everything(), is.na)) %>%
  summarise(Count = n())

print("Number of rows with at least one missing value:")
print(rows_with_missing)

# Fill the missing values with mean
dengue_features_train <- dengue_features_train %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# This shows that the missing values are resolved
print(dengue_features_train)
print(missing_values_by_column)


# Temperature
# Finding columns with "_temp_k"
temperature_kelvin_columns <- grep("_temp_k", names(dengue_features_train), value = TRUE)
print(temperature_kelvin_columns)

# Convert Kelvin to Celsius for the identified columns
dengue_features_train[temperature_kelvin_columns] <- dengue_features_train[temperature_kelvin_columns] - 273.15

# Rename columns by replacing '_temp_k' with '_temp_c'
names(dengue_features_train) <- gsub("_temp_k", "_temp_c", names(dengue_features_train))

# View updated column names
print(names(dengue_features_train))



# Round all numerical columns to 3 decimal places
dengue_features_train <- dengue_features_train %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Display the first few rows of the dataset
head(dengue_features_train)
dim(dengue_features_train)

#Average Station Max and Min
# Create a new column 'avg_station_max_min'
dengue_features_train <- dengue_features_train %>%
  mutate(avg_station_max_min = (station_max_temp_c + station_min_temp_c) / 2)

# Display the first few values of the new column
head(dengue_features_train$avg_station_max_min)

# Average Analysis Max and Min 
# Create a new column 'avg_analysis_max_min'
dengue_features_train <- dengue_features_train %>%
  mutate(avg_analysis_max_min = (reanalysis_max_air_temp_c + reanalysis_min_air_temp_c) / 2)

# Display the first few values of the new column
head(dengue_features_train$avg_analysis_max_min)



# Add the 'total_cases' column from dengue_labels to dengue_features
dengue_features_train$total_cases <- dengue_labels_train$total_cases

# View the first few rows to verify
head(dengue_features_train$total_cases)

summary(dengue_features_train)




# CORRELATION MATRIX

# Select only numerical columns
numeric_columns <- dengue_features_train %>% 
  select(where(is.numeric))

# Create the correlation matrix
correlation_matrix <- cor(numeric_columns, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)


# Plot the correlation matrix
ggcorrplot(correlation_matrix, 
           method = "square", 
           type = "lower", 
           lab = TRUE,
           tl.cex = 8,
           tl.srt = 45,
           lab_size = 0, # Too many labels block the visual
           colors = c("blue", "white", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), # Tilt x-axis text
        axis.text.y = element_text(size = 8) # Adjust y-axis text size
  )





# EXPLORATORY DATA ANALYSIS

# Boxplot of total_cases by city
ggplot(dengue_features_train, aes(x = city, y = total_cases)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Total Cases by City",
    x = "City",
    y = "Total Cases"
  ) +
  theme_minimal()


#if require install ggplot

# Assuming 'x' is a vector of column names for NDVI features
x <- c("ndvi_ne", "ndvi_nw", "ndvi_se", "ndvi_sw")

# Create individual scatter plots
plot1 <- ggplot(dengue_features_train, aes_string(x = x[1], y = "total_cases")) +
  geom_point(color = "red") +
  labs(title = paste("Total Cases vs", x[1]), x = x[1], y = "Total Cases") +
  theme_minimal()

plot2 <- ggplot(dengue_features_train, aes_string(x = x[2], y = "total_cases")) +
  geom_point(color = "green") +
  labs(title = paste("Total Cases vs", x[2]), x = x[2], y = "Total Cases") +
  theme_minimal()

plot3 <- ggplot(dengue_features_train, aes_string(x = x[3], y = "total_cases")) +
  geom_point(color = "blue") +
  labs(title = paste("Total Cases vs", x[3]), x = x[3], y = "Total Cases") +
  theme_minimal()

plot4 <- ggplot(dengue_features_train, aes_string(x = x[4], y = "total_cases")) +
  geom_point(color = "#B16150") +
  labs(title = paste("Total Cases vs", x[4]), x = x[4], y = "Total Cases") +
  theme_minimal()

# Arrange the plots in a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# total cases vs precipitation 
# Assuming 'x' is a vector containing column names

x <- c("ndvi_ne", "ndvi_nw", "ndvi_se", "ndvi_sw", "precipitation_amt_mm")

# Create a scatter plot
ggplot(dengue_features_train, aes_string(x = x[5], y = "total_cases")) +
  geom_point(color = "orange") +
  labs(title = paste("Total Cases vs", x[5]), 
       x = x[5], 
       y = "Total Cases") +
  theme_minimal() +
  theme(legend.position = "none")



# ANOTHER CORR PLOT but for select features
# Select numerical variables
numeric_vars <- dengue_features_train %>%
  select(ndvi_ne, ndvi_nw, ndvi_se, ndvi_sw, station_max_temp_c, station_min_temp_c, precipitation_amt_mm)

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Visualize the correlation matrix
ggcorrplot(cor_matrix, method = "square", lab = TRUE)



# FEATURE ENGINEERING
# Split the data by city
X_sj <- dengue_features_train[dengue_features_train$city == 'sj', ]
X_iq <- dengue_features_train[dengue_features_train$city == 'iq', ]

# Drop specific columns
X_sj <- X_sj[, !(names(X_sj) %in% c('city', 'weekofyear', 'week_start_date'))]
X_iq <- X_iq[, !(names(X_iq) %in% c('city', 'weekofyear', 'week_start_date'))]

# One-hot encode the 'year' column
one_hot_sj <- model.matrix(~ year - 1, data = X_sj)
X_sj <- X_sj[, !names(X_sj) %in% 'year']
X_sj <- cbind(X_sj, one_hot_sj)

one_hot_iq <- model.matrix(~ year - 1, data = X_iq)
X_iq <- X_iq[, !names(X_iq) %in% 'year']
X_iq <- cbind(X_iq, one_hot_iq)

# View the transformed datasets
head(X_sj)
head(X_iq)


# Select columns related to years (assuming years are the last columns in reverse order)
years <- rev(names(X_sj)[24:length(names(X_sj))])

# Subset and rearrange the dataset
X_sj <- cbind(X_sj[, years], X_sj[, 1:24])

# Display the first few rows
head(X_sj)

glimpse(X_sj)

# Select columns related to years (assuming years are the last columns in reverse order)
years <- rev(names(X_iq)[24:length(names(X_iq))])

# Subset and rearrange the dataset
X_iq <- cbind(X_iq[, years], X_iq[, 1:24])

# Display the first few rows
head(X_iq)
glimpse(X_sj)

# Removing dummy variable trap (dropping specific year columns)
X_sj <- X_sj[, !(names(X_sj) %in% "1990")]
X_iq <- X_iq[, !(names(X_iq) %in% "2000")]

# Convert the data frames to arrays (matrices in R)
x_sj_arr <- as.matrix(X_sj)
x_iq_arr <- as.matrix(X_iq)


# Define train-test split ratio
test_ratio <- 0.2

# Create train-test splits for SJ data
set.seed(23)  # For reproducibility
sj_indices <- createDataPartition(x_sj_arr[, ncol(x_sj_arr)], p = 1 - test_ratio, list = FALSE)

X_sj_train <- x_sj_arr[sj_indices, -ncol(x_sj_arr)]
y_sj_train <- x_sj_arr[sj_indices, ncol(x_sj_arr)]

X_sj_test <- x_sj_arr[-sj_indices, -ncol(x_sj_arr)]
y_sj_test <- x_sj_arr[-sj_indices, ncol(x_sj_arr)]

# Create train-test splits for IQ data
iq_indices <- createDataPartition(x_iq_arr[, ncol(x_iq_arr)], p = 1 - test_ratio, list = FALSE)

X_iq_train <- x_iq_arr[iq_indices, -ncol(x_iq_arr)]
y_iq_train <- x_iq_arr[iq_indices, ncol(x_iq_arr)]

X_iq_test <- x_iq_arr[-iq_indices, -ncol(x_iq_arr)]
y_iq_test <- x_iq_arr[-iq_indices, ncol(x_iq_arr)]



# Feature Scaling for SJ
sj_scaler <- preProcess(X_sj_train, method = c("center", "scale"))

# Apply scaling to training and test sets 
X_sj_train <- predict(sj_scaler, X_sj_train)
X_sj_test <- predict(sj_scaler, X_sj_test)

# Feature Scaling for IQ
iq_scaler <- preProcess(X_iq_train, method = c("center", "scale"))

# Apply scaling to training and test sets
X_iq_train <- predict(iq_scaler, X_iq_train)
X_iq_test <- predict(iq_scaler, X_iq_test)


# Verify normalization
summary(dengue_features_train)


#Scatter plot
# Loop through all features and create individual plots
par(mfrow = c(2, 2))  # Set up a 2x2 grid for multiple plots
for (i in 1:ncol(X_sj_train)) {
  plot(X_sj_train[, i], y_sj_train, pch = 19, col = "green", 
       xlab = paste("Feature", i), ylab = "Target", main = paste("Feature", i, "vs Target"))
}
par(mfrow = c(1, 1))  # Reset plotting layout



# Scale the Data

# Standardize the training data for SJ
sj_scaler <- preProcess(X_sj_train, method = c("center", "scale"))

# Apply scaling to training and test sets
X_sj_train <- predict(sj_scaler, X_sj_train)
X_sj_test <- predict(sj_scaler, X_sj_test)

# Standardize the training data for IQ
iq_scaler <- preProcess(X_iq_train, method = c("center", "scale"))

# Apply scaling to training and test sets
X_iq_train <- predict(iq_scaler, X_iq_train)
X_iq_test <- predict(iq_scaler, X_iq_test)



# Define for Random Forest

# Define training control with cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5
)

# Define the hyperparameter grid
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8),
  splitrule = c("variance"),
  min.node.size = c(1, 5, 10)
)



# Random Forest for SJ

# Train the Random Forest model using grid search with cross-validation
sj_rf <- train(
  x = X_sj_train,
  y = y_sj_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid
)

# Display the best hyperparameters
print(sj_rf$bestTune)

# Predict on the test set
sj_rf_pred <- predict(sj_rf, X_sj_test)

# Calculate Mean Absolute Error (MAE)
mae_sj_rf <- mean(abs(sj_rf_pred - y_sj_test))
cat("MAE:", mae_sj_rf, "\n")




# Random Forest for IQ

# Train the Random Forest model using grid search with cross-validation
iq_rf <- train(
  x = X_iq_train,
  y = y_iq_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid
)

# Display the best hyperparameters
print(iq_rf$bestTune)

# Predict on the test set
iq_rf_pred <- predict(iq_rf, X_iq_test)

# Calculate Mean Absolute Error (MAE)
mae_iq_rf <- mean(abs(iq_rf_pred - y_iq_test))
cat("MAE:", mae_iq_rf, "\n")



# k-NN Model

# Define & Tune Hyper-parameters for k-NN
knn_grid <- expand.grid(
  k = c(3, 5, 7),           # Number of neighbors to try
  algorithm = c("ball_tree", "kd_tree", "brute"),  # Algorithms (manually incorporate logic later if needed)
  leaf_size = c(25)         # Leaf size (applicable only for certain algorithms)
)


# Train the k-NN model with cross-validation for SJ
sj_knn_grid_cv <- train(
  X_sj_train, y_sj_train,
  method = "knn",
  trControl = trainControl(
    method = "cv",  # Cross-validation
    number = 5,     # Number of folds
    summaryFunction = defaultSummary  # Include metrics like MAE
  ),
  # tuneGrid = knn_grid,
  metric = "MAE"  # Optimize for Mean Absolute Error
)

# Extract best score (MAE)
best_score <- min(sj_knn_grid_cv$results$MAE)

# Extract best parameters
best_params <- sj_knn_grid_cv$bestTune

# Print results
print(best_score)
print(best_params)


# Predict on the test set
y_sj_pred <- predict(sj_knn_grid_cv$finalModel, X_sj_test)


# Calculate Mean Absolute Error (MAE)
mae_sj_knn <- mean(abs(y_sj_test - y_sj_pred))

# Print MAE
print(mae_sj_knn)

# Overview of the Model: k-NN for SJ
sj_knn_grid_cv


# Train the k-NN model with cross-validation for IQ
iq_knn_grid_cv <- train(
  X_iq_train, y_iq_train,
  method = "knn",
  trControl = trainControl(
    method = "cv",  # Cross-validation
    number = 5,     # Number of folds
    summaryFunction = defaultSummary  # Include metrics like MAE
  ),
  tuneGrid = expand.grid(k = c(3, 5, 7)),  # Only tuning over k
  metric = "MAE"  # Optimize for Mean Absolute Error
)


# Extract best score (MAE)
best_score <- min(iq_knn_grid_cv$results$MAE)

# Extract best parameters
best_params <- iq_knn_grid_cv$bestTune

# Print results
print(best_score)
print(best_params)



# Predict on the test set
y_iq_pred <- predict(iq_knn_grid_cv$finalModel, X_iq_test)

# Calculate Mean Absolute Error (MAE)
mae_iq_knn <- mean(abs(y_iq_test - y_iq_pred))

# Print MAE
print(mae_iq_knn)

iq_knn_grid_cv

# XGBOOST MODEL

library(xgboost)

# Convert the data to matrices
sj_dtrain <- as.matrix(X_sj_train)
sj_dtest <- as.matrix(X_sj_test)

iq_dtrain <- as.matrix(X_iq_train)
iq_dtest <- as.matrix(X_iq_test)


# Define training parameters for xgboost
xgb_params <- list(
  objective = "reg:squarederror",  # Regression task
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Maximum tree depth
  subsample = 0.8,                 # Subsample ratio of training data
  colsample_bytree = 0.8           # Subsample ratio of columns
)

# Convert training data to DMatrix format
dtrain_sj <- xgb.DMatrix(data = sj_dtrain, label = y_sj_train)
dtest_sj <- xgb.DMatrix(data = sj_dtest, label = y_sj_test)

# Train XGBoost model for San Juan
xgb_sj <- xgb.train(
  params = xgb_params,
  data = dtrain_sj,
  nrounds = 100,
  watchlist = list(train = dtrain_sj, test = dtest_sj),
  early_stopping_rounds = 10,
  print_every_n = 10
)

# Train XGBoost model for Iquitos
dtrain_iq <- xgb.DMatrix(data = iq_dtrain, label = y_iq_train)
dtest_iq <- xgb.DMatrix(data = iq_dtest, label = y_iq_test)

xgb_iq <- xgb.train(
  params = xgb_params,
  data = dtrain_iq,
  nrounds = 100,
  watchlist = list(train = dtrain_iq, test = dtest_iq),
  early_stopping_rounds = 10,
  print_every_n = 10
)

# Predict for San Juan
xgb_sj_pred <- predict(xgb_sj, sj_dtest)
mae_sj_xgb <- mean(abs(xgb_sj_pred - y_sj_test))
cat("XGBoost MAE (San Juan):", mae_sj_xgb, "\n")

# Predict for Iquitos
xgb_iq_pred <- predict(xgb_iq, iq_dtest)
mae_iq_xgb <- mean(abs(xgb_iq_pred - y_iq_test))
cat("XGBoost MAE (Iquitos):", mae_iq_xgb, "\n")





# COMPARE RESULTS
# Create a data frame to store the test results for both models and datasets
test_results <- data.frame(
  Model = c("Random Forest", "Random Forest", "k-NN", "k-NN", "XGBoost", "XGBoost"),
  Dataset = c("San Juan (SJ)", "Iquitos (IQ)", "San Juan (SJ)", "Iquitos (IQ)", "San Juan (SJ)", "Iquitos (IQ)"),
  MAE = c(mae_sj_rf, mae_iq_rf, mae_sj_knn, mae_iq_knn, mae_sj_xgb, mae_iq_xgb)
)


# Print the results
print("Test Results Comparison:")
print(test_results)



# Create a bar plot to compare MAE across models and datasets
ggplot(test_results, aes(x = Dataset, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Model Comparison: MAE on Test Data",
       x = "Dataset",
       y = "Mean Absolute Error (MAE)",
       fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add scatter plots for actual vs predicted values
# Random Forest: San Juan
rf_sj_comparison <- data.frame(Actual = y_sj_test, Predicted = sj_rf_pred)
ggplot(rf_sj_comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  labs(title = "Random Forest: Actual vs Predicted (SJ)",
       x = "Actual Total Cases",
       y = "Predicted Total Cases") +
  theme_minimal()

# Random Forest: Iquitos
rf_iq_comparison <- data.frame(Actual = y_iq_test, Predicted = iq_rf_pred)
ggplot(rf_iq_comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "green") +
  labs(title = "Random Forest: Actual vs Predicted (IQ)",
       x = "Actual Total Cases",
       y = "Predicted Total Cases") +
  theme_minimal()

# k-NN: San Juan
knn_sj_comparison <- data.frame(Actual = y_sj_test, Predicted = y_sj_pred)
ggplot(knn_sj_comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "red") +
  labs(title = "k-NN: Actual vs Predicted (SJ)",
       x = "Actual Total Cases",
       y = "Predicted Total Cases") +
  theme_minimal()

# k-NN: Iquitos
knn_iq_comparison <- data.frame(Actual = y_iq_test, Predicted = y_iq_pred)
ggplot(knn_iq_comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple") +
  labs(title = "k-NN: Actual vs Predicted (IQ)",
       x = "Actual Total Cases",
       y = "Predicted Total Cases") +
  theme_minimal()


