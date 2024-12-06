# Load necessary libraries
library(dplyr)
library(randomForest)
library(caret)
library(tidyverse)

# Load the data (adjust the path accordingly)
data <- read.csv("crime_randomforest.csv")

# Drop rows with missing values
data <- na.omit(data)

# Define features and target
features <- c("vict_age", "vict_sex", "vict_descent", "AREA.NAME", "period")
target <- "Crm.Cd.Group"

# Encode categorical variables as factors
data[features] <- lapply(data[features], as.factor)
data[[target]] <- as.factor(data[[target]])

# Split data into training and testing sets
set.seed(42)
train_index <- createDataPartition(data[[target]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# ~~~~~~~~~~ Manual Oversampling for Multi-Class Balancing ~~~~~~~~~~

balance_multiclass_data <- function(data, target_column) {
  # Get the counts of each class
  class_counts <- table(data[[target_column]])
  max_count <- max(class_counts)  # Determine the largest class size
  
  # Oversample minority classes
  balanced_data <- do.call(rbind, lapply(names(class_counts), function(class) {
    class_data <- data[data[[target_column]] == class, ]
    if (nrow(class_data) < max_count) {
      # Oversample
      class_data <- class_data[sample(1:nrow(class_data), max_count, replace = TRUE), ]
    }
    return(class_data)
  }))
  
  return(balanced_data)
}

# Apply balancing to your training data
train_data_balanced <- balance_multiclass_data(train_data, target_column = target)

# Verify the class distribution in the balanced training data
cat("\nBalanced Class Distribution:\n")
print(table(train_data_balanced[[target]]))

# ~~~~~~~~~~ Train a Random Forest Model ~~~~~~~~~~

# Train a Random Forest model with balanced data
model <- randomForest(
  formula = as.formula(paste(target, "~", paste(features, collapse = "+"))),
  data = train_data_balanced,
  ntree = 100,
  mtry = floor(sqrt(length(features))),
  importance = TRUE
)

# Evaluate the model on test data
pred <- predict(model, test_data)
conf_matrix <- confusionMatrix(pred, test_data[[target]])
print(conf_matrix)

# ~~~~~~~~~~ Evaluating Top-3 and Low-3 Accuracy ~~~~~~~~~~

calculate_accuracy <- function(proba, y_true, top_n = 3, low_n = 3) {
  # Sort probabilities for each sample
  sorted_indices <- apply(proba, 1, order, decreasing = TRUE)  # Sorted class indices for each sample
  
  # Top-N Accuracy
  top_n_indices <- sorted_indices[1:top_n, , drop = FALSE]  # Indices of top-N classes
  top_n_correct <- sapply(1:nrow(proba), function(i) {
    as.numeric(y_true[i]) %in% top_n_indices[, i]
  })
  top_n_accuracy <- mean(top_n_correct)
  
  # Low-N Accuracy
  low_n_indices <- sorted_indices[(nrow(sorted_indices) - low_n + 1):nrow(sorted_indices), , drop = FALSE]  # Indices of low-N classes
  low_n_correct <- sapply(1:nrow(proba), function(i) {
    as.numeric(y_true[i]) %in% low_n_indices[, i]
  })
  low_n_accuracy <- mean(low_n_correct)
  
  return(list(top_n_accuracy = top_n_accuracy, low_n_accuracy = low_n_accuracy))
}

# Predict probabilities
proba <- predict(model, test_data, type = "prob")

# Convert true labels to numeric indices
y_test_numeric <- as.numeric(test_data[[target]])

# Calculate Top-3 and Low-3 accuracy
accuracy_results <- calculate_accuracy(proba, y_test_numeric, top_n = 3, low_n = 3)
cat("Top-3 Accuracy:", round(accuracy_results$top_n_accuracy, 2), "\n")
cat("Low-3 Accuracy:", round(accuracy_results$low_n_accuracy, 2), "\n")

# ~~~~~~~~~~ Feature Importance ~~~~~~~~~~

# Calculate feature importance
importance <- as.data.frame(randomForest::importance(model))
importance$Feature <- rownames(importance)

# Normalize the MeanDecreaseGini values to sum to 1 (similar to Python)
importance <- importance %>%
  mutate(importance = MeanDecreaseGini / sum(MeanDecreaseGini)) %>%
  select(Feature, importance) %>%
  arrange(desc(importance))

# Print the importance in the desired format
cat("Feature Importance:\n")
print(importance)

# ~~~~~~~~~~ Prediction for New Input ~~~~~~~~~~

predict_new_sample_with_probabilities <- function(model, input_data, features, train_data) {
  # Map numerical Vict.Age to age groups
  if ("Vict.Age" %in% colnames(input_data)) {
    input_data$vict_age <- case_when(
      input_data$Vict.Age == -1 ~ NA_character_,
      input_data$Vict.Age < 15 ~ "Youth (0~14)",
      input_data$Vict.Age > 64 ~ "Elderly (65~)",
      TRUE ~ "Adult (15~64)"
    )
    # Drop unused Vict.Age column
    input_data <- input_data %>% select(-Vict.Age)
  }
  
  # Ensure factor levels match those in the training data
  for (col in features) {
    if (is.factor(train_data[[col]])) {
      input_data[[col]] <- factor(input_data[[col]], levels = levels(train_data[[col]]))
      if (any(is.na(input_data[[col]]))) {
        stop(paste("Invalid levels in input data for feature:", col))
      }
    }
  }
  
  # Predict probabilities
  probabilities <- predict(model, input_data, type = "prob")
  
  # Predict the most likely class
  predicted_label <- predict(model, input_data)
  
  # Combine predicted label and probabilities into a result
  result <- list(predicted_label = predicted_label, probabilities = probabilities)
  return(result)
}

# Example Input
example_input <- data.frame(
  Vict.Age = 30,           # Numerical age
  vict_sex = "M",          # Categorical
  vict_descent = "Hispanic",      # Categorical
  AREA.NAME = "77TH STREET",   # Categorical
  period = "night"       # Categorical
)

# Predict for the example input
tryCatch({
  prediction_results <- predict_new_sample_with_probabilities(model, example_input, features, train_data)
  # Print predicted label
  cat("Predicted Label:", as.character(prediction_results$predicted_label), "\n")
  # Print probabilities
  cat("Probabilities:\n")
  print(prediction_results$probabilities)
}, error = function(e) {
  print(paste("Error:", e$message))
})

