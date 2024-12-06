# Load necessary libraries
library(reticulate)

# Activate Python environment
#use_python("/path/to/python")  # Adjust to your Python environment path

# Import necessary Python modules
sklearn <- import("sklearn")
joblib <- import("joblib")

# Load the saved Python model and encoders
model <- joblib$load("random_forest_model.joblib")  # Replace with your model file
label_encoders <- joblib$load("label_encoders.pkl")  # Label encoders for features
target_label_mapping <- joblib$load("target_label_mapping.pkl")  # Target label mapping

# Define the feature order
features <- c("vict_age", "vict_sex", "vict_descent", "AREA NAME", "period")

# Function to map numerical vict_age to age groups (if necessary)
map_age_to_group <- function(age) {
  if (age < 15) {
    return("Youth (0~14)")
  } else if (age > 64) {
    return("Elderly (65~)")
  } else {
    return("Adult (15~64)")
  }
}

# Function to encode the input data
encode_input <- function(input_data, features, label_encoders) {
  encoded_input <- numeric(length(features))
  
  for (i in seq_along(features)) {
    feature <- features[i]
    value <- input_data[[feature]]
    
    if (feature == "vict_age") {
      # No encoding needed for numeric features
      encoded_input[i] <- as.numeric(value)
    } else {
      # Wrap value in a Python-compatible 1D array (list-like structure)
      value_py <- r_to_py(list(as.character(value)))  # Convert R value to Python list
      encoded_value <- label_encoders[[feature]]$transform(value_py)
      encoded_input[i] <- encoded_value[[1]]  # Extract the encoded value from Python list
    }
  }
  
  return(as.data.frame(t(encoded_input)))  # Return a 1-row data frame
}


# Example input data
example_input <- data.frame(
  vict_age = 30,
  vict_sex = "M",
  vict_descent = "Hispanic",
  `AREA NAME` = "CENTRAL",
  period = "late_night",
  check.names = FALSE
)
example_input
# Ensure input column order matches Python model's expected feature order
example_input <- example_input[, features]

# Encode the input data
encoded_input <- encode_input(example_input, features, label_encoders)

colnames(encoded_input) <- features

# Convert to Python-compatible format
encoded_input_py <- r_to_py(encoded_input)

# Predict using the Python model
prediction <- model$predict(encoded_input_py)
probabilities <- model$predict_proba(encoded_input_py)

# Decode the predicted label
predicted_label <- target_label_mapping[[as.character(prediction[1])]]

# Decode the probabilities
decoded_probabilities <- setNames(probabilities[1, ], target_label_mapping)

# Print the results
cat("Predicted Label:", predicted_label, "\n")
cat("Probabilities:\n")
print(decoded_probabilities)
