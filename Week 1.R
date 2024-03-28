library(caret)
library(dplyr)

# Read data
data <- read.csv("C:/Users/91770/Desktop/New folder/Machine Learning/Sisheerra/oulad-students.csv")

# Remove rows with missing values
data <- data %>%
  na.omit()

# Convert categorical variables into factors
categorical_vars <- c("code_module", "code_presentation", "gender", "region", 
                      "highest_education", "imd_band", "age_band", 
                      "num_of_prev_attempts", "disability", "final_result")

data <- data %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Split data into training (80%) and testing (20%) sets
set.seed(120)
train_index <- createDataPartition(data$final_result, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- train(final_result ~ ., data = train_data, method = "glm", family = "binomial")

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
confusionMatrix(predictions, test_data$final_result)

