#####################################
## Logistic regression small exercise
#####################################

###############################
## Step 1: Generate sample data
###############################

library(ggplot2)
library(tidyverse)

# For reproducibility
set.seed(123)

# Generate data
df <- data.frame(
  age = round(rnorm(100, mean = 60, sd = 10)),
  BMI = round(rnorm(100, mean = 25, sd = 4), 1),
  smoker = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3))
)

# Simulate heart attack occurrence (dependent on age, BMI, smoker)

df$risk_score <- with(df, 
                      0.02 * age + 0.05 * BMI + 0.6 * smoker + rnorm(100))

  df$heart_attack <- with(df, ifelse(df$risk_score > 3, 1, 0))


head(df)

############################################
## Step 2: Split Data (Training and Testing)
############################################

# Install if needed:
install.packages("caTools")
library(caTools)

set.seed(42)
split <- sample.split(df$heart_attack, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

dim(train)
dim(test)

########################################
## Step 3: Fit logistic regression model
########################################

# Logistic regression model
model <- glm(heart_attack ~ age + BMI + smoker, data = train, family = binomial)

# Model summary
summary(model)

###################################################
## Step 4: Step 4: Make Predictions on Testing Data
###################################################

# Predict probabilities
test$predicted_prob <- predict(model, newdata = test, type = "response")

# Classify predictions (threshold = 0.5)
test$predicted_class <- ifelse(test$predicted_prob >= 0.5, 1, 0)

head(test)

#############################
## Step 5: Evaluate the Model
#############################

# Accuracy: Measures how many predictions were correct.
# Confusion Matrix: Shows true positives, false positives, true negatives, and false negatives.


# Confusion matrix
table(Predicted = test$predicted_class, Actual = test$heart_attack)

# Calculate accuracy
accuracy <- mean(test$predicted_class == test$heart_attack)
cat("Accuracy:", accuracy)



