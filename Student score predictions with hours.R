# Tittle: Prediction of the % of student based on the number of study hour (simple linear regression)

## LOADING DATA
# Load necessary libraries
library(readxl)
library(ggplot2)

# Load the dataset
Hours_Scores <- read_excel("Hours_Scores.xlsx")
View(Hours_Scores)
names(Hours_Scores)
print(names(Hours_Scores))
print(str(Hours_Scores))
print(sapply(Hours_Scores, class))

## EXPLORATORY DATA ANALYSIS
# View the first few rows of the data
head(Hours_Scores)

# Summary statistics
summary(Hours_Scores)

# Structure of the data
str(Hours_Scores)

# visualization of the data
ggplot(Hours_Scores, aes(x=Hours, y=Scores)) + 
  geom_point() + 
  labs(title="Hours vs Scores", x="Study Hours", y="Percentage Score")

## Linear Regression Model
# Fit the linear model
model <- lm(Scores ~ Hours, data=Hours_Scores)

# Summary of the model
print(summary(model))

# Plot the regression line
ggplot(Hours_Scores, aes(x=Hours, y=Scores)) + 
  geom_point() + 
  geom_smooth(method="lm", col="blue") + 
  labs(title="Hours vs Scores with Regression Line", x="Study Hours", y="Percentage Score")

# Make predictions
predicted_scores <- predict(model, newdata=Hours_Scores)

# Add predictions to the data frame
Hours_Scores$Predicted_Scores <- predicted_scores

# Calculate residuals
Hours_Scores$Residuals <- Hours_Scores$Scores - Hours_Scores$Predicted_Scores

# Print the first few rows to verify predictions and residuals
print(head(Hours_Scores))

## Observed vs Predicted Plot
# Plot the observed vs predicted values
ggplot(Hours_Scores, aes(x=Hours, y=Scores)) + 
  geom_point() + 
  geom_line(aes(y=Predicted_Scores), color="blue") + 
  labs(title="Observed vs Predicted Scores", x="Study Hours", y="Scores")

## Model Evaluation
# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(Hours_Scores$Residuals^2))
print(paste("RMSE:", rmse))

# Mean Absolute Error (MAE)
mae <- mean(abs(Hours_Scores$Residuals))
print(paste("MAE:", mae))

# Prediction score for a student who studies 9.25 hours per day
new_data <- data.frame(Hours = 9.25)
predicted_score <- predict(model, newdata=new_data)
print(paste("Predicted score for a student who studies 9.25 hours per day:", predicted_score))

