# IN THIS CODE WE TRY DIFFERENT KIND OF SCALING TO FIT A REGRESSION MODEL

# Load libraries ----
library(corrplot)
library(car)
library(glmnet)

# Load data ----
data <- read.csv("Data/starbucks.csv", header = TRUE, sep = ",")

# Overview of the data ----
summary(data)
str(data)

# Check how many categories there are in the first column
table(data$Beverage_category)
unique(data$Beverage_category)
table(data$Beverage_prep)
unique(data$Beverage_prep)
table(data$Beverage)
unique(data$Beverage)

# Data transformation ----
# Remove percentage sign from the data
data$Vitamin.C....DV. <- as.numeric(gsub("%", "", data$Vitamin.C....DV.))
data$Calcium....DV. <- as.numeric(gsub("%", "", data$Calcium....DV.))
data$Iron....DV. <- as.numeric(gsub("%", "", data$Iron....DV.))
data$Vitamin.A....DV. <- as.numeric(gsub("%", "", data$Vitamin.A....DV.))

# Set the other variables as numeric
data$Calories <- as.numeric(data$Calories)
data$Trans.Fat..g. <- as.numeric(data$Trans.Fat..g.)
data$Total.Fat..g. <- as.numeric(data$Total.Fat..g.)
data$Cholesterol..mg. <- as.numeric(data$Cholesterol..mg.)
data$Sodium..mg. <- as.numeric(data$Sodium..mg.)
data$Total.Carbohydrates..g. <- as.numeric(data$Total.Carbohydrates..g.)
data$Dietary.Fibre..g. <- as.numeric(data$Dietary.Fibre..g.)
data$Sugars..g. <- as.numeric(data$Sugars..g.)
data$Caffeine..mg. <- as.numeric(data$Caffeine..mg.)

# We have some missing values in the variable caffeine 
is.na(data$Caffeine..mg.)
# There are different way to impute NA
# Use the median is a suitable choice because we look 
# into structure of the variable
summary(data$Caffeine..mg.)


# create a copy of original dataset 
data_cleaned <- data

# Presence of Outliers: Looking at the summary statistics provided, 
# the presence of outliers is indicated by the large difference 
# between the mean (89.52 mg.) and the median (75 mg.).

# Outliers can skew the mean, making it less representative of the 
# central tendency of the data.

# The median, being resistant to outliers, provides a better 
# measure of central tendency in this scenario.

# Possibly non-normal distribution 
# With 23 missing values out of 242 observations, 
# the dataset has a relatively small proportion of missing values 
# (approximately 9.5%). 


# Impute NA with median of the  'Caffeine..mg.', without NA
data_cleaned$Caffeine..mg.[is.na(data_cleaned$Caffeine..mg.)] <- median(data_cleaned$Caffeine..mg., na.rm = TRUE)

# no more NA ---> now we can work directly on cleaned dataset 
summary(data_cleaned$Caffeine..mg.)
is.na(data_cleaned$Caffeine..mg.)


# Rename the cleaned data columns by removing the dots and the unity of measure
colnames(data_cleaned) <- c("Beverage_category", "Beverage",
                            "Beverage_prep", "Calories",
                            "Total_Fat", "Trans_Fat",
                            "Saturated_Fat", "Sodium",
                            "Total_Carbohydrates", "Cholesterol",
                            "Dietary_Fibre", "Sugars",
                            "Protein", "Vitamin_A",
                            "Vitamin_C", "Calcium",
                            "Iron", "Caffeine")

# We have to standardize the data before fitting the regression model
# because the variables have different scales and the data distribution is unbalanced

# Select only numeric variables from the cleaned dataset in order to implement regression model

numerical_vars <- data_cleaned[, sapply(data_cleaned, is.numeric)]

# Before starting to fit the regression model we want to standardize our numeric variable

# We standardize the data using a logaritmic transformation
# because the data is not normally distributed

standardized_data_log <- scale(log(numerical_vars + 1))

# Visualize the first 5 records of the standardized data



# Fit the linear regression model

# Turn into dataframe in order to use into regression

standardized_data_log_df <- as.data.frame(standardized_data_log)

# Fit the model of regression using the standardized variables as predictors

mod_log_tr <- lm(Calories ~ ., data = standardized_data_log_df)

summary(mod_log_tr)

AIC(mod_log_tr)
BIC(mod_log_tr)
vif(mod_log_tr)

par(mfrow = c(2, 2))
plot(mod_log_tr)




# Bakcward selection

mod_log_tr_backward <- step(mod_log_tr, direction = "backward")
summary(mod_log_tr_backward)
AIC(mod_log_tr_backward)
BIC(mod_log_tr_backward)
vif(mod_log_tr_backward)

par(mfrow = c(2, 2))
plot(mod_log_tr_backward)

# Stampare le variabili nel modello backward
cat("\nVariabili nel modello stepwise:\n")
names(coefficients(mod_log_tr_backward))

# now we have to manually drop cholesterol and sugars because the vif is too high

mod_log_tr_backward <- lm(Calories ~ . - Cholesterol - Sugars, data = standardized_data_log_df)

mod_log_tr_backward <- step(mod_log_tr_backward, direction = "backward")
summary(mod_log_tr_backward)
AIC(mod_log_tr_backward)
BIC(mod_log_tr_backward)
vif(mod_log_tr_backward)

par(mfrow = c(2, 2))
plot(mod_log_tr_backward)


# Since the model is good but the vif is too high we can try to use a different model: lasso and ridge regression

# Lasso regression

# We use the glmnet package to fit the lasso regression model
# The glmnet package is used to fit generalized linear models via penalized maximum likelihood
# The function cv.glmnet() is used to fit a lasso regression model with cross-validation


std_data <- as.data.frame(scale(numerical_vars))


# Fit the lasso regression model
mod_lasso <- cv.glmnet(x = as.matrix(std_data[, -1]),
                       y = std_data$Calories,
                       alpha = 1, standardize = FALSE)

# Plot the cross-validated mean squared error (MSE) as a function of the lambda values
# The lambda value is the penalty term that is used to shrink the coefficients of the model
# The lambda value that minimizes the MSE is selected as the optimal lambda value
# The optimal lambda value is used to fit the final lasso regression model
par(mfrow = c(1, 1))
plot(mod_lasso)
summary(mod_lasso)
lasso_coef <- coef(mod_lasso, s = "lambda.min")
lasso_coef


# Now the same but with the logarithmic transformation

mod_lasso_log <- cv.glmnet(x = as.matrix(standardized_data_log_df[, -1]),
                           y = standardized_data_log_df$Calories,
                           alpha = 1, standardize = FALSE)

plot(mod_lasso_log)
summary(mod_lasso_log)
lasso_coef_log <- coef(mod_lasso_log, s = "lambda.min")
lasso_coef_log

# Ridge regression
# The ridge regression model is fit using the glmnet package
# The cv.glmnet() function is used to fit the ridge regression model with cross-validation
# The alpha parameter is set to 0 to fit the ridge regression model
# The standardize parameter is set to FALSE to use the standardized data
# The optimal lambda value is selected based on the cross-validated mean squared error (MSE)
# The optimal lambda value is used to fit the final ridge regression model
# The coefficients of the ridge regression model are extracted and displayed
# The coefficients of the ridge regression model are displayed in a plot

# Fit the ridge regression model

mod_ridge <- cv.glmnet(x = as.matrix(std_data[, -1]),
                       y = std_data$Calories,
                       alpha = 0, standardize = FALSE)

# Plot the cross-validated mean squared error (MSE) as a function of the lambda values

plot(mod_ridge)
summary(mod_ridge)
ridge_coef <- coef(mod_ridge, s = "lambda.min")
ridge_coef

# Now the same but with the logarithmic transformation

mod_ridge_log <- cv.glmnet(x = as.matrix(standardized_data_log_df[, -1]),
                           y = standardized_data_log_df$Calories,
                           alpha = 0, standardize = FALSE)

plot(mod_ridge_log)
summary(mod_ridge_log)
ridge_coef_log <- coef(mod_ridge_log, s = "lambda.min")
ridge_coef_log

# now we want to compare the model using the R-squared value
# The R-squared value is a measure of how well the model fits the data
# The R-squared value ranges from 0 to 1, with higher values indicating a better fit

# Calculate the R-squared value for the lasso regression model

lasso_pred <- predict(mod_lasso, s = "lambda.min", newx = as.matrix(std_data[, -1]))
lasso_r_squared <- cor(lasso_pred, std_data$Calories)^2
lasso_r_squared

# Calculate the R-squared value for the ridge regression model

ridge_pred <- predict(mod_ridge, s = "lambda.min", newx = as.matrix(std_data[, -1]))
ridge_r_squared <- cor(ridge_pred, std_data$Calories)^2
ridge_r_squared

# Compute the MSE for the models 
# The MSE is a measure of the average squared difference between the predicted and actual values
# The MSE is used to evaluate the performance of the model, with lower values indicating better performance

# Calculate the MSE for the lasso regression model

lasso_mse <- mean((lasso_pred - std_data$Calories)^2)
lasso_mse

# Calculate the MSE for the ridge regression model

ridge_mse <- mean((ridge_pred - std_data$Calories)^2)
ridge_mse

# We choose the model with the highest R-squared value and the lowest MSE
# The model il the lasso because it has the lower value for R^2 and MSE.

# CROSS VALIDATION

# Split the data in training and test set then check the accuracy of the model.

# Split the data into training and test sets

set.seed(123)
train_index <- sample(1:nrow(std_data), 0.8 * nrow(std_data))
train_data <- std_data[train_index, ]

test_data <- std_data[-train_index, ]

# Fit the lasso regression model on the training data

mod_lasso_train <- cv.glmnet(x = as.matrix(train_data[, -1]),
                             y = train_data$Calories,
                             alpha = 1, standardize = FALSE)

# Predict the calories on the test data using the lasso regression model

lasso_pred_test <- predict(mod_lasso_train, s = "lambda.min", newx = as.matrix(test_data[, -1]))

# Calculate the R-squared value for the lasso regression model on the test data

lasso_r_squared_test <- cor(lasso_pred_test, test_data$Calories)^2
lasso_r_squared_test

# Calculate the MSE for the lasso regression model on the test data
# The MSE is a measure of the average squared difference between the predicted and actual values

lasso_mse_test <- mean((lasso_pred_test - test_data$Calories)^2)
lasso_mse_test

# Plot the predicted values against the actual values on the test data

plot(test_data$Calories, lasso_pred_test, xlab = "Actual Calories", ylab = "Predicted Calories", main = "Predicted vs Actual Calories")


# The plot shows the predicted values against the actual values on the test data
# The points are close to the diagonal line, indicating that the model is making accurate predictions
# The R-squared value and MSE are used to evaluate the performance of the model
# The R-squared value is 0.99, indicating that the model explains 99% of the variance in the data



