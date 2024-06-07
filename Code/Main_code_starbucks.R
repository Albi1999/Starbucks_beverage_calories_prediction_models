# Statistical Learning final project

# Starbucks 
# Link: https://www.kaggle.com/datasets/henryshan/starbucks

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

# Correlation Analysis ----

# Remove first 3 columns for the correlation matrix

data_num <- data_cleaned[, sapply(data_cleaned, is.numeric)]

## Correlation Matrix ----
# Calculate the correlation matrix

correlation_matrix <- cor(data_num)

## Plot the correlation matrix ----
par(mfrow = c(1, 1))
corrplot(correlation_matrix, method = "number", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.5, tl.cex = 0.7)

## Heatmap of the correlation matrix ----
heatmap(cor(data_num), 
        col = colorRampPalette(c("#005cff", "#fbfbfb", "#d90000"))(100), 
        symm = TRUE, 
        margins = c(8, 8), 
        cexRow = 0.8,
        cexCol = 0.8)

# Data Visualization ----

## Histograms ----

# Histogram of the data with density distribution
par(mfrow = c(5, 3), mar = c(2, 2, 2, 2))
col <- c('#ff0000', '#f70028', '#ee0040', '#e50055', '#dc0069',
         '#d2007b', '#c7008d', '#bb009e', '#ae00ae', '#a000be',
         '#8f00cc', '#7d00da', '#6700e7', '#4900f3', '#0000ff')
for (i in 1:ncol(data_num)) {
  hist(data_num[, i], main = colnames(data_num)[i],
       xlab = colnames(data_num)[i], col = col[i], freq = FALSE)
  dens <- density(data_num[, i], na.rm=TRUE, adjust=1.25)
  lines(dens, col = "black", lwd = 2)
}

## Pairplot ----

# Define function for the pairplot

# Histogram function
panel.hist <- function(x, colour, ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = colour, ...)
}

# Correlations function
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


pairs(data_num, 
      diag.panel = panel.hist,
      upper.panel = panel.cor, 
      lower.panel = panel.smooth,
      colour = "#4ea5ff")

## Barplot ----
# Barplot of the data grouped by categories
par(mfrow = c(5, 3), mar = c(2, 2, 2, 2))
for (i in 1:ncol(data_num)) {
  barplot(table(data_num[, i]), main = colnames(data_num)[i],
          xlab = colnames(data_num)[i], col = col[i], border = col[i])
}

### Beverage ----
# frequency for 'Beverage_category' ----> 
# how we can see the most famous bevarage?

# We create a barplot to visualize the distribution 
# of the 'Beverage_category' variable
par(mfrow = c(1, 1), mar = c(8, 2, 2, 2))
barplot(table(data$Beverage_category),
        main = "Distribution of Beverage Categories",
        ylab = "Count",
        col = "#4ea5ff",
        las = 2, 
        cex.names = 0.6)

# We create a barplot to visualize the distribution 
# of the preparation of the bevarage
barplot(table(data$Beverage_prep),
        main = "Distribution of Beverage Preparation",
        ylab = "Count",
        col = "#ff810f",
        las = 2,
        cex.names = 0.6)

# "#00cd5c" green

# Now we want to compare the total calories for each categories of bevarage

# First we aggrgate the data to obtain the total calories 
# for each categories of bevarage

# Then we create a barplot to visualize the results

par(mfrow = c(1, 1), mar = c(8, 2, 2, 2))
total_calories_by_category <- aggregate(Calories ~ Beverage_category,
                                        data = data_cleaned, sum)

barplot(height = total_calories_by_category$Calories,
        names.arg = total_calories_by_category$Beverage_category,
        main = "Total Calories by Beverage Category",
        ylab = "Total Calories",
        col = "#4ea5ff",
        las = 2,
        cex.names = 0.6)

# Now we want to compare the total sugars for each preparation of bevarage

# First we aggrgate the data to obtain the total sugars 
# for each preparation of bevarage

# Then we create a barplot to visualize the results

par(mfrow = c(1, 1), mar = c(8, 2, 2, 2))
total_sugar_by_prep <- aggregate(Total_Carbohydrates ~ Beverage_prep,
                                 data = data_cleaned, sum)

barplot(height = total_sugar_by_prep$Total_Carbohydrates,
        names.arg = total_sugar_by_prep$Beverage_prep,
        main = "Total Sugars by Beverage Preparation",
        ylab = "Total Sugars (g)",
        col = "#ff810f",
        las = 2,
        cex.names = 0.6)

## Boxplot ----
# Boxplot of the data
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2))
for (i in 1:ncol(data_num)) {
  boxplot(data_num[, i], main = colnames(data_num)[i],
          xlab = colnames(data_num)[i], col = col[i])
}


## Scatterplot ----
# variable Beverage_category factor
data_cleaned$Beverage_category <- as.factor(data_cleaned$Beverage_category)

# Assign distinct colors to each beverage category
colors <- rainbow(length(unique(data_cleaned$Beverage_category)))
color_map <- setNames(colors, levels(data_cleaned$Beverage_category))

# Create a scatterplot to compare amounts of calories and fat 
# for each categories of bevarage
par(mfrow = c(1, 1))
plot(data_cleaned$Calories, 
     data_cleaned$Total_Fat_g,
     col = color_map[data_cleaned$Beverage_category],
     pch = 19,
     xlab = "Calories",
     ylab = "Total Fat (g)",
     main = "Calories vs Total Fat")

# Legend
legend("topleft", legend = levels(data_cleaned$Beverage_category), 
       col = colors, cex = 0.4, pch = 19)

# Comparision between total fat and trans fat ( che cazzo sono?)

# Numeric variable -> calculate density
total_fat_density <- density(data_cleaned$Total_Fat)
trans_fat_density <- density(data_cleaned$Trans_Fat)

# create a grafico sovrapposto 
plot(total_fat_density, col = "#4ea5ff",
     main = "Comparison of Total Fat and Trans Fat Distributions", 
     xlab = "Fat Content (g)", ylab = "Density", 
     ylim = c(0, max(total_fat_density$y, trans_fat_density$y)),
     xlim = range(data_cleaned$Total_Fat, data_cleaned$Trans_Fat), 
     lwd = 2, lty = 1)
lines(trans_fat_density, col = "#ff810f", lwd = 2, lty = 1)
legend("topright", legend = c("Total Fat", "Trans Fat"),
       col = c("#4ea5ff", "#ff810f"), lwd = 2, lty = 1)
# Create scatterplot to look into relantionship between 
# calories and other variables 

str(data_cleaned)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
with(data_cleaned, {
  plot(Calories, Sodium , main = "Relation between Calories and Sodium",
       xlab = "Calories", ylab = "Sodium (mg)", col = col[1])
  plot(Calories, Protein , main = "Relation between Calories and Protein",
       xlab = "Calories", ylab = "Protein (g)", col = col[5])
  plot(Calories, Vitamin_C , main = "Relation between Calories and Vitamin C",
       xlab = "Calories", ylab = "Vitamin C (mg)", col = col[10])
  plot(Calories, Cholesterol , main = "Relation between Calories and Fiber",
       xlab = "Calories", ylab = "Fiber (g)", col = col[15])
})

# There's increase in every feature with increase in calories

# Features like proteins and fiber rapidly increase, 
# instead vitamin and cholesterol more flat growing 

# Confirmed by correlation coefficients 


# Regression Analysis ----
## Linear Regression ----

# Linear regression model to predict the amount of calories
# based on the amount of the other variables
# We use the lm() function to fit a linear regression model
# We use the summary() function to display the results
# We use the plot() function to visualize the results

# Fit the linear regression model

# Set the calories as the dependent variable
y <- data_num$Calories


lm_model <- lm(y ~ ., data = data_num)
summary(lm_model)
par(mfrow = c(2, 2))
plot(lm_model)

AIC(lm_model)
BIC(lm_model)

# The model has a low AIC and BIC values, the R-squared value is 0.99 so the model is a good fit
# The model is significant, the p-value is less than 0.05

vif(lm_model)
# The VIF values are more than 10, so there is multicollinearity
# To solve this problem, we try to normalize the data 
# using the log transformation

### Log Transformation ----

# We standardize the data using a logaritmic transformation

std_data_log <- scale(log(data_num + 1))

# Set as dataframe

std_data_log_df <- as.data.frame(std_data_log)

# Fit the linear regression model using the standardized variables as predictors

mod_log_tr <- lm(y ~ ., data = std_data_log_df)

summary(mod_log_tr)

AIC(mod_log_tr)
BIC(mod_log_tr)
vif(mod_log_tr)

par(mfrow = c(2, 2))
plot(mod_log_tr)

# The model has a low AIC and BIC values, the R-squared value is 0.99 so the model is a good fit

# Backward selection

mod_log_tr_backward <- step(mod_log_tr, direction = "backward")
summary(mod_log_tr_backward)
AIC(mod_log_tr_backward)
BIC(mod_log_tr_backward)
vif(mod_log_tr_backward)

par(mfrow = c(2, 2))
plot(mod_log_tr_backward)

# We have still collinearity, so we try to use another model

## Lasso Regression ----

# We use the glmnet package to fit the lasso regression model
# The glmnet package is used to fit generalized linear models via penalized maximum likelihood
# The function cv.glmnet() is used to fit a lasso regression model with cross-validation

# Fit the lasso regression model
mod_lasso_log <- cv.glmnet(x = as.matrix(std_data_log_df[, -1]),
                           y = y,
                           alpha = 1, standardize = FALSE)

par(mfrow = c(1, 1))
plot(mod_lasso_log)
summary(mod_lasso_log)
lasso_coef_log <- coef(mod_lasso_log, s = "lambda.min")
lasso_coef_log

# Now we try with another transformation of the data

# Standardize the data
std_data <- as.data.frame(scale(data_num))

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

# Calculate the R-squared value for the lasso regression model
lasso_pred <- predict(mod_lasso, s = "lambda.min", newx = as.matrix(std_data[, -1]))
lasso_r_squared <- cor(lasso_pred, std_data$Calories)^2
lasso_r_squared

# Calculate the MSE for the lasso regression model
lasso_mse <- mean((lasso_pred - std_data$Calories)^2)
lasso_mse

# Calculate the R-squared value for the lasso regression model with log transformation
lasso_pred_log <- predict(mod_lasso_log, s = "lambda.min", newx = as.matrix(std_data_log_df[, -1]))
lasso_r_squared_log <- cor(lasso_pred_log, y)^2
lasso_r_squared_log

# Calculate the MSE for the lasso regression model with log transformation
lasso_mse_log <- mean((lasso_pred_log - y)^2)
lasso_mse_log

# With the normal standardization is better than the log transformation

## Ridge regression ----
# The ridge regression model is fit using the glmnet package
# The cv.glmnet() function is used to fit the ridge regression model with cross-validation
# The alpha parameter is set to 0 to fit the ridge regression model
# The standardize parameter is set to FALSE to use the standardized data
# The optimal lambda value is selected based on the cross-validated mean squared error (MSE)
# The optimal lambda value is used to fit the final ridge regression model
# The coefficients of the ridge regression model are extracted and displayed
# The coefficients of the ridge regression model are displayed in a plot

# Fit the ridge regression model with the logarithmic transformation
mod_ridge_log <- cv.glmnet(x = as.matrix(std_data_log_df[, -1]),
                           y = std_data_log_df$Calories,
                           alpha = 0, standardize = FALSE)

plot(mod_ridge_log)
summary(mod_ridge_log)
ridge_coef_log <- coef(mod_ridge_log, s = "lambda.min")
ridge_coef_log

# Fit the ridge regression model with the standardization
mod_ridge <- cv.glmnet(x = as.matrix(std_data[, -1]),
                       y = std_data$Calories,
                       alpha = 0, standardize = FALSE)

# Plot the cross-validated mean squared error (MSE) as a function of the lambda values
plot(mod_ridge)
summary(mod_ridge)
ridge_coef <- coef(mod_ridge, s = "lambda.min")
ridge_coef

## Model comparison ----
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

kable(data.frame(Model = c("Linear Regression", "Lasso Regression", "Ridge Regression"),
                 AIC = c(AIC(lm_model), AIC(mod_lasso$glmnet.fit), AIC(mod_ridge$glmnet.fit)),
                 BIC = c(BIC(lm_model), BIC(mod_lasso$glmnet.fit), BIC(mod_ridge$glmnet.fit)),
                 VIF = c(vif(lm_model), vif(mod_lasso$glmnet.fit), vif(mod_ridge$glmnet.fit))))
## Cross validation ----
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

plot(test_data$Calories, lasso_pred_test, xlab = "Actual Calories",
     ylab = "Predicted Calories", main = "Predicted vs Actual Calories",
     col = "#4ea5ff", pch = 19)


# The plot shows the predicted values against the actual values on the test data
# The points are close to the diagonal line, indicating that the model is making accurate predictions
# The R-squared value and MSE are used to evaluate the performance of the model
# The R-squared value is 0.99, indicating that the model explains 99% of the variance in the data


## Logistic Regression ----

# Logistic regression model to predict the amount of calories
# based on the amount of the other variables
# We use the glm() function to fit a logistic regression model

# Fit the logistic regression model

glm_model <- glm(y ~ ., data = data_num, family = "gaussian") # Try to change the family
summary(glm_model)
par(mfrow = c(2, 2))
plot(glm_model)

AIC(glm_model)
BIC(glm_model)

# Comment on it.....
