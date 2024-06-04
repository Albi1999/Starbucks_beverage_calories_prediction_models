# Statistical Learning final project

# Starbucks 
# Link: https://www.kaggle.com/datasets/henryshan/starbucks

# Load libraries ----
library(corrplot)

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

data_num <- data_cleaned[, -c(1:3)]

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

# Remove calories column in order to use the other variables 
# as independent variables
data_num <- data_num[, -1]

lm_model <- lm(y ~ ., data = data_num)
summary(lm_model)
par(mfrow = c(2, 2))
plot(lm_model)

AIC(lm_model)
BIC(lm_model)

# The model has a low AIC and BIC values, the R-squared value is 0.99 so the model is a good fit
# The model is significant, the p-value is less than 0.05

### Cross-validation ----
# Now we try with a different approach, 
# we split the data into training and testing sets
# We use the training set to fit the model 
# and the testing set to evaluate the model

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(data_num), 0.8 * nrow(data_num))
train_data <- data_num[train_index, ]
test_data <- data_num[-train_index, ]

# Fit the linear regression model using the training set
y_train <- y[train_index]

lm_model_train <- lm(y_train ~ ., data = train_data)
summary(lm_model_train)

AIC(lm_model_train)
BIC(lm_model_train)

# Make predictions using the testing set
y_test <- y[-train_index]
predictions_lm <- predict(lm_model_train, newdata = test_data)

# Evaluate the model using the testing set

# Calculate the mean squared error
mse_lm<- mean((y_test - predictions_lm)^2)
mse_lm

# Calculate the root mean squared error
rmse_lm <- sqrt(mse_lm)
rmse_lm

# Now we plot the residuals to check if the model is a good fit
par(mfrow = c(1, 1))
plot(lm_model_train, which = 1)

# The residuals are randomly distributed around zero,
# so the model is a good fit

# now we compute the accuracy of the model and then we plot the results

# Compute the accuracy of the model
accuracy_lm <- 1 - (rmse_lm / mean(y_test))
accuracy_lm

# Plot the results
plot(y_test, predictions_lm, main = "Actual vs Predicted Values",
     xlab = "Actual Values", ylab = "Predicted Values",
     col = "#4ea5ff", pch = 19)

# The actual and predicted values are close to each other,
# so the model is a good fit

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

### Cross-validation ----

# Fit the logistic regression model using the training set
glm_model_train <- glm(y_train ~ ., data = train_data, family = "gaussian")

# Make predictions using the testing set
predictions_glm <- predict(glm_model_train, newdata = test_data)

# Evaluate the model using the testing set

# Calculate the mean squared error
mse_glm <- mean((y_test - predictions_glm)^2)
mse_glm

# Calculate the root mean squared error
rmse_glm <- sqrt(mse_glm)
rmse_glm

# Now we plot the residuals to check if the model is a good fit
par(mfrow = c(1, 1))
plot(glm_model_train, which = 1)

# now we compute the accuracy of the model and then we plot the results

# Compute the accuracy of the model
accuracy_glm <- 1 - (rmse_glm / mean(y_test))
accuracy_glm

# Plot the results
plot(y_test, predictions_glm, main = "Actual vs Predicted Values",
     xlab = "Actual Values", ylab = "Predicted Values",
     col = "#4ea5ff", pch = 19)

