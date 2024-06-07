# IN THIS CODE WE TRY DIFFERENT KIND OF SCALING TO FIT A REGRESSION MODEL

# Load libraries ----
library(corrplot)
library(car)

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

head(standardized_data)



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


# THE BEST ONE IS:
epsilon <- 1e-9
log_tran_data <- log(numerical_vars + epsilon)

# set as df

log_tran_data_df <- as.data.frame(log_tran_data)

mod_log_tr_2 <- lm(Calories ~ ., data = log_tran_data_df)

summary(mod_log_tr_2)

AIC(mod_log_tr_2)
BIC(mod_log_tr_2)
vif(mod_log_tr_2)


# MIN MAX SCALING

min_max_data <- as.data.frame(apply(numerical_vars, 2, function(x) (x - min(x)) / (max(x) - min(x))))

mod_min_max <- lm(Calories ~ ., data = min_max_data)

summary(mod_min_max)

AIC(mod_min_max)
BIC(mod_min_max)
vif(mod_min_max)

par(mfrow = c(2, 2))
plot(mod_min_max)

# VIENE MALE


# ROBUST SCALING
# Robust scaling is a technique that is robust to outliers.
# It is calculated by subtracting the median from each observation and dividing by the IQR.
# This technique is useful when the data contains outliers.

robust_data <- as.data.frame(apply(numerical_vars, 2, function(x) (x - median(x)) / IQR(x)))

mod_robust <- lm(Calories ~ ., data = robust_data)

summary(mod_robust)

AIC(mod_robust)
BIC(mod_robust)
vif(mod_robust)

par(mfrow = c(2, 2))
plot(mod_robust)
# Mean scaling



mean_data <- as.data.frame(apply(numerical_vars, 2, function(x) (x - mean(x)) / max(x) - min(x)))

mod_mean <- lm(Calories ~ ., data = mean_data)

summary(mod_mean)

AIC(mod_mean)
BIC(mod_mean)
vif(mod_mean)


par(mfrow = c(2, 2))
plot(mod_mean)

