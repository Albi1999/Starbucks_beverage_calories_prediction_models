# NEW regression model

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
epsilon <- 1e-9
log_tran_data <- log(numerical_vars + epsilon)

# set as df

log_tran_data_df <- as.data.frame(log_tran_data)

# Fit the linear regression model

mod_log_tr <- lm(Calories ~ ., data = log_tran_data_df)

summary(mod_log_tr)

AIC(mod_log_tr)
BIC(mod_log_tr)
vif(mod_log_tr)

# Bakcward selection

mod_log_tr_backward <- step(mod_log_tr, direction = "backward")
summary(mod_log_tr_backward)
AIC(mod_log_tr_backward)
BIC(mod_log_tr_backward)
vif(mod_log_tr_backward)

# Stampare le variabili nel modello completo
cat("Variabili nel modello completo:\n")
names(coefficients(mod_log_tr))

# Stampare le variabili nel modello backward
cat("\nVariabili nel modello backward:\n")
names(coefficients(mod_log_tr_backward))


# Forwards selection

mod_log_tr_forward <- step(mod_log_tr, direction = "forward")
summary(mod_log_tr_forward)
AIC(mod_log_tr_forward)
BIC(mod_log_tr_forward)
vif(mod_log_tr_forward)

# Stampare le variabili nel modello forward
cat("\nVariabili nel modello forward:\n")
names(coefficients(mod_log_tr_forward))


# Stepwise selection

mod_log_tr_stepwise <- step(mod_log_tr, direction = "both")
summary(mod_log_tr_stepwise)
AIC(mod_log_tr_stepwise)
BIC(mod_log_tr_stepwise)
vif(mod_log_tr_stepwise)

# Stampare le variabili nel modello stepwise
cat("\nVariabili nel modello stepwise:\n")
names(coefficients(mod_log_tr_stepwise))



# ANOVA between full model and backward model

anova(mod_log_tr, mod_log_tr_backward)

# Analysis of residuals

# We can check the residuals of the model to see if they are normally distributed

# We can use the Shapiro-Wilk test to check the normality of the residuals

shapiro.test(residuals(mod_log_tr_backward))

par(mfrow = c(2, 2))
plot(mod_log_tr_backward)

plot(mod_logistic)






# Logistic model

# We can also fit a logistic model to predict the calories

# We have to create a binary variable to predict the calories
# We can use the median of the calories as threshold
# If the calories are greater than the median we set 1, otherwise 0
# We can use the median because the data is not normally distributed

median_calories <- median(log_tran_data_df$Calories)

log_tran_data_df$Calories <- ifelse(log_tran_data_df$Calories > median_calories, 1, 0)

# Fit the logistic model

mod_logistic <- glm(Calories ~ ., data = log_tran_data_df, family = "binomial")

summary(mod_logistic)

AIC(mod_logistic)
BIC(mod_logistic)
vif(mod_logistic)


