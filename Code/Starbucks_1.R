
# Starbucks data
library(olsrr)

data <- read.csv("Data/starbucks.csv", header = TRUE, sep = ",")

# Data transformation

# Remove percentage sign from the data

data$Vitamin.C....DV. <- as.numeric(gsub("%", "", data$Vitamin.C....DV.))
data$Calcium....DV. <- as.numeric(gsub("%", "", data$Calcium....DV.))
data$Iron....DV. <- as.numeric(gsub("%", "", data$Iron....DV.))
data$Vitamin.A....DV. <- as.numeric(gsub("%", "", data$Vitamin.A....DV.))

# as numeric data type

data$Calories <- as.numeric(data$Calories)
data$Trans.Fat..g. <- as.numeric(data$Trans.Fat..g.)
data$Total.Fat..g. <- as.numeric(data$Total.Fat..g.)
data$Cholesterol..mg. <- as.numeric(data$Cholesterol..mg.)
data$Sodium..mg. <- as.numeric(data$Sodium..mg.)
data$Total.Carbohydrates..g. <- as.numeric(data$Total.Carbohydrates..g.)
data$Dietary.Fibre..g. <- as.numeric(data$Dietary.Fibre..g.)
data$Sugars..g. <- as.numeric(data$Sugars..g.)
data$Caffeine..mg. <- as.numeric(data$Caffeine..mg.)

# Remove first 3 columns

data_num <- data[, -c(1:3)]

y <- data_num$Calories

# Remove calories column

data_num <- data_num[, -1]

# Regression model

model <- lm(y ~ ., data = data_num)
summary(model)
plot(model)

# Remove outliers

data_num <- data_num[!is.na(y),]
y <- y[!is.na(y)]

model <- lm(y ~ ., data = data_num)
summary(model)


backward <- ols_step_backward_p(model)
backward$
summary(backward$model)
mod_backward <- backward$model
plot(mod_backward)

