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
table(data$Beverage_prep)
table(data$Beverage)

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

# We have some NA values in the data
# The NA values are in the Caffeine..mg. column
# We will remove the rows with NA values
data_cleaned <- data[!is.na(data$Caffeine..mg.),]

# rename the cleaned data columns by removing the dots and the unity of measure

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
corrplot(correlation_matrix, method = "number", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

## Heatmap of the correlation matrix ----
heatmap(cor(data_num), 
        col = colorRampPalette(c("#005cff", "#fbfbfb", "#d90000"))(100), 
        symm = TRUE, 
        margins = c(10, 10), 
        cexRow = 1.4,
        cexCol = 1.4)

# Data Visualization ----

## Histograms ----

# Histogram of the data with density distribution
par(mfrow = c(5, 3))
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

# Split the data into 3 groups in order to have a better visualization
data_num1 <- data_num[, 1:5]
data_num2 <- data_num[, 6:10]
data_num3 <- data_num[, 11:15]
pairs(data_num1, 
      diag.panel = panel.hist,
      upper.panel = panel.cor, 
      lower.panel = panel.smooth,
      colour = "#4ea5ff")

pairs(data_num2, 
      diag.panel = panel.hist,
      upper.panel = panel.cor,
      lower.panel = panel.smooth,
      colour = "#00cd5c")

pairs(data_num3, 
      diag.panel = panel.hist,
      upper.panel = panel.cor,
      lower.panel = panel.smooth,
      colour = "#ff810f")


## Barplot ----
# Barplot of the data
par(mfrow = c(5, 3))
for (i in 1:ncol(data_num)) {
  barplot(table(data_num[, i]), main = colnames(data_num)[i],
          xlab = colnames(data_num)[i], col = col[i])
}

## Boxplot ----
# Boxplot of the data
par(mfrow = c(5, 3))
for (i in 1:ncol(data_num)) {
  boxplot(data_num[, i], main = colnames(data_num)[i],
          xlab = colnames(data_num)[i], col = col[i])
}







# Regression model -----

# Remove first 3 columns for the regression models
data_num <- data[, -c(1:3)]

# Set the first column as the dependent variable
y <- data_num$Calories

# Remove calories column
data_num <- data_num[, -1]

model <- lm(y ~ ., data = data_num)
summary(model)
plot(model)

AIC(model)

# Remove outliers

data_num <- data_num[!is.na(y),]
y <- y[!is.na(y)]

model <- lm(y ~ ., data = data_num)
summary(model)
plot(model)

backward <- ols_step_backward_p(model)
backward$
summary(backward$model)
mod_backward <- backward$model
plot(mod_backward)

