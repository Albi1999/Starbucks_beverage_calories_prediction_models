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

#we have some missing values in the variable caffeine 
is.na(data$Caffeine..mg.)
#different way to impute NA ----> median is suitable choice  because we look into structure of the variable
summary(data$Caffeine..mg.)

#check 
unique(data$Total.Fat..g.)
summary(data$Total.Fat..g.)

#create a copy of original dataset 

data_cleaned <- data

#Presence of Outliers: Looking at the summary statistics provided, the presence of outliers is indicated by the large difference between the mean (89.52 mg.) and the median (75 mg.). 
#Outliers can skew the mean, making it less representative of the central tendency of the data.
#The median, being resistant to outliers, provides a better measure of central tendency in this scenario.
#Possibly non-normal distribution 
#With 23 missing values out of 242 observations, the dataset has a relatively small proportion of missing values (approximately 9.5%). 

# calculate the median fo the  'Caffeine..mg.', without NA
median_caffeine <- median(data_cleaned$Caffeine..mg., na.rm = TRUE)

# Impute NA with median
data_cleaned$Caffeine..mg.[is.na(data_cleaned$Caffeine..mg.)] <- median_caffeine

# no more NA ---> now we can work directly on cleaned dataset 
summary(data_cleaned$Caffeine..mg.)
is.na(data_cleaned$Caffeine..mg.)


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

# frequency for 'Beverage_category' ----> how we can see the most famous bevarage?
beverage_counts <- table(data$Beverage_category)

# create a barplot for the variable 
barplot(beverage_counts,
        main = "Distribution of Beverage Categories",
        xlab = "Beverage Category",
        ylab = "Count",
        col = "skyblue",
        las = 2, 
        cex.names = 0.8)


# frequency for 'Beverage_prep'
beverage_prep_counts <- table(data$Beverage_prep)

#barplot 
barplot(beverage_prep_counts,
        main = "Distribution of Beverage Preparation",
        xlab = "Preparation",
        ylab = "Count",
        col = "skyblue",
        las = 2, 
        cex.names = 0.8)

#we want to obtain a barplot in which we can see the amount of total calories for each categories of bevarage 

# Aggrega i dati per ottenere il totale delle calorie per ogni categoria di bevanda
total_calories_by_category <- aggregate(Calories ~ Beverage_category, data = data_cleaned, sum)

# Crea il barplot utilizzando le funzioni base di R
barplot(height = total_calories_by_category$Calories,
        names.arg = total_calories_by_category$Beverage_category,
        main = "Total Calories by Beverage Category",
        xlab = "Beverage Category",
        ylab = "Total Calories",
        col = "skyblue",
        las = 2, # Ruota le etichette dell'asse x per essere verticali
        cex.names = 0.8) # Dimensione delle etichette dell'asse x

#same to compare bevarage preparation and sugar 

total_sugar_by_prep <- aggregate(Total_Carbohydrates ~ Beverage_prep, data = data_cleaned, sum)

barplot(height = total_sugar_by_prep$Total_Carbohydrates,
        names.arg = total_sugar_by_prep$Beverage_prep,
        main = "Total Sugars by Beverage Preparation",
        xlab = "Beverage Preparation",
        ylab = "Total Sugars (g)",
        col = "skyblue",
        las = 2, # Ruota le etichette dell'asse x per essere verticali
        cex.names = 0.8) # Dimensione delle etichette dell'asse x

#other data visualization 

# variable Beverage_category factor
data_cleaned$Beverage_category <- as.factor(data_cleaned$Beverage_category)

# Assign distinct colors to each beverage category
colors <- rainbow(length(unique(data_cleaned$Beverage_category)))
color_map <- setNames(colors, levels(data_cleaned$Beverage_category))

# create a scatterplot to compare amounts of calories and fat for each categories of bevarage 
plot(data_cleaned$Calories, 
     data_cleaned$Total_Fat_g,
     col = color_map[data_cleaned$Beverage_category],
     pch = 19, # Tipo di punto
     xlab = "Calories",
     ylab = "Total Fat (g)",
     main = "Calories vs Total Fat")

#legend
legend("topright", 
       legend = levels(data_cleaned$Beverage_category), 
       col = colors, 
       pch = 19)

#comparision between total fat and trans fat ( che cazzo sono?)

#numeric variable -> calculate density
total_fat_density <- density(data_cleaned$Total_Fat)
trans_fat_density <- density(data_cleaned$Trans_Fat)

# create a grafico sovrapposto 
plot(total_fat_density, col = "skyblue", main = "Comparison of Total Fat and Trans Fat Distributions", 
     xlab = "Fat Content (g)", ylab = "Density", ylim = c(0, max(total_fat_density$y, trans_fat_density$y)),
     xlim = range(data_cleaned$Total_Fat, data_cleaned$Trans_Fat), 
     lwd = 2, lty = 1)
lines(trans_fat_density, col = "orange", lwd = 2, lty = 1)
legend("topright", legend = c("Total Fat", "Trans Fat"), col = c("skyblue", "orange"), lwd = 2, lty = 1)

#create scatterplot to look into relantionship between calories and other variables 

str(data_cleaned)

par(mfrow = c(2, 2))  # Imposta il layout dei grafici a griglia 2x2
with(data_cleaned, {
  plot(Calories, Sodium , main = "Relation between Calories and Sodium", xlab = "Calories", ylab = "Sodium (mg)")
  plot(Calories, Protein , main = "Relation between Calories and Protein", xlab = "Calories", ylab = "Protein (g)")
  plot(Calories, Vitamin_C , main = "Relation between Calories and Vitamin C", xlab = "Calories", ylab = "Vitamin C (mg)")
  plot(Calories, Cholesterol , main = "Relation between Calories and Fiber", xlab = "Calories", ylab = "Fiber (g)")
})

#There's increase in every feature with increase in calories
#features like proteins and fiber rapidly increase, instead vitamin and cholesterol more flat growing 
#confirm by correlation coefficients 