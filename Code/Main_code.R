# Statistical Learning Project ----

# Load the libraries ----
library("corrplot")

# Load data ----
students <- read.csv("Data/study_performance.csv", sep = ",", header = TRUE)
head(students)

# Overview of the data ---- DESCRIBE DATA 
summary(students)
structure(students)
dim(students)

# This project understands how the student's performance (test scores) is affected by other variables such 
# as Gender, Ethnicity, Parental level of education, Lunch and Test preparation course.
# GOAL:
# To understand the influence of the parent's background,
# test preparation etc on students' performance we use mean score as the target 
# variable we want to predict or we can do the mean between
# the 3 scores and use it as performance indicator

# create a copy of the original dataset 
stud <- students

# Transform the data ----
# Manage the non-numeric variables as categoric by using the one-hot encoding 

# Convert non-numeric variables to factors
stud$gender <- as.factor(stud$gender)
table(stud$gender)
stud$race_ethnicity <- as.factor(stud$race_ethnicity)
table(stud$race_ethnicity)
stud$parental_level_of_education <- as.factor(stud$parental_level_of_education)
table(stud$parental_level_of_education)
stud$lunch <- as.factor(stud$lunch)
table(stud$lunch)
stud$test_preparation_course <- as.factor(stud$test_preparation_course)
table(stud$test_preparation_course)

# Perform one-hot encoding for each categorical variable separately 
gender_dummy <- model.matrix(~ gender - 1, data = stud)
race_dummy <- model.matrix(~ race_ethnicity - 1, data = stud)
education_dummy <- model.matrix(~ parental_level_of_education - 1, data = stud)
lunch_dummy <- model.matrix(~ lunch - 1, data = stud)
test_prep_dummy <- model.matrix(~ test_preparation_course - 1, data = stud)

# Combine all dummy variables into one new data frame that we called stud_dummy, which contains ONLY DUMMY VARIABLES
stud__just_dummy <- cbind(gender_dummy, race_dummy, education_dummy, lunch_dummy, test_prep_dummy)

# Now you have encoded variables in your dataset with dummy variables

#crate a new copy of the dataset stud, in which we have the original numeric variable and we replace the original one with the new variable dummy
stud_dummy <- stud

# Remove the original non categorical variable from the dataset stud_dummy
stud_dummy <- subset(stud_dummy, select = -c(gender, race_ethnicity, parental_level_of_education, lunch, test_preparation_course))

# Add the new columns to the dataset stud_dummy
stud_dummy <- cbind(stud_dummy, gender_dummy, race_dummy, education_dummy, lunch_dummy, test_prep_dummy)

# Now the dataset stud_dummy contains all new variables dummy 
# and the numeric variable, by keeping the same size

# To summaize: 

# Original Problem:
# The original problem was about handling categorical variables in a dataset named 'stud'. 
# The dataset contained non-numeric variables such as gender, race/ethnicity, parental level of education, lunch, and test preparation course. 
# The goal was to convert these categorical variables into numeric format, specifically into dummy variables, to use them in correlation matrix and regression analysis.

# Solution:
# To solve this problem, we followed these steps:

# 1.We converted each non-numeric variable into a factor using the as.factor() function in R. This step ensures that R treats these variables as categorical.
# 2.We performed one-hot encoding for each categorical variable separately using the model.matrix() function. This function converts a factor variable into a set of binary dummy variables.
# 3.We combined all the dummy variables into one dataframe using the cbind() function.
# 4.Finally, we replaced the original categorical variables in the 'stud_dummy' dataset with the new dummy variables while maintaining the same dimensions of the dataset.

# Result:
# Now, in the updated 'stud_dummy' dataset, all the original categorical variables have been replaced with new dummy variables. 
# The dataset contains both numeric variables and these new dummy variables, allowing us to use it for correlation matrix and regression analysis.

# Analysis:
# Yes, it is correct to perform correlation matrix and regression analysis on this new dataset. The inclusion of dummy variables allows us to capture the effects of categorical variables in these analyses. 
# You can now proceed with your correlation and regression analyses confidently.

# Correlations Analysis -----
## Numeric variables Corr ----
# Correlations Analysis between the numeric variable only 
# Select only the numeric variables from the dataset stud
# Work now on the dataset stud_dummy

stud_numeric_original <- stud_dummy[, c("math_score", "reading_score", "writing_score")]

# Correlation matrix
# Displays the correlation matrix using a heatmap
heatmap(cor(stud_numeric_original), 
        col = colorRampPalette(c("#005cff", "#fbfbfb", "#d90000"))(100), 
        symm = TRUE, 
        margins = c(10, 10), 
        cexRow = 1.4,
        cexCol = 1.4)

# Displays the correlation matrix using a  corrplot
corrplot(cor(stud_numeric_original), method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")
# we notice that there is a strong positive correlation between all the 3 score
# that means the increase of, an average, score also increase the other

## Dummy variables Corr ----
# Correlation analysis with dummy variables deemed significant.
# We want to see if and how scores changes within other variables. 
# We're looking for linear relantioships 

### Gender Corr ----

# Calculate the correlation matrix between the numeric variables and the dummy 'gender' variable
# Displays the correlation matrix using a  corrplot
corrplot(cor(cbind(stud_numeric_original, gender_dummy)), method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")
# Negative correlation (-0.17) indicates that there is an inverse relationship between gender and math scores.
# That siggests us that there in general a trend for math score to be sliglty worse for female compared to male

### Education Corr ----
# Calculate the correlation matrix between the numeric variables and the dummy 'education'
# Displays the correlation matrix using a  corrplot
corrplot(cor(cbind(stud_numeric_original, education_dummy)), method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")
# There is no significant correlations. 
# That means that the level of education of the parents does not affect the scores of the students

### Race Corr ----
# Calculate the correlation matrix between the numeric variables and the dummy 'race'
# Displays the correlation matrix using a  corrplot
corrplot(cor(cbind(stud_numeric_original, race_dummy)), method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")
# Positive correlation between math score and group E

### Lounch Corr ----
## Calculate the correlation matrix between the numeric variables and the dummy 'lunch'
# Displays the correlation matrix using a  corrplot
corrplot(cor(cbind(stud_numeric_original, lunch_dummy)), method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")
#In general perform slighlty better how has a standard meal

# New variable general_score ----
# We create a new target variable general_score that is the mean of the three scores
# We can use this variable as a performance indicator
scores <- c("math_score", "reading_score", "writing_score")
stud$general_score <- (stud$math_score + stud$reading_score + stud$writing_score) / length(scores)

y <- stud$general_score
par(mfrow=c(1,1))
hist(y, main = "General Score", xlab = "Score", col = "#ff9800")
abline(v = mean(y), col = "#005cff", lwd = 3)
text(mean(y), 50, "Mean", col = "#005cff", pos = 2)
# Most of the students get a general score between 60 and 80 out of 100
# Scores are expressed in centesimal


# New binary variable pass_exam ----
# We create a new binary variable pass_exam that classifies as 0 all students 
# who obtained a general score less than 60 so the fail the exam 
# and as 1 all students who obtained a general score greater than or equal to 60, so they pass the exam

stud$pass_exam <- ifelse(stud$general_score  < 60, 0, 1)
head(students)
stud$pass_exam
table(stud$pass_exam)

# Data Visualization ----

## Histograms ----
par(mfrow=c(1,3))
hist(stud$math_score, main = "Math Score", xlab = "Score", col = "#d90000")
abline(v = mean(stud$math_score), col = "black", lwd = 3)
text(mean(stud$math_score), 50, "Mean", col = "black", pos = 2)
hist(stud$reading_score, main = "Reading Score", xlab = "Score", col = "#005cff")
abline(v = mean(stud$reading_score), col = "black", lwd = 3)
text(mean(stud$reading_score), 50, "Mean", col = "black", pos = 2)
hist(stud$writing_score, main = "Writing Score", xlab = "Score", col = "#52b640")
abline(v = mean(stud$writing_score), col = "black", lwd = 3)
text(mean(stud$writing_score), 50, "Mean", col = "black", pos = 2)

# Define function histogram 
panel.hist <- function(x, ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "#005cff", ...)
}

# Define function for correlations 
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

# Selection numerical variables
numerical_vars <- c("math_score", "reading_score", "writing_score", "general_score")

# Scatterplot matrix ----
pairs(stud[, numerical_vars], 
      diag.panel = panel.hist,    # Use panel.hist for histograms on the diagonal
      upper.panel = panel.cor,    # Use panel.cor for correlations above the diagonal
      lower.panel = panel.smooth, # Use panel.smooth for the regression below the diagonal
      labels = c("Math", "Reading", "Writing", "Total"))

# From the plot we can confirm that there is high correlations between the scores, also with total scores
# since it results as the mean of other scores
# regression lines fit very well the cloud of point 
# distributions approximatly normal 

# We want to compare scores by gender 

# hist function
panel.hist <- function(x, ...)
{
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "#005cff", ...)
}

# correlation function 
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

## Scatterplot matrix by gender ----
pairs(stud[, numerical_vars], 
      col = ifelse(stud$gender == "female", "#ffa7c5", "#00c4df"),
      pch = 16, # Imposta il tipo di punto
      labels = c("Math", "Reading", "Writing", "Total"),
      main = "Scatterplot Matrix - Gender")
# ADD COMMENTS 
# Aggiungi la legenda ---- SISTEMA
legend("topleft", legend = c("Female", "Male"), 
       col = c("#ffa7c5", "#00c4df"), pch = 16, 
       title = "Gender", cex = 1.0 , bg = "transparent")

## Barplot ----

# Set Size of the plot 
par(mfrow = c(3, 2), mar = c(5, 5, 4, 2))

### Total score by gender ----
# Barplot of total score by gender
barplot(table(stud$gender, stud$pass_exam), 
        main = "Passed exam by Gender",
        xlab = "Gender", ylab = "Frequency", col = c("#86ddf7", "#ffa7c5"),
        legend = rownames(table(stud$gender, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

### Total score by race ----
# Barplot of total score by race
barplot(table(stud$race_ethnicity, stud$pass_exam), 
        main = "Passed exam by Race/Ethnicity",
        xlab = "Race/Ethnicity", ylab = "Frequency", col = c('#ff0000', '#ff7100', '#ffa600', '#ffd400', '#ffff00'),
        legend = rownames(table(stud$race_ethnicity, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

### Total score by parents level of education ----
# Barplot of total score by parents level of education 
barplot(table(stud$parental_level_of_education, stud$pass_exam), 
        main = "Passed exam by Parental Education Level",
        xlab = "Parental Education Level", ylab = "Frequency", las = 2, col = c('#003200', '#005800', '#038202', '#45aa34', '#71d45c', '#a0ff87'),
        legend = rownames(table(stud$parental_level_of_education, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

### Total score by lunch ----
# Barplot of total score by lunch
barplot(table(stud$lunch, stud$pass_exam), 
        main = "Passed exam by Lunch",
        xlab = "Lunch", ylab = "Frequency", col = c('#d90000', '#006cff'),
        legend = rownames(table(stud$lunch, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

### Total score by pass preparation in the course ----
# Barplot of total score by pass preparation in the course
barplot(table(stud$test_preparation_course, stud$pass_exam), 
        main = "Passed exam by Test\nPreparation Course",
        xlab = "Test Preparation Course", ylab = "Frequency",
        legend = rownames(table(stud$test_preparation_course, stud$pass_exam)),
        beside = TRUE, col = c('#800080', '#ffa500'), axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

# Add comments of all barplot 

## Boxplot ----

### General score with other variables ----
# Boxplot the general score with other variables
par(mfrow=c(2,3))
boxplot(stud$general_score ~ stud$test_preparation_course, main = "General Score by Test Preparation Course", xlab = "Test Preparation Course", ylab = "Score", col = "#005cff")
boxplot(stud$general_score ~ stud$parental_level_of_education, main = "General Score by Parental level of education", xlab = "Parental level of education", ylab = "Score", col = "#52b640")
boxplot(stud$general_score ~ stud$race_ethnicity, main = "General Score by Ethnicity", xlab = "Ethnicity", ylab = "Score", col = "#d90000")
boxplot(stud$general_score ~ stud$gender, main = "General Score by gender", xlab = "Gender", ylab = "Score", col = c("#86ddf7", "#ffa7c5"))
boxplot(stud$general_score ~ stud$lunch, main = "General Score by lunch", xlab = "Lunch", ylab = "Score", col = "#ff9800")
boxplot(stud$general_score ~ stud$pass_exam, main = "General Score by pass_exam", xlab = "Passed Exam", ylab = "Score", col = "#830783")

### Math score with other variables ----
# Boxplot the math score with other variables
par(mfrow=c(2,3))
boxplot(stud$math_score ~ stud$test_preparation_course, main = "Math Score by Test Preparation Course", xlab = "Test Preparation Course", ylab = "Math Score", col = "#005cff")
boxplot(stud$math_score ~ stud$parental_level_of_education, main = "Math Score by Parental level of education", xlab = "Parental level of education", ylab = "Math Score", col = "#52b640")
boxplot(stud$math_score ~ stud$race_ethnicity, main = "Math Score by Ethnicity", xlab = "Ethnicity", ylab = "Math Score", col = "#d90000")
boxplot(stud$math_score ~ stud$gender, main = "Math Score by gender", xlab = "Gender", ylab = "Math Score", col = c("#86ddf7", "#ffa7c5"))
boxplot(stud$math_score ~ stud$lunch, main = "Math Score by lunch", xlab = "Lunch", ylab = "Math Score", col = "#ff9800")

# Regression ----
# Since we have now numerical and dummy variables we're able to fit a regression model 
# our goal is to predict the variable total score with other variables 

# work on stud dataset 

y = stud$general_score
# Regression with all dummy variable. Here we can't use other scores as predictor since our variable is 
# built from combinations od this score 
mod_1 <- lm(y ~ pass_exam + test_preparation_course + parental_level_of_education + lunch + gender + race_ethnicity, data = stud)
summary(mod_1)
AIC(mod_1)
# Math score with other scores
mod_2 <- lm(math_score ~ reading_score + writing_score + pass_exam + test_preparation_course + parental_level_of_education + lunch + gender + race_ethnicity, data = stud)
summary(mod_2)

mod_3 <- lm(y ~ pass_exam + test_preparation_course  + lunch + gender + race_ethnicity, data = stud)
summary(mod_3)

mod_4  <- lm(y ~ pass_exam + test_preparation_course + parental_level_of_education + lunch + gender + race_ethnicity + reading_score + writing_score, data = stud)
summary(mod_4)
# Clearly very high 

# Keep model 1

##  Backward selection ----
# backward: start with full model and keep to drop variables by evaluating the p-value  
# until we end up with a model with all significant variable
# Funzione per eseguire il backward elimination

stud_cleaned <- stud[, -c(6, 7, 8, 9)]

formula <- y ~ gender + race_ethnicity + parental_level_of_education + lunch + test_preparation_course + pass_exam

mod_full <- lm(formula, data = stud_cleaned)
summary(mod_full)
mod_full <- lm(y ~., data = stud_cleaned)
summary(mod_full)
drop1(mod_full, scope = formula, test = "F")

curr_mod <- update(mod_full, y ~. - gender)
drop1(curr_mod, test = "F")

curr_mod <- update(curr_mod, y ~. - race_ethnicity)
drop1(curr_mod, test = "F")
summary(curr_mod)

mod_backward <- curr_mod
summary(mod_backward)
par(mfrow=c(1,1))
plot(mod_backward)

# Forward selection ----
mod_null <- lm(y ~ 1, data = stud_cleaned)
add1(mod_null, scope = formula, test = "F")

mod__1 <- lm(y ~ pass_exam, data = stud_cleaned)
add1(mod__1, scope = formula, test = "F")

mod__2 <- lm(y ~ pass_exam + test_preparation_course, data = stud_cleaned)
add1(mod__2, scope = formula, test = "F")

mod__3 <- lm(y ~ pass_exam + test_preparation_course + lunch, data = stud_cleaned)
add1(mod__3, scope = formula, test = "F")

mod__4 <- lm(y ~ pass_exam + test_preparation_course + lunch + parental_level_of_education, data = stud_cleaned)
summary(mod__4)

mod_forward <- mod__4

# AIC and BIC ----
# AIC and BIC are used to compare models with different number of predictors

# AIC

AIC(mod_forward)
AIC(mod_backward)

BIC(mod_forward)
BIC(mod_backward)




