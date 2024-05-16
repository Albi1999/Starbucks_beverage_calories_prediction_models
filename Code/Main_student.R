# Statistical Learning Project ----

# Load the libraries ----
library("corrplot")

# Load data ----
students <- read.csv("Data/study_performance.csv", sep = ",", header = TRUE) 
head(students)

# Transform the data ----
n <- nrow(students)

stud <- students

# Transform the test_preparation_course variable into a binary variable
stud$test_preparation_course <- ifelse(students$test_preparation_course == "none", 0, 1)
# plot the frequency table
table(stud$test_preparation_course)

# Transform the gender variable into a binary variable
stud$gender <- ifelse(students$gender == "male", 0, 1)
# plot the frequency table
table(stud$gender)

# Transform the lunch variable into a binary variable
stud$lunch <- ifelse(students$lunch == "standard", 0, 1)
# plot the frequency table
table(stud$lunch)

# Transform the race etnicity variable into a categorical variable
stud$race_ethnicity <- as.factor(students$race)
# plot the frequency table
table(stud$race_ethnicity)

# Transform the parental level of education variable into a categorical variable
stud$parental_level_of_education <- as.factor(students$parental_level_of_education)
# plot the frequency table
table(stud$parental_level_of_education)

# Transform the other variables into numerical variables
stud$math_score <- as.numeric(students$math_score)
stud$reading_score <- as.numeric(students$reading_score)
stud$writing_score <- as.numeric(students$writing_score)

# Check the data ----
head(stud)

# Plot the data ----
par(mfrow=c(1,3))
hist(stud$math_score, main = "Math Score", xlab = "Score", col = "#d90000")
hist(stud$reading_score, main = "Reading Score", xlab = "Score", col = "#005cff")
hist(stud$writing_score, main = "Writing Score", xlab = "Score", col = "#52b640")

# Correlation matrix ----
numerical_vars <- c("math_score", "test_preparation_course", "reading_score", "writing_score", "gender", "lunch")
cor_matrix_2 <- cor(stud[numerical_vars])
corrplot(cor_matrix_2, method = "number", order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Boxplot the math score with other variables ----
par(mfrow=c(1,1))
boxplot(stud$math_score ~ stud$test_preparation_course, main = "Math Score by Test Preparation Course", xlab = "Test Preparation Course", ylab = "Math Score", col = "#005cff")
boxplot(stud$math_score ~ stud$parental_level_of_education, main = "Math Score by Parental level of education", xlab = "Parental level of education", ylab = "Math Score", col = "#52b640")
boxplot(stud$math_score ~ stud$race_ethnicity, main = "Math Score by Ethnicity", xlab = "Ethnicity", ylab = "Math Score", col = "#d90000")
boxplot(stud$math_score ~ stud$gender, main = "Math Score by gender", xlab = "Gender", ylab = "Math Score", col = c("#86ddf7", "#ffa7c5"))
boxplot(stud$math_score ~ stud$lunch, main = "Math Score by lunch", xlab = "Lunch", ylab = "Math Score", col = "#ff9800")


# FUNCTION "pairs" for matrix plot ----
# Let's try with the pairs function, it seems to be a good graph




# It doesn’t work already 




pairs(stud)

pairs(stud, diag.panel=panel.hist)

# define the functions "panel.hist" and "panel.cor"
# before using the following two commands. 
# (the two functions can be found below but they are 
# provided in the help of the function "pairs")



## panel.hist function
## puts histograms on the diagonal

panel.hist <- function(X, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## panel.cor function
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
str(stud[numerical_vars])
pairs(stud[numerical_vars], diag.panel=panel.hist, upper.panel=panel.cor)
pairs(stud[numerical_vars], diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)
