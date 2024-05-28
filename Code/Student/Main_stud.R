# Statistical Learning Project ----

# Load the libraries ----
library("corrplot")

# Load data ----
students <- read.csv("Data/study_performance.csv", sep = ",", header = TRUE)
head(students)
summary(students)
structure(students)
dim(students)


# GOAL:
# To understand the influence of the parent's background, 
# test preparation etc on students' performance we use mean score as the target 
# variable we want to predict or we can do the mean bethween
# the 3 scores and use it as perfomance indicator


# Transform the data ----
n <- nrow(students)

stud <- students
# We work now on stud dataset, a copy of the student dataset in order to not modify the original one
# we want to turn the characater into categorical variable by encoding them

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
# In questo modo, stiamo specificando i livelli corretti per la variabile race_ethnicity, e poi convertendo questa variabile in una variabile numerica, dove "group B" corrisponde a 1, "group C" a 2 e così via. 
#Abbiamo sottratto 1 per far sì che i valori inizino da 0 come richiesto

stud$race_ethnicity <- as.numeric(factor(students$race_ethnicity, levels = c("group A", "group B", "group C", "group D", "group E")))
# plot the frequency table
table(stud$race_ethnicity)

# Transform the parental level of education variable into a categorical variable
stud$parental_level_of_education <- as.numeric(factor(students$parental_level_of_education, levels = c("bachelor's degree", "some college", "master's degree", "associate's degree", "high school", "some high school")))
# plot the frequency table
table(stud$parental_level_of_education)

# Transform the other variables into numerical variables
stud$math_score <- as.numeric(students$math_score)
stud$reading_score <- as.numeric(students$reading_score)
stud$writing_score <- as.numeric(students$writing_score)

# Check the data ----
head(stud)

# New variable generale_score ----
# We create a new target variable generale_score that is the mean of the three scores
# We can use this variable as a performance indicator
scores <- c("math_score", "reading_score", "writing_score")
stud$general_score <- (stud$math_score + stud$reading_score + stud$writing_score) / length(scores)

y <- stud$general_score
par(mfrow=c(1,1))
hist(y, main = "General Score", xlab = "Score", col = "#ff9800")

# New binary variable pass_exam ----
# We create a new binary variable pass_exam that classifies as 0 all students 
# who obtained a general score less than 60
# and as 1 all students who obtained a general score greater than or equal to 60

stud$pass_exam <- ifelse(stud$general_score < 60, 0, 1)
head(students)
stud$pass_exam
table(stud$pass_exam)

# Plot the data ----
par(mfrow=c(1,3))
hist(stud$math_score, main = "Math Score", xlab = "Score", col = "#d90000")
hist(stud$reading_score, main = "Reading Score", xlab = "Score", col = "#005cff")
hist(stud$writing_score, main = "Writing Score", xlab = "Score", col = "#52b640")


## Boxplot the general score with other variables ----
par(mfrow=c(1,1))
boxplot(stud$general_score ~ stud$test_preparation_course, main = "General Score by Test Preparation Course", xlab = "Test Preparation Course", ylab = "Score", col = "#005cff")
boxplot(stud$general_score ~ stud$parental_level_of_education, main = "General Score by Parental level of education", xlab = "Parental level of education", ylab = "Score", col = "#52b640")
boxplot(stud$general_score ~ stud$race_ethnicity, main = "General Score by Ethnicity", xlab = "Ethnicity", ylab = "Score", col = "#d90000")
boxplot(stud$general_score ~ stud$gender, main = "General Score by gender", xlab = "Gender", ylab = "Score", col = c("#86ddf7", "#ffa7c5"))
boxplot(stud$general_score ~ stud$lunch, main = "General Score by lunch", xlab = "Lunch", ylab = "Score", col = "#ff9800")
boxplot(stud$general_score ~ stud$pass_exam, main = "General Score by pass_exam", xlab = "Passed Exam", ylab = "Score", col = "#830783")

## Boxplot the math score with other variables ----
par(mfrow=c(1,1))
boxplot(stud$math_score ~ stud$test_preparation_course, main = "Math Score by Test Preparation Course", xlab = "Test Preparation Course", ylab = "Math Score", col = "#005cff")
boxplot(stud$math_score ~ stud$parental_level_of_education, main = "Math Score by Parental level of education", xlab = "Parental level of education", ylab = "Math Score", col = "#52b640")
boxplot(stud$math_score ~ stud$race_ethnicity, main = "Math Score by Ethnicity", xlab = "Ethnicity", ylab = "Math Score", col = "#d90000")
boxplot(stud$math_score ~ stud$gender, main = "Math Score by gender", xlab = "Gender", ylab = "Math Score", col = c("#86ddf7", "#ffa7c5"))
boxplot(stud$math_score ~ stud$lunch, main = "Math Score by lunch", xlab = "Lunch", ylab = "Math Score", col = "#ff9800")

## Other plot ----





# Correlation matrix ----
par(mfrow=c(1,1))
variables <- c("math_score", "reading_score", "writing_score", "general_score", "pass_exam", "test_preparation_course", "parental_level_of_education")
cor_matrix <- cor(stud)
corrplot(cor_matrix, method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Regression ----

# We can use the linear regression model to predict the general score of the students
# We defined the general score as the y variable and the other variables as the x variables

mod_1 <- lm(y ~ ., data = stud)
summary(mod_1)
