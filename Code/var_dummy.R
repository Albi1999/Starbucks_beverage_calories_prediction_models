# Load the libraries ----
library("corrplot")

# Load data ----
students <- read.csv("Data/study_performance.csv", sep = ",", header = TRUE)
head(students)
summary(students)
structure(students)
dim(students)

#create a copy of the original dataset 

stud <- students

#Manage the non-numeric variables as categoric by using the one-hot encoding 


# Convert non-numeric variables to factors
stud$gender <- as.factor(stud$gender)
stud$race_ethnicity <- as.factor(stud$race_ethnicity)
stud$parental_level_of_education <- as.factor(stud$parental_level_of_education)
stud$lunch <- as.factor(stud$lunch)
stud$test_preparation_course <- as.factor(stud$test_preparation_course)

# Perform one-hot encoding for each categorical variable separately
gender_dummy <- model.matrix(~ gender - 1, data = stud)
race_dummy <- model.matrix(~ race_ethnicity - 1, data = stud)
education_dummy <- model.matrix(~ parental_level_of_education - 1, data = stud)
lunch_dummy <- model.matrix(~ lunch - 1, data = stud)
test_prep_dummy <- model.matrix(~ test_preparation_course - 1, data = stud)

# Combine all dummy variables into one data frame
stud_dummy <- cbind(gender_dummy, race_dummy, education_dummy, lunch_dummy, test_prep_dummy)

# Now you have encoded variables in your dataset with dummy variables

# Rimuovi le colonne originali dal dataset stud
stud <- subset(stud, select = -c(gender, race_ethnicity, parental_level_of_education, lunch, test_preparation_course))

# Aggiungi le nuove colonne dummy al dataset stud
stud <- cbind(stud, gender_dummy, race_dummy, education_dummy, lunch_dummy, test_prep_dummy)

# Ora il dataset stud contiene le nuove variabili dummy e le variabili numeriche originali, mantenendo la stessa dimensione

#To summaize: 

#Original Problem:
#The original problem was about handling categorical variables in a dataset named 'stud'. 
#The dataset contained non-numeric variables such as gender, race/ethnicity, parental level of education, lunch, and test preparation course. 
#The goal was to convert these categorical variables into numeric format, specifically into dummy variables, to use them in correlation matrix and regression analysis.

#Solution:
#To solve this problem, we followed these steps:
  
#1.We converted each non-numeric variable into a factor using the as.factor() function in R. This step ensures that R treats these variables as categorical.
#2.We performed one-hot encoding for each categorical variable separately using the model.matrix() function. This function converts a factor variable into a set of binary dummy variables.
#3.We combined all the dummy variables into one dataframe using the cbind() function.
#4.Finally, we replaced the original categorical variables in the 'stud' dataset with the new dummy variables while maintaining the same dimensions of the dataset.

#Result:
#Now, in the updated 'stud' dataset, all the original categorical variables have been replaced with new dummy variables. 
#The dataset contains both numeric variables and these new dummy variables, allowing us to use it for correlation matrix and regression analysis.

#Analysis:
#Yes, it is correct to perform correlation matrix and regression analysis on this new dataset. The inclusion of dummy variables allows us to capture the effects of categorical variables in these analyses. 
#You can now proceed with your correlation and regression analyses confidently.

#Analisi delle correlazioni -----> between the numeric variable only 
# Seleziona solo le variabili numeriche dal dataset stud

# Seleziona solo le tre variabili numeriche originali dal dataset stud
stud_numeric_original <- stud[, c("math_score", "reading_score", "writing_score")]

# Calcola la matrice di correlazione
correlation_matrix <- cor(stud_numeric_original)

# Visualizza la matrice di correlazione utilizzando un heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        symm = TRUE, 
        margins = c(10, 10))


# Visualizza la matrice di correlazione utilizzando corrplot
corrplot(correlation_matrix, method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")


#Analisi delle correlazioni con variabili dummy ritenute significative
#Unisci la variabile dummy 'gender' al dataframe 'stud_numeric_original'
stud_numeric_original1 <- cbind(stud_numeric_original, gender_dummy)

# Calcola la matrice di correlazione tra le variabili numeriche e la variabile dummy 'gender'
correlation_matrix1 <- cor(stud_numeric_original1)

# Visualizza la matrice di correlazione utilizzando corrplot con i coefficienti di correlazione
corrplot(correlation_matrix1, method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")


#Unisci la variabile dummy 'education' al dataframe 'stud_numeric_original'

stud_numeric_original2 <- cbind(stud_numeric_original, education_dummy)
# Calcola la matrice di correlazione tra le variabili numeriche e la variabile dummy 'education'
correlation_matrix2 <- cor(stud_numeric_original2)
# Visualizza la matrice di correlazione utilizzando corrplot con i coefficienti di correlazione
corrplot(correlation_matrix2, method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")


#Unisci la variabile dummy 'race' al dataframe 'stud_numeric_original'

stud_numeric_original3 <- cbind(stud_numeric_original, race_dummy)
# Calcola la matrice di correlazione tra le variabili numeriche e la variabile dummy 'education'
correlation_matrix3 <- cor(stud_numeric_original3)
# Visualizza la matrice di correlazione utilizzando corrplot con i coefficienti di correlazione
corrplot(correlation_matrix3, method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")

#Unisci la variabile dummy 'lunch' al dataframe 'stud_numeric_original'

stud_numeric_original4 <- cbind(stud_numeric_original, lunch_dummy)
# Calcola la matrice di correlazione tra le variabili numeriche e la variabile dummy 'education'
correlation_matrix4 <- cor(stud_numeric_original4)
# Visualizza la matrice di correlazione utilizzando corrplot con i coefficienti di correlazione
corrplot(correlation_matrix4, method = "number", tl.col = "black", tl.srt = 45, addCoef.col = "black")


# New variable general_score ----
# We create a new target variable general_score that is the mean of the three scores
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

stud$pass_exam <- ifelse(stud$general_score  < 60, 0, 1)
head(students)
stud$pass_exam
table(stud$pass_exam)

#work on students dataset
scores <- c("math_score", "reading_score", "writing_score")
students$total_score <- (students$math_score + students$reading_score + students$writing_score) / length(scores)

# Carica la libreria GGally
library(GGally)

# Seleziona le variabili di interesse e la variabile di suddivisione (genere)
data_subset <- students[, c("math_score", "reading_score", "writing_score", "total_score", "gender")]

# Crea lo scatterplot matrix diviso per genere
scatterplot_matrix <- ggpairs(data = data_subset, 
                              columns = c("math_score", "reading_score", "writing_score", "total_score"),
                              mapping = aes(color = gender),
                              upper = list(continuous = wrap("cor", size = 3)),
                              lower = list(continuous = "points"),
                              title = "Scatterplot Matrix by Gender")

# Visualizza lo scatterplot matrix
print(scatterplot_matrix)


# Definizione della funzione per l'istogramma
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
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Definizione della funzione per la correlazione
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

# Seleziona le variabili numeriche di interesse
numerical_vars <- c("math_score", "reading_score", "writing_score", "total_score")

# Disegna lo scatterplot matrix
pairs(students[, numerical_vars], 
      diag.panel = panel.hist,    # Usa panel.hist per gli istogrammi sulla diagonale
      upper.panel = panel.cor,    # Usa panel.cor per le correlazioni sopra la diagonale
      lower.panel = panel.smooth, # Usa panel.smooth per la regressione liscia sotto la diagonale
      labels = c("Math", "Reading", "Writing", "Total")) # Aggiunge i label alle variabili

#stessa cosa ma by gender 

# Funzione per disegnare gli istogrammi
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
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Funzione per la correlazione
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

# Seleziona le variabili numeriche di interesse
numerical_vars <- c("math_score", "reading_score", "writing_score", "total_score")

# Crea lo scatterplot matrix con distinzione per genere
pairs(students[, numerical_vars], 
      col = ifelse(students$gender == "female", "blue", "red"), # Usa il colore blu per le femmine e rosso per i maschi
      pch = 16, # Imposta il tipo di punto
      labels = c("Math", "Reading", "Writing", "Total"), # Aggiunge i label alle variabili
      main = "Scatterplot Matrix") # Titolo principale

# Aggiungi la legenda
legend("topright", legend = c("Female", "Male"), 
       col = c("blue", "red"), pch = 16, 
       title = "Gender", cex = 1.0, bg = "transparent")

#Some other data visualization

# Barplot del punteggio totale per genere
# Imposta la dimensione della figura
par(mfrow = c(3, 2), mar = c(5, 5, 4, 2))

# Barplot del punteggio totale per genere
barplot(table(students$gender, stud$pass_exam), 
        main = "Passed exam by Gender",
        xlab = "Gender", ylab = "Frequency", col = c("#86ddf7", "#ffa7c5"),
        legend = rownames(table(students$gender, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

# Barplot del punteggio totale per etnia
barplot(table(students$race_ethnicity, stud$pass_exam), 
        main = "Passed exam by Race/Ethnicity",
        xlab = "Race/Ethnicity", ylab = "Frequency", col = c('#ff0000', '#ff7100', '#ffa600', '#ffd400', '#ffff00'),
        legend = rownames(table(students$race_ethnicity, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

# Barplot del punteggio totale per livello di istruzione dei genitori
barplot(table(students$parental_level_of_education, stud$pass_exam), 
        main = "Passed exam by Parental Education Level",
        xlab = "Parental Education Level", ylab = "Frequency", las = 2, col = c('#003200', '#005800', '#038202', '#45aa34', '#71d45c', '#a0ff87'),
        legend = rownames(table(students$parental_level_of_education, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))


# Barplot del punteggio totale per tipo di pranzo
barplot(table(students$lunch, stud$pass_exam), 
        main = "Passed exam by Lunch",
        xlab = "Lunch", ylab = "Frequency", col = c('#d90000', '#006cff'),
        legend = rownames(table(students$lunch, stud$pass_exam)),
        beside = TRUE, axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

# Barplot del punteggio totale per completamento del corso di preparazione al test
barplot(table(students$test_preparation_course, stud$pass_exam), 
        main = "Passed exam by Test\nPreparation Course",
        xlab = "Test Preparation Course", ylab = "Frequency",
        legend = rownames(table(students$test_preparation_course, stud$pass_exam)),
        beside = TRUE, col = c('#800080', '#ffa500'), axisnames = TRUE, args.legend = list(x = "topleft", cex = 0.7), names.arg = c("Failed", "Passed"))

# Resetta le impostazioni di par
par(mfrow = c(1, 1))


