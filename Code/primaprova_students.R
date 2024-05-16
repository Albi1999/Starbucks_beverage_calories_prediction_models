#import and summarize data 
students <- read.csv("Data/study_performance.csv") 
head(students)
summary(students)
structure(students)
dim(students)

#check if we have NA 
is.na(students)
is.na(students$parental_level_of_education)
#we have some NA fot this variable 
#essendo una variabile non numerica o comunque categorica possiamo sostituire i valori mancanti con la moda
#cioè il valore più frequente nel dataset

# Calcola la moda della variabile parental_level_of_education
mode_value <- names(sort(table(students$parental_level_of_education), decreasing = TRUE))[1]

# Sostituisci i valori mancanti con la moda
students$parental_level_of_education[is.na(students$parental_level_of_education)] <- mode_value

# Aggiorna la variabile categorica
students$parent_education_category_compl <- as.integer(factor(students$parental_level_of_education, levels = c("bachelor's degree", "some college", "master's degree", "associate's degree", "high school")))
students$parent_education_category_compl
# Visualizzazione delle prime righe del dataframe
head(students)
#no more NA

#goal: To understand the influence of the parent's background, test preparation etc on students' performance
#we use math score as the target variable we want to predict or we can do the mean bethween  the 3 scores and use it as perfomance indicatore

#work now on students dataset, we want to turn the characater into categorical variable by doing the encoding
students$test_preparation_course_binary <- ifelse(students$test_preparation_course == "none", 0, 1)
head(students)

students$gender_binary <- ifelse(students$gender == "male", 0, 1)
head(students)


students$lunch_binary <- ifelse(students$lunch == "standard", 0, 1)
head(students)

# Creazione di una variabile binaria per l'etnia
students$ethnicity_A <- ifelse(students$race_ethnicity == "Group A", 1, 0)
students$ethnicity_B <- ifelse(students$race_ethnicity == "Group B", 1, 0)
students$ethnicity_C <- ifelse(students$race_ethnicity == "Group C", 1, 0)
students$ethnicity_D <- ifelse(students$race_ethnicity == "Group D", 1, 0)
students$ethnicity_E <- ifelse(students$race_ethnicity == "Group E", 1, 0)
head(students)

unique(students$race_ethnicity)
#senza il one hot encoding - In questo modo, stiamo specificando i livelli corretti per la variabile race_ethnicity, e poi convertendo questa variabile in una variabile numerica, dove "group B" corrisponde a 1, "group C" a 2 e così via. 
#Abbiamo sottratto 1 per far sì che i valori inizino da 0 come richiesto

# Aggiornamento dei livelli della variabile race_ethnicity
students$race_ethnicity <- factor(students$race_ethnicity, levels = c("group B", "group C", "group A", "group D", "group E"))

# Creazione della variabile categorica per l'etnia
students$ethnicity_category <- as.integer(students$race_ethnicity) - 1

# Visualizzazione delle prime righe del dataframe
head(students)
students$ethnicity_category

#we do the same for the variable parents level of education: 

# Aggiornamento dei livelli della variabile parental_level_of_education
students$parental_level_of_education <- factor(students$parental_level_of_education, levels = c("bachelor's degree", "some college", "master's degree", "associate's degree", "high school"))

# Creazione della variabile categorica per il livello di istruzione dei genitori
students$parent_education_category <- as.integer(students$parental_level_of_education)
students$parent_education_category
# Visualizzazione delle prime righe del dataframe
head(students)


# Calcolo del generale_score come nuova variable che risulta essere la media dei tre diversi score
students$generale_score <- (students$math_score + students$reading_score + students$writing_score) / 3
students$generale_score
# Visualizzazione delle prime righe del dataframe con la nuova variabile
head(students)

#calcolo la mediana della nuova variabile gnerale_score
mediana = median(students$generale_score)
mediana

#costruisco una nuova variabile binaria pass_exam, che classifica come 0 tutti gli studenti che hanno ottenuto un general
#score minore di 68.3 cioè della mediana e 1 altrimenti 


students$pass_exam <- ifelse(students$generale_score < mediana, 0, 1)
head(students)
students$pass_exam 


students$pass_exam1 <- ifelse(students$generale_score < 60, 0, 1)
head(students)
students$pass_exam1

cbind(students$pass_exam, students$pass_exam1)

#data visualization and EDA 

#regressioni a cazzo 
model1 <- lm(math_score ~ test_preparation_course_binary + reading_score + writing_score + gender_binary + lunch_binary, data=students)
summary(model1)


vars <- c("math_score", "test_preparation_course_binary", "reading_score", "writing_score", "gender_binary", "lunch_binary")
cor_matrix <- cor(students[vars])


