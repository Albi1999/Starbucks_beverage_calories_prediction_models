 model <- lm( Audience.spending ~ Entries + Number.of.shows , data=total_month)
 summary(model)

#date as dummies variable 12
 model1 <- lm(  Entries  ~ Audience.spending  + Number.of.shows , data=total_month)
 summary(model1)

 setwd("C:\\Users\\Greta\\Desktop\\students")
 students <- read.csv("study_performance.csv") 
 head(students)
 
 
 students$test_preparation_course_binary <- ifelse(students$test_preparation_course == "none", 0, 1)
 head(students)
 
 students$gender_binary <- ifelse(students$gender == "male", 0, 1)
 head(students)
 
 students$lunch_binary <- ifelse(students$lunch == "standard", 0, 1)
 head(students)
   
 model1 <- lm(math_score ~ test_preparation_course_binary + reading_score + writing_score + gender_binary + lunch_binary, data=students)
 summary(model1)
 
 
 vars <- c("math_score", "test_preparation_course_binary", "reading_score", "writing_score", "gender_binary", "lunch_binary")
 cor_matrix <- cor(students[vars])
 
 
 
 