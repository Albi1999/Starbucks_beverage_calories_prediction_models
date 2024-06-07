# Statistical Learning final project

# Regression Analysis ----
## Linear Regression ----

# This code is dedicated to the regression part (GRETA)

# Linear regression model to predict the amount of calories
# based on the amount of the other variables
# We use the lm() function to fit a linear regression model
# We use the summary() function to display the results
# We use the plot() function to visualize the results

# Fit the linear regression model
#Select only numeric variables from the cleaned dataset in order to implement regression model 

numerical_vars <- data_cleaned[, sapply(data_cleaned, is.numeric)]

#before starting to fit the regression model we want to standardize our numeric variable because they have very different scales.
#and the data's distrubition is unbalenced 

standardized_data <- scale(numerical_vars)

# Visualizza i primi 5 record dei dati standardizzati
head(standardized_data)

#turn into dataframe in order to use into regression
numerical_vars_df <- as.data.frame(standardized_data)

# Fit del modello di regressione utilizzando le variabili standardizzate come predittori
calories_lm_st <- lm(Calories ~ ., data = numerical_vars_df)

# Stampare il summary del modello
summary(calories_lm_st)
AIC(calories_lm_st) #negativo 

#Il valore negativo di AIC (-748.2623) indica che il modello di regressione lineare standardizzato fornisce un miglior
#compromesso tra adattamento ai dati e complessità del modello rispetto al modello di riferimento.

#fit the regression model using amount of calories as target variable and all other numeric as predictors 
#full model using standardized varriables
calories_lm <- lm(Calories ~ ., data = numerical_vars)
summary(calories_lm)

AIC(calories_lm)
BIC(calories_lm)

# The model has a low AIC and BIC values, the R-squared value is 0.99 so the model is a good fit
# The model is significant, the p-value is less than 0.05

#Now we apply the selection of of predictors with backward method (automatic)
calories_lm_backward <- step(calories_lm, direction = "backward")
summary(calories_lm_backward)
AIC(calories_lm_backward)
BIC(calories_lm_backward)

# Stampare le variabili nel modello completo
cat("Variabili nel modello completo:\n")
names(coefficients(calories_lm))

# Stampare le variabili nel modello backward
cat("\nVariabili nel modello backward:\n")
names(coefficients(calories_lm_backward))

#the backward selection drops only the variable "Saturated_Fat"  since it's not  considered significant in explaining 
#he amount of calories 
#mantein all other variables

#compared between models: same R^2 but the values of AIC and BIC are sligthly better in the backward model ---> better 

#forward selection 
calories_lm_forward <- step(calories_lm, direction = "forward")
summary(calories_lm_forward)
AIC(calories_lm_forward)

#still better the backward model 

#stepwise selection 
calories_lm_stepwise<- step(calories_lm, direction = "both")
summary(calories_lm_stepwise)
AIC(calories_lm_stepwise)

#same as bakcward 

final_model <- calories_lm_backward

#Recap della selezione: 

#Modello completo

#AIC: 1494.304
#Include tutte le variabili numeriche

#Modello backward

#AIC: 1492.616
#Ha rimosso la variabile "Saturated_Fat"

#Comparison

#Coefficients:
#Both models have very similar coefficients for the variables that were retained.
#The removal of "Saturated_Fat" in the backward model did not significantly affect the estimates of the other coefficients.

#Significance of Variables:
#In the full model, "Saturated_Fat" had a high p-value (0.589), indicating it was not a significant variable.
#In the backward model, "Saturated_Fat" was removed, slightly improving the AIC while keeping all other variables significant.

#Overall Performance:
#Both models perform very similarly in terms of R-squared and residual standard error.
#The backward model is preferable because it has a slightly lower AIC, suggesting it is a more parsimonious model without sacrificing the quality of the fit.


#ANOVA comparision between full and final model

anova_results <- anova(calories_lm, calories_lm_backward)
print(anova_results)

#Degrees of Freedom (Res.Df): The full model has 227 degrees of freedom, while the backward model has 228. This is because we removed one variable from the full model.

#Residual Sum of Squares (RSS): The full model has an RSS of 5964.83, while the backward model has an RSS of 5972.55.
#This indicates that the difference between the two models in terms of residual error is very small.

#Sum of Squares (Sum of Sq): The difference between the two models in terms of sum of squares is -7.7235, 
#indicating that the removed variable ("Saturated_Fat") does not significantly contribute to explaining the variability in calories.

#F-statistic (F): The F value is 0.2937 with a p-value of 0.588. This high p-value indicates that there is no significant difference between the two models. 
#In other words, the reduced model is not significantly worse than the full model.

#Conclusion
#The ANOVA shows that the removal of the "Saturated_Fat" variable does not have a significant impact on the model.
#This confirms that the model obtained through backward selection is more parsimonious without compromising the quality of the fit. 
#Therefore, the backward model is preferable to the full model.


#Analisi della multicollinearità 

# Calcolare il VIF per il modello backward

install.packages("car")
library(car) 
vif_backward <- vif(calories_lm_backward)
print(vif_backward)
#Remind: 
#VIF = 1: nessuna collinearità
#VIF tra 1 e 5: moderata collinearità
#VIF > 5: alta collinearità (alcuni considerano VIF > 10 come soglia di preoccupazione)

#we noticed high value of VIF in some variable Cholesterol e Sugars ----> due to different reasons 
#probably high correlation between them 

#Different Measurement Scales: If the variables in the model have significantly different measurement scales such as g and mg this could affect the VIF values. 
#In this case, normalizing the variables might help reduce multicollinearity.

calories_lm_st <- lm(Calories ~ ., data = numerical_vars_df)
summary(calories_lm_st)
AIC(calories_lm_st)

#selection
calories_lm_st_back <- step(calories_lm_st, direction = "backward")
summary(calories_lm_st_back)
AIC(calories_lm_st_back)

#check VIF 
vif_backward_st <- vif(calories_lm_st_back)
print(vif_backward_st)

