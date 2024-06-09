# Statistical Learning final project
# GRETA
# Regression Analysis ----
## Linear Regression ----

# This code is dedicated to the regression part (GRETA)

# Linear regression model to predict the amount of calories
# based on the amount of the other variables
# We use the lm() function to fit a linear regression model
# We use the summary() function to display the results
# We use the plot() function to visualize the results

#fit linear simple regression with just one variable on data_cleaned
#looking at correlation plot we choose Sugars due to high correlation 

#This code will fit a simple linear regression model predicting "Calories" using "Sugars" as the predictor variable and provide a summary of the model.

calories_lm_simple <- lm(Calories ~ Sugars, data = data_cleaned)
summary(calories_lm_simple)

#The coefficient for "Sugars" (4.7426) indicates that, on average, for every one-unit increase in "Sugars", the predicted "Calories" increases by approximately 4.7426 units.
#Both the intercept and the coefficient for "Sugars" are statistically significant (p < 0.001), indicating a strong linear relationship between "Sugars" and "Calories".
#The F-statistic is highly significant (p < 2.2e-16), indicating that the overall regression model is statistically significant in explaining the variance in "Calories".
#Model Fit:The adjusted R-squared value (0.8268) indicates that approximately 82.68% of the variance in "Calories" can be explained by the predictor variable "Sugars".

AIC(calories_lm_simple)
BIC(calories_lm_simple)

#Overall, this output suggests that the simple linear regression model provides a statistically significant relationship between "Sugars" and "Calories", with "Sugars" being a strong predictor of "Calories".
#However, the AIC and BIC values suggest that there might be other models that provide a better fit for the data.
#too simple non cattura bene le relazioni interne 

#Diagnostica dei residui 
par(mfrow = c(2, 2))
plot(calories_lm_simple)

#Aggiungere commenti + outliars work on it 

# Fit the multiple linear regression model
#Select only numeric variables from the cleaned dataset in order to implement regression model 

numerical_vars <- data_cleaned[, sapply(data_cleaned, is.numeric)]

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
#We've also tried forward and spetwise selection but we choose to mantain backward 

#set backward as final model so from now work on this 

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

#diagnostica residui 

par(mfrow = c(2, 2))
plot(final_model)

#Analisi della multicollinearità 

#Determine VIF for the final model

install.packages("car")
library(car) 
vif_mod <- vif(final_model)
print(vif_mod)


#Remind: 
#VIF = 1: nessuna collinearità
#VIF tra 1 e 5: moderata collinearità
#VIF > 5: alta collinearità (alcuni considerano VIF > 10 come soglia di preoccupazione)

#we noticed high value of VIF in some variable Cholesterol e Sugars ----> due to different reasons 
#but in general is too high
#probably high correlation between variables,means that variable contribuite in the same way to predict and explain calories
#same information
#data unbalanced 
#Different Measurement Scales: If the variables in the model have significantly different measurement scales such as g and mg this could affect the VIF values. 
#In this case, normalizing the variables might help reduce multicollinearity.

#we've tried different type of normalization 

#first at all scale by standard normalization 
#Il valore negativo di AIC (-748.2623) indica che il modello di regressione lineare standardizzato fornisce un miglior
#compromesso tra adattamento ai dati e complessità del modello rispetto al modello di riferimento
#but VIF still to high

#To reduce the problem of high VIF in linear regression, it is generally preferable to use the transformation that includes both log transformation and standardization of the data. 
#This is because standardization helps to put all variables on the same scale, reducing the likelihood of multicollinearity.
#Log transformation: Reduces the variance of the variables, making the distribution more normal and reducing the impact of outliers.
#Standardization: Puts all variables on a common scale, with mean 0 and standard deviation 1, further reducing multicollinearity.

std_data_log <- scale(log(data_num + 1))

# Set as dataframe

std_data_log_df <- as.data.frame(std_data_log)

# Fit the linear regression model using the standardized variables as predictors

mod_log_tr <- lm(Calories ~ ., data = std_data_log_df)

summary(mod_log_tr)

AIC(mod_log_tr)
BIC(mod_log_tr)

## The model has a low AIC (in this case we have negative value of AIC, we want close to zero ) and BIC values
#negative AIC is okay with log tasformation and stardadization sooooo no panic !!!!
#the R-squared value is 0.99 so the model is a good fit

mod_log_tr_backward <- step(mod_log_tr, direction = "backward")
summary(mod_log_tr_backward)
AIC(mod_log_tr_backward)
BIC(mod_log_tr_backward)


#AIC slighlty worst but R^2 good

vif(mod_log_tr_backward)

#VIF still high for sugars and cholesterol 
#so we tried to remove manually these variables 
mod_log_tr_updated <- lm(Calories ~ . - Cholesterol - Sugars, data = std_data_log_df)
## Ricalcola i valori VIF per il modello aggiornato
vif_updated <- vif(mod_log_tr_updated)
print(vif_updated)

#again backward 
mod_log_tr_backward_2 <- step(mod_log_tr_updated, direction = "backward")
summary(mod_log_tr_backward_2)
names(coefficients(mod_log_tr_backward_2))
#modello molto ridotto
#R^2 the worst but still not bad  


vif_2 <- vif(mod_log_tr_backward_2)
print(vif_2)
#pefect

AIC(mod_log_tr_backward_2)
BIC(mod_log_tr_backward_2)
#BEST VALUES!!!!

#diagnostica dei residui male??? -----> da commentare meglio 
par(mfrow = c(2, 2))
plot(mod_log_tr_backward_2)

#Model Diagnostics: Non-normal residuals suggest that some assumptions of linear regression might be violated. 
#Specifically, the assumption of normality of the residuals is not met. 
#This can affect the validity of hypothesis tests on the coefficients and predictions.

shapiro.test((residuals(mod_log_tr_backward_2)))

#Given the p-value is significantly smaller than 0.05, we reject the null hypothesis. 
#This indicates that the residuals of the model mod_log_tr_backward_2 do not follow a normal distribution.
#In this case, W is quite a bit lower than 1, suggesting the residuals deviate from normality.
#Sol Robust Methods: Use robust regression methods that do not assume normality of errors

#so we've tried other trasformation like min-max scaling and robust scaling.
#but not satisfactory due to VIF still to high. 
#Regularization: Using regularization methods such as ridge regression or lasso regression penalizes the coefficients of variables, helping to reduce multicollinearity.

## Lasso Regression ---- 
#Lasso regression can aid in variable selection by reducing the coefficients of non-significant variables to zero.

# We use the glmnet package to fit the lasso regression model
# The glmnet package is used to fit generalized linear models via penalized maximum likelihood
# The function cv.glmnet() is used to fit a lasso regression model with cross-validation

library(glmnet)

#Lasso regression tends to shrink the coefficients of less important variables towards zero, effectively performing variable selection. By eliminating irrelevant variables from the model, 
#it reduces the number of predictors and thereby reduces multicollinearity.
#Lasso tends to produce sparse solutions, meaning it drives many coefficients to exactly zero. When variables are removed from the model, 
#the multicollinearity among predictors decreases, leading to lower VIF values.
#It performs automatic features selection by shrinking some coefficients to zero. 
#This feature selection process inherently removes redundant variables and reduces multicollinearity in the model.

# Fit the lasso regression model
#we also have standardize data which is important for regularization tecniques
#We use data with logaritmic trasformation in order to reduce multicollinearity and stabilize 

#set y as calories








