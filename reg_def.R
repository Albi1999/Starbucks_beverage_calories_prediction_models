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
#we also have standardize data which is important for regularization tecniques.
#Standardization is essential because Lasso penalizes the coefficients of the predictors, and if the predictors are not on the same scale,
#those with larger scales will be penalized more than those with smaller scales. This can lead to distorted results.

#set y as calories which is our target variable 
y <- data_cleaned$Calories 

# Standardize the data
std_data <- as.data.frame(scale(data_num))

# Fit the lasso regression model
mod_lasso <- cv.glmnet(x = as.matrix(std_data[, -1]),
                       y = std_data$Calories,
                       alpha = 1, standardize = FALSE)

summary(mod_lasso)
lasso_coef <- coef(mod_lasso, s = "lambda.min")
lasso_coef

#By setting some coefficients to zero such as Saturated_Fat), Lasso helps in feature selection, which reduces the complexity of the model.
#The remaining non-zero coefficients indicate the variables that have a significant contribution to predicting the calories. 
#The signs and magnitudes of these coefficients show the direction and strength of their relationships with the target variable (calories).
#ad example A one-unit increase in Sodium, holding all other variables constant, is associated with a decrease of approximately 0.021 calories.
#Specifically, the `cv.glmnet` function performs cross-validation to determine the value of lambda that minimizes the prediction error. 
#This value is identified as `lambda.min, in our case we found that the optimal value of lambda is equal to 1.
#To further evaluate the model's performance, metrics such as R-squared and Mean Squared Error (MSE) should be considered. 
#These metrics will help in understanding how well the model explains the variance in the data and the average error of the predictions, respectively.
#Also,the lambda value that minimizes the MSE is selected as the optimal lambda value

# Calculate the R-squared value for the lasso regression model
lasso_pred <- predict(mod_lasso, s = "lambda.min", newx = as.matrix(std_data[, -1]))
lasso_r_squared <- cor(lasso_pred, std_data$Calories)^2
lasso_r_squared

# Calculate the MSE for the lasso regression model
lasso_mse <- mean((lasso_pred - std_data$Calories)^2)
lasso_mse

#The R-squared value of approximately 0.998 indicates that the Lasso regression model explains about 99.76% of the variance in the Calories variable. 
#This suggests a very strong fit, as the model is capturing almost all the variability in the target variable.

#The Mean Squared Error (MSE) of approximately 0.0024 indicates a very low average squared difference between the observed actual outcomes and the outcomes predicted by the model. 
#This suggests that the model's predictions are very close to the actual values, indicating high accuracy.

## The optimal lambda value is used to fit the final lasso regression model
par(mfrow = c(1, 1))
plot(mod_lasso)
#ADD COMMENT 


#We also tried Ridge Regression in order to reduce multicollinearity:
#Likely LASSO, the Ridge regression is regulariztion tecnique witch introduce a term of penality that 
#tends to shrink coefficients towards zero without eliminating them entirely, meaning that all variables can remain in the model. 
#Differently from LASSO, that has the ability to zero out some coefficients, making them exactly zero, thus providing a form of variable selection,
#Ridge regression is an appropriate choice for reducing multicollinearity without sacrificing the presence of all variables in the model, 
#whereas LASSO may be preferred when one wants to select only a subset of the most relevant variables.

# The ridge regression model is fit using the glmnet package
# The cv.glmnet() function is used to fit the ridge regression model with cross-validation

# Fit the ridge regression model with the standardization
mod_ridge <- cv.glmnet(x = as.matrix(std_data[, -1]),
                       y = std_data$Calories,
                       alpha = 0, standardize = FALSE)
summary(mod_ridge)
ridge_coef <- coef(mod_ridge, s = "lambda.min")
ridge_coef

#Again the best value of lambda is detemined by cross validation in order to minimize the error, likely to LASSO, is equal to 1.
#the interpretation of coefficients is the same of lasso.

#Evaluating the model with R^2 and MSE

# Calculate the R-squared value for the ridge regression model

ridge_pred <- predict(mod_ridge, s = "lambda.min", newx = as.matrix(std_data[, -1]))
ridge_r_squared <- cor(ridge_pred, std_data$Calories)^2
ridge_r_squared

#similar to LASSO, very high 

# Calculate the MSE for the ridge regression model
ridge_mse <- mean((ridge_pred - std_data$Calories)^2)
ridge_mse

# Plot the cross-validated mean squared error (MSE) as a function of the lambda values
plot(mod_ridge)
#ADD comment

#Model comparison: 
# now we want to compare the model using the R-squared value
# The R-squared value is a measure of how well the model fits the data
# The R-squared value ranges from 0 to 1, with higher values indicating a better fit
# The MSE is a measure of the average squared difference between the predicted and actual values
# The MSE is used to evaluate the performance of the model, with lower values indicating better performance
# We choose the model with the highest R-squared value and the lowest MSE
#Based on these values we choose LASSO as the best model, since teh R^2 are equal, but the MSE is a bit better in LASSO.

#??????????????????????we want to check is Lasso regression has effectvily reduced the multicollinearity, so we calculated the VIF on 
#predictors resulted by fitted Lasso, by looking at coefficients and correlation matrix

# Seleziona solo le variabili predittive rimanenti
lasso_selected_vars <- std_data[, -1][, which(lasso_coef[-1] != 0)]

# Calcola la matrice di correlazione
cor_matrix_lasso <- cor(lasso_selected_vars)

# Visualizza la matrice di correlazione con un grafico
corrplot::corrplot(cor_matrix_lasso, method = "number")

#TENERE?????????????????????????
#The variables "Total_Fat," "Trans_Fat," "Sodium," "Total_Carbohydrates," "Cholesterol," "Dietary_Fibre," "Sugars," "Protein," "Vitamin_A," "Vitamin_C," "Calcium," "Iron," and "Caffeine" have coefficients of significant magnitudes, 
#suggesting that these variables are important for predicting calories.
#Despite the regularization of the Lasso model, some variables have coefficients of significant magnitudes, which could suggest that these variables are not strongly correlated with each other, thus reducing the impact of multicollinearity. 
#Overall, the absence of coefficients with very large magnitudes and the presence of coefficients close to zero for some variables suggest that the Lasso model may have helped mitigate multicollinearity and select only the most important variables for predicting calories.

### Cross validation ---- 
#This is a way to see if the model perferm well on the test set and generalize effeciently the data we gave as training set
# Split the data in training and test set then check the accuracy of the model.
# Split the data into training with 80% of examples and test sets with 20% of examples
set.seed(123)
train_index <- sample(1:nrow(std_data), 0.8 * nrow(std_data))
train_data <- std_data[train_index, ]
test_data <- std_data[-train_index, ]

# Fit the lasso regression model on the training data
mod_lasso_train <- cv.glmnet(x = as.matrix(train_data[, -1]),
                             y = train_data$Calories,
                             alpha = 1, standardize = FALSE)

# Predict the calories on the test data using the lasso regression model

lasso_pred_test <- predict(mod_lasso_train, s = "lambda.min", newx = as.matrix(test_data[, -1]))

# Calculate the R-squared value for the lasso regression model on the test data

lasso_r_squared_test <- cor(lasso_pred_test, test_data$Calories)^2
lasso_r_squared_test

#The accuracy is really high on test indicating that the model has learned effectively from trained data
#and generalize well on unknwon  examples 

# Calculate the MSE for the lasso regression model on the test data
# The MSE is a measure of the average squared difference between the predicted and actual values

lasso_mse_test <- mean((lasso_pred_test - test_data$Calories)^2)
lasso_mse_test

#Overall, the high R-squared value and low MSE on the test data suggest that the lasso regression model has learned effectively from the training data and generalizes well to unseen examples.
# Plot the predicted values against the actual values on the test data

plot(test_data$Calories, lasso_pred_test, xlab = "Actual Calories",
     ylab = "Predicted Calories", main = "Predicted vs Actual Calories",
     col = "#4ea5ff", pch = 19)


# The plot shows the predicted values against the actual values on the test data
# The points are close to the diagonal line, indicating that the model is making accurate predictions
# The R-squared value and MSE are used to evaluate the performance of the model
# The R-squared value is 0.99, indicating that the model explains 99% of the variance in the data

#Logistic regression.
#Logistic regression is a statistical model used to predict the outcome of a binary categorical dependent variable based on one or more independent variables.
#Since logistic regression is tipycally used for classification task and our variable Calories ,that we want to predict is continous 
#random variable, we have to traspose the problem into a classification one by making the variable binary. 
#In order to do that we classify foods into two categories: "low calorie" and "high calories" defining a threshold to distinguish between the two classes.

#let's take a look into the structure of the variable 
#using numeric dataset only


#We've tried with normal data, standardize data, and log trasformation since again it helps to reduce multicolli.
#and we find out that the best is with log trasformed data 
#looking at the summary and the plot of the variable, we notice that calories follow a semi-gaussian distribution
#both Median and mean are reasonable approach to use, since they are close to each other. 
#However we choose median as treshold is less sensitive to outliers and skewness in the data. 
#It ensures that half the data points are classified as "low calories" and the other half as "high calories," providing balanced classes.

y <- std_data_log_df$Calories
calories_median <- median(y)
calories_median

# Load necessary package
library(glmnet)

# Create a new binary target variable based on the median
std_data_log_df$Calorie_Class <- ifelse(std_data_log_df$Calories > calories_median, 1, 0)
table(std_data_log_df$Calorie_Class)
#Creates a new binary variable (Calorie_Class) where 1 indicates high calorie (above median) and 0 indicates low calorie (below or equal to median).


#Fit a logistic regression model on the complete dataset obatines with log tasformation and by removing first column 
logistic_model <- glm(Calorie_Class ~ ., data = std_data_log_df[,-1], family = binomial)

#Summary of the logistic model
summary(logistic_model)

#The coefficient for Cholesterol is statistically significant at the 0.01 level.
#The coefficient for Total_Fat it is marginally significant at the 0.05 level.
# the residual deviance is much smaller than the null deviance, indicating that the model with predictors explains more variability than the null model.
#the AIC is 69.424, suggesting that the model has reasonable fit.
#Number of Fisher Scoring Iterations: Indicates the number of iterations performed by the Fisher scoring algorithm during model fitting. In this case, it took 11 iterations.

# The best is with log transformation so we try to use it into the cross validation tecnique...

#Split the data into training (80%) and test sets (20%)
set.seed(123)
train_index <- sample(1:nrow(std_data_log_df), size = 0.8 * nrow(std_data_log_df))
train_data <- std_data_log_df[train_index, ]
test_data <- std_data_log_df[-train_index, ]
#remove the original calories column such as continous variable
new_test_data <- test_data[,-1]
str(train_data) #new variable is included
#remove the original calories column such as continous variable
new_train_data <- train_data[,-1]

#Fit a logistic regression model on the new training data
logistic_model_train <- glm(Calorie_Class ~ ., data = new_train_data, family = binomial)

#Summary of the logistic model
summary(logistic_model_train)

#the coefficients significant are pretty much the same, with almost similar level of significance 
#Total_Fat: Although not highly significant (p = 0.0558), 
#it shows a tendency towards significance (p < 0.1), suggesting that it might have an effect on the probability of an observation being in the "high calorie" category.
#Cholesterol: This predictor has a significant coefficient (p = 0.0219),
#indicating that it likely influences the probability of an observation belonging to the "high calorie" category.
#Given that this dataset is smaller, it's not unexpected to see fewer coefficients reaching statistical significance due to reduced statistical power
#the AIC is lower that the one calculated in the full set indicating better performance

#Predict on the new test data
predictions <- predict(logistic_model_train, newdata = new_test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
conf_matrix <- table(Predicted = predicted_classes, Actual = new_test_data$Calorie_Class)

# Print the confusion matrix
print(conf_matrix)
#The model correctly predicted 22 instances as "low calorie" when they were actually "low calorie."
#The model incorrectly predicted 2 instances as "low calorie" when they were actually "high calorie."
#The model incorrectly predicted 2 instances as "high calorie" when they were actually "low calorie."
#The model correctly predicted 23 instances as "high calorie" when they were actually "high calorie."

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", accuracy))
# The model's accuracy of approximately 91.8% indicates it is performing well overall in classifying the calorie content correctly.
#Generalize well on the test set

# Now we have to plot some graph of the model and integrate it in the report

par(mfrow = c(2,2))
plot(logistic_model_train)


######DA RIVEDERE.............
#Residuals vs Fitted:
  
 # This plot shows the Pearson residuals against the fitted values.
#Ideally, there should be no clear pattern, indicating that the model is well-fitted. However, the presence of data points at extreme values (far from 0) suggests potential issues with model fit or outliers.
#Normal Q-Q Plot:
  
 # The Q-Q plot compares the standardized deviance residuals to a theoretical normal distribution.
#Significant deviations from the straight line suggest that the residuals are not normally distributed, which can indicate potential problems with the model. In this case, the data points deviate from the line, particularly at the higher quantiles, indicating that the residuals are not perfectly normally distributed.
#Scale-Location Plot (Spread-Location Plot):
  
 # This plot shows the square root of the standardized residuals against the fitted values.
#The red line helps to identify trends. Ideally, the points should be randomly scattered without a clear pattern. Here, we see some clustering and trends at extreme fitted values, suggesting heteroscedasticity or non-constant variance.
#Residuals vs Leverage:
  
 # This plot shows standardized residuals against leverage, highlighting influential data points.
#The dashed lines represent Cook's distance. Points outside these lines indicate influential observations that have a significant impact on the model. In this plot, several points, especially at higher leverage values, fall outside the dashed lines, indicating they are influential.
#Interpretation
#Potential Issues with Model Fit: The presence of extreme residuals in the Residuals vs Fitted and Scale-Location plots suggests that the model might not fit well across all observations. This can be due to outliers or the model not capturing the underlying data structure adequately.
#Non-Normal Residuals: The Q-Q plot indicates that the residuals are not perfectly normally distributed, which is expected in logistic regression but still worth noting.
#Influential Points: The Residuals vs Leverage plot shows several influential points, suggesting that some observations have a disproportionate impact on the model. These points should be investigated further to understand their nature and whether they are legitimate data points or outliers.
#Next Steps
#Investigate Influential Points: Check the data points identified as influential in the Residuals vs Leverage plot to understand why they have high leverage and residuals.
#Consider Model Refinement: If certain variables consistently show poor performance, it might be necessary to transform them, add interaction terms, or consider alternative modeling techniques.
#Check for Multicollinearity: Ensure that multicollinearity is not affecting the model by calculating Variance Inflation Factors (VIFs) for the predictors.
#Evaluate Model with Additional Metrics: Use additional performance metrics such as ROC AUC, Precision-Recall curves, and confusion matrix to evaluate the model's predictive performance comprehensively.


#we try now to fit polynomial regression ,wich extend linear regression by modelling teh relationship between the target variable y and predictors
#with a n-esim polynomial. It helps when the relationshion between variables is not linear and A simple straight line fails to capture the complexity of the data.

#looking at the scatterplot betwenn calories and sugar, calories and protein,calories and fiber we want to investigate more about thsi relationship
#calories and fiber looks linear 
#calories and protein not so linear, points follow a random trend
#calories and sugar confirmed also by residuals plot, looks linear 

#we want to model the variable calories in function of sugars, protein,and fiber 
#By adding polynomial terms, the model becomes more flexible and can fit the data better. 
#However, it is important not to overdo the degree of the polynomial to avoid overfitting.
#for this reason we choose to apply quadratic polynomial since is a good compromise between model fexibility and its capacity to generalize data.

#standardize data
data_num_std <- as.data.frame(scale(data_num))


#Create polynomial terms (secondo grado)
data_num_poly <- data_num_std
data_num_poly$Sugars2 <- data_num_std$Sugars^2
data_num_poly$Protein2 <- data_num_std$Protein^2
data_num_poly$Dietary_Fibre2 <- data_num_std$Dietary_Fibre^2
data_num_poly$Sugars_Protein <- data_num_std$Sugars * data_num_std$Protein
data_num_poly$Sugars_Fiber <- data_num_std$Sugars * data_num_std$Dietary_Fibre
data_num_poly$Protein_Fiber <- data_num_std$Protein * data_num_std$Dietary_Fibre

#we've included also interaction between variables, in order to understand how the independent variables influence the dependent variable in combination with each other.

#Separate in test and training set we the same criterions as before
set.seed(123)
train_index <- sample(seq_len(nrow(data_num_poly)), size = 0.8 * nrow(data_num_poly))
train_data <- data_num_poly[train_index, ]

test_data <- data_num_poly[-train_index, ]

#Fit polynomial model on training data
model_poly <- lm(Calories ~ Sugars + Protein + Dietary_Fibre + Sugars2 + Protein2 + Dietary_Fibre2 +
                   Sugars_Protein + Sugars_Fiber + Protein_Fiber, data = train_data)

#evaluate the model
summary(model_poly)
#the model suggests that Sugars and Protein are strong predictors of Calories. 
#There is also evidence of non-linear relationships for Sugars and Protein, as indicated by the marginal significance of their quadratic terms. 
#However, Dietary_Fibre and most interaction terms are not significant, suggesting they do not add much explanatory power to the model.

par(mfrow = c(2,2))
plot(model_poly)
#Residuals vs Fitted Plot:the residuals appear to be somewhat randomly scattered, but there are some points (like 880 and 850) that are further from the center, indicating potential outliers. 
#The lack of a clear pattern suggests that the model captures the data well, though the slight curvature suggests there might still be some non-linearity present

#Q-Q Plot: most points follow the reference line closely, but there are deviations at the tails, especially at the higher end (points 880 and 128). 
#This suggests that while the residuals are approximately normally distributed, there are some deviations which could be due to outliers or heavy tails.

#Scale_location plot: there is a slight upward trend in the red line, suggesting that the variance of the residuals might increase with the fitted values. 
#This indicates potential heteroscedasticity, where the variability of the residuals is not constant across all levels of the fitted values.

#Residuals  vs Levarage: points 128 and 850 appear to be influential as they are far from the center and beyond the Cook's distance lines.
#This suggests that these points have a significant impact on the model and could potentially distort the results. 
#???????????It might be worth investigating these points further to understand why they are influential and if they represent true variability or potential data issues.

# Predictions Test Set
predictions <- predict(model_poly, newdata = test_data)

# Calculate (Mean Squared Error, MSE)
mse <- mean((test_data$Calories - predictions)^2)
print(paste("Mean Squared Error: ", mse))
# MSE of 0.0431 suggests that the model's predictions are quite close to the actual values, on average.

# Calculate del R-squared
sse <- sum((test_data$Calories - predictions)^2)
sst <- sum((test_data$Calories - mean(test_data$Calories))^2)
r_squared <- 1 - sse / sst
print(paste("R-squared: ", r_squared))
#The R-squared (coefficient of determination) measures the proportion of the variance in the dependent variable (Calories) 
#that is predictable from the independent variables (Sugars, Protein, and Dietary_Fibre along with their polynomial and interaction terms). 
#An R-squared value of 0.966 means that 96.6% of the variance in Calories is explained by the model. 
#This indicates a very good fit, as a higher R-squared value (close to 1) suggests that the model explains a large portion of the variance in the dependent variable.

#In general polynomail regression fits well, but we can notice that the quadratic terms and interaction betweem variables 
#does not expain calories. so maybe not usefull 

#LDA 

#Linear Discriminant Analysis (LDA) is a statistical technique primarily used for classification and dimensionality reduction. 
#Its main goal is to find a linear combination of features that best separates two or more classes of objects or events.
#LDA seeks to maximize the separation between classes by minimizing the variance within classes and maximizing the variance between classes.

#In the context of our dataset, LDA is useful for classifying different categories of beverages (Beverage_category). 
# LDA is particularly useful for classification problems where the goal is to assign observations to one of several classes. In our case, we want to classify beverages into different categories based on all numeric variables.
#If the categories of beverages in your dataset are linearly separable, LDA can find the linear combinations of predictive variables that best separate these categories.

#If the categories of beverages in your dataset are linearly separable, LDA can find the linear combinations of predictive variables that best separate these categories.

#Dimensionality Reduction: Even if your dataset has a moderate number of variables, LDA can help reduce the dimensionality of the problem, making classification more manageable and reducing the risk of overfitting.

#Interpretability: LDA produces coefficients that are easily interpretable and can provide insights into how each variable contributes to the classification of beverages.

## Load necessary library
library(MASS)

# Assume `cleaned_data` is your dataset and `Beverage_category` is the target variable
# Extract numeric predictors and the target variable
numeric_vars <- numerical_vars
target_var <- data_cleaned$Beverage_category
data_cleaned$Beverage_category <- as.factor(data_cleaned$Beverage_category)

# Combine the numeric predictors with the target variable
data_for_lda <- cbind(numeric_vars, Beverage_category = target_var)

# Split the data into training and testing sets (same partions as always)
set.seed(123)
train_index <- sample(seq_len(nrow(data_for_lda)), size = 0.8 * nrow(data_for_lda))
train_data <- data_for_lda[train_index, ]
test_data <- data_for_lda[-train_index, ]

# Fit the LDA model using the training data
lda_model <- lda(Beverage_category ~ ., data = train_data)

#LDA aims to find the linear combination of predictors (numeric variables) that best separates the different categories of beverages.
#Method: It projects the data onto a lower-dimensional space (linear discriminants) while maximizing the separation between the different categories (Beverage_category).
#Application: By fitting an LDA model, you can classify new beverage entries based on their numeric attributes.

#we've used all numerical variables as predictors 

# Print the model summary
print(lda_model)

#let's look deepy into the summary:
#1.The prior probabilities indicate the proportion of each beverage category in the training dataset. This shows the initial likelihood of each category before considering any predictors.
#2.These are the mean values of each numeric predictor for each beverage category. They provide insights into the average nutritional and compositional profile of each beverage type.
#Calories: Varies widely, with Coffee having the lowest (4.67) and Smoothies the highest (282.22).
#Total Fat, Trans Fat, Saturated Fat: Reflect the fat content differences across beverage types.
#Sodium, Total Carbohydrates, Cholesterol: Provide further details on the nutritional content.
#Dietary Fibre, Sugars, Protein: Highlight the beverage's dietary fiber, sugar, and protein content.
#Vitamin A, Vitamin C, Calcium, Iron, Caffeine: Show the average micronutrient and caffeine levels.
#3.The coefficients for each linear discriminant function (LD1 to LD8) indicate the importance and direction of each predictor in distinguishing between beverage categories.
#LD1: Accounts for 75.36% of the variation between categories. Major contributing factors include Total Fat, Saturated Fat, and Sugars, suggesting these are crucial in separating the beverage types.
#LD2: Accounts for 17.12% of the variation. Trans Fat and Dietary Fibre are significant contributors.
#Subsequent linear discriminants (LD3 to LD8) account for progressively smaller proportions of the variation.
#4.The proportion of trace values indicates how much of the total variability in the dataset is captured by each linear discriminant. 
#LD1 captures the majority (75.36%), followed by LD2 (17.12%), with the remaining discriminants capturing much smaller amounts of variability.

#After applying the Linear Discriminant Analysis (LDA) model to classify beverage categories in your dataset, we assessed its performance using a confusion matrix and calculated the accuracy.
# Predict the Beverage_category for the test data
predictions <- predict(lda_model, test_data)$class

# Create a confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_data$Beverage_category)
print(conf_matrix)

#The model made 4 errors out of 49 predictions: 2 false negatives and 2 false positives. 
#This suggests that while the model performs well, there are occasional misclassifications, which could be due to overlap in the predictor values between certain beverage categories.

# Calculate the accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", accuracy))

#An accuracy of 91.84% indicates that the LDA model is very effective in classifying the beverage categories based on the given predictors. 
#This high accuracy suggests that the nutritional and compositional variables we included are strong discriminators of the different beverage types.

##LDA can be highly accurate when there are well-separated classes, making it suitable for classifying distinct beverage types based on their nutritional and compositional attributes.
#Interpretability: LDA provides insights into which numeric variables are most influential in distinguishing between beverage categories.
#Performance: It often performs well even with relatively small datasets, provided the classes are linearly separable to some extent.


#IMPOOOORTANTISSIMO
#Given the high accuracy, this LDA model can be reliably used for classifying new beverage entries in your dataset. 
#It can help automate the categorization process, ensuring consistent and accurate classification based on the nutritional content.
#This model can assist in various aspects such as inventory management, nutritional labeling, and marketing strategies by accurately categorizing beverages into predefined categories based on their nutritional attributes.



# Visualize the results -------> da sistemare senza ggplot fa cacare
# Plot the LDA results (2 discriminant functions)
plot(lda_model)

#Overall:
#Classification Power: The LDA model effectively uses the nutritional and compositional variables to classify beverages into their respective categories. 
#The first few linear discriminants (especially LD1 and LD2) capture most of the variability, indicating that the model is likely to be quite effective in distinguishing between beverage types.

#Important Predictors: Total Fat, Saturated Fat, and Sugars are particularly important in differentiating between beverage categories.
#This suggests that these nutritional factors are key characteristics that define different types of beverages in your dataset.

#Group Means Analysis: By examining the group means, you can see which beverage types are higher or lower in certain nutrients.
#For instance, Smoothies have the highest calories and protein, whereas Coffee has the lowest in both.

#Practical Application: This LDA model can now be used to classify new beverage entries based on their numeric attributes. 
#It can help in identifying which category a new beverage falls into, which is useful for inventory management, nutritional analysis, and consumer information.
