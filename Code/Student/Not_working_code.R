
# Funzione per eseguire il backward elimination

# Data without the variables that generates the general score
stud_cleaned <- stud[, -c(6, 7, 8, 9)]
backward_selection <- function(data, response) {
  # Full model with all predictors
  full_model <- lm(as.formula(paste(response, "~ .")), data = data)
  
  # Perform backward selection
  reduced_model <- step(full_model, direction = "backward")
  
  return(reduced_model)
}

backward_model <- backward_selection(stud_cleaned, "y")
summary(backward_model)

# Function to perform forward selection
forward_selection <- function(data, response) {
  # Null model with no predictors
  null_model <- lm(as.formula(paste(response, "~ 1")), data = data)
  
  # Perform forward selection
  reduced_model <- step(null_model, direction = "both", scope = list(lower = null_model, upper = lm(as.formula(paste(response, "~ .")), data = data)))
  
  return(reduced_model)
}

forward_model <- forward_selection(stud_cleaned, "y")
summary(forward_model)

stepwise_selection <- function(data, response) {
  # Full model with all predictors
  full_model <- lm(as.formula(paste(response, "~ .")), data = data)
  
  # Perform stepwise selection
  reduced_model <- step(full_model, direction = "both")
  
  return(reduced_model)
}

stepwise_model <- stepwise_selection(stud_cleaned, "y")
summary(stepwise_model)
