
# Different data found:

# Data parkinsons ----
data_park <- read.delim2("Data/parkinsons_updrs.data", header = TRUE, sep = ",")
# Link: https://archive.ics.uci.edu/dataset/189/parkinsons+telemonitoring

# Trasform the data to numeric

data_park <- data.frame(apply(data_park, 2, as.numeric))

y <- data_park$total_UPDRS

model <- lm(total_UPDRS ~ ., data = data_park)
summary(model)
plot(model)

mod_ <- lm(total_UPDRS ~ . - Shimmer.dB., data = data_park)
summary(mod_)

mod_ <- lm(total_UPDRS ~ . - Jitter.DDP - Shimmer.dB. - Shimmer.DDA - Shimmer.APQ3 - Jitter.PPQ5, data = data_park)
summary(mod_)
plot(mod_)
AIC(mod_)


library(olsrr)

# backward elimination

model <- lm(motor_UPDRS ~ ., data = data_park)

backward <- ols_step_backward_p(model)
summary(backward$model)
mod_backward <- backward$model
plot(mod_backward)

mod <- lm(total_UPDRS ~ ., data = data_park)

backward_2 <- ols_step_backward_p(mod)
summary(backward$model)
mod_backward_2 <- backward$model
plot(mod_backward_2)

AIC(mod_backward_2)
AIC(mod_backward)

mod_back <- lm(total_UPDRS ~ . - NHR, data = data_park)
summary(mod_back)

plot(mod_back)

backward_selection <- function(data, response) {
  # Full model with all predictors
  full_model <- lm(as.formula(paste(response, "~ .")), data = data)
  
  # Perform backward selection
  reduced_model <- step(full_model, direction = "backward")
  
  return(reduced_model)
}

backward_model <- backward_selection(data_park, "total_UPDRS")
summary(backward_model)
plot(backward_model)


formula <- total_UPDRS ~ .

mod_full <- lm(formula, data = data_park)
summary(mod_full)
mod_full <- lm(total_UPDRS ~., data = data_park)
summary(mod_full)
drop1(mod_full, scope = formula, test = "F")

curr_mod <- update(mod_full, y ~. - motor_UPDRS)
drop1(curr_mod, test = "F")

curr_mod <- update(curr_mod, y ~. - DFA)
drop1(curr_mod, test = "F")


summary(curr_mod)

mod_backward <- curr_mod
summary(mod_backward)
par(mfrow=c(1,1))
plot(mod_backward)





# forward selection

mod <- lm(total_UPDRS ~ 1, data = data_park)

ols_step_forward_p(mod, data_park$total_UPDRS)


