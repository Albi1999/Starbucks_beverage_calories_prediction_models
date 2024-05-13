# Statistical Learning Project ----

# Load the libraries ----
library("readxl")
library("corrplot")

# Load data ----

## Cinema/area 2018-2022 ----
# This data are related to the number of total shows, entries and expenses for cinema divided for each area
shows_cine_area <- read_excel("Data/Cinema by territorial area.xlsx", sheet = 1)
entries_cine_area <- read_excel("Data/Cinema by territorial area.xlsx", sheet = 2)
expense_cine_area <- read_excel("Data/Cinema by territorial area.xlsx", sheet = 3)

## Places/area 2018-2022 ----
# This data are related to the number of entertainment places for each area divided by activity
pla_act_cin_area <- read_excel("Data/Places of performance in the regions.xlsx", sheet = 1)
pla_act_tea_area <- read_excel("Data/Places of performance in the regions.xlsx", sheet = 2)
pla_act_con_area <- read_excel("Data/Places of performance in the regions.xlsx", sheet = 3)
pla_act_spo_area <- read_excel("Data/Places of performance in the regions.xlsx", sheet = 4)
pla_act_bal_area <- read_excel("Data/Places of performance in the regions.xlsx", sheet = 5)
pla_act_via_area <- read_excel("Data/Places of performance in the regions.xlsx", sheet = 6)
pla_act_mos_area <- read_excel("Data/Places of performance in the regions.xlsx", sheet = 7)

## Total/area 2018-2022 ----
# This data are related to the number of total shows, entries and expenses for each area
shows_area <- read_excel("Data/National territory all sectors (macro area and region).xlsx", sheet = 1)
entries_area <- read_excel("Data/National territory all sectors (macro area and region).xlsx", sheet = 2)
expense_area <- read_excel("Data/National territory all sectors (macro area and region).xlsx", sheet = 3)

## Total/activity 2018-2022 ----
# This data are related to the number of total shows, entries and expenses divided by activity
shows_activity <- read_excel("Data/National territory all sectors (total).xlsx", sheet = 1)
entries_activity <- read_excel("Data/National territory all sectors (total).xlsx", sheet = 2)
expense_activity <- read_excel("Data/National territory all sectors (total).xlsx", sheet = 3)

## Theatre/month 2018-2022 ----
# This data are related to the number of total shows, entries and expenses for theatral activity divided by month
theatre_month <- read.csv("Data/Monthly trends by sector (Theater).csv", sep = ";", header = TRUE)

## Cinema/month 2018-2022 ----
# This data are related to the number of total shows, entries and expenses for Cinema activity divided by month
cinema_month <- read.csv("Data/Monthly trends by sector (Cinema).csv", sep = ";", header = TRUE)

## Total/month 2018-2022 ----
# This data are related to the number of total shows, entries and expenses for the all the activities divided by month
total_month <- read.csv("Data/Monthly trends by sector (All sectors).csv", sep = ";", header = TRUE)

## Organizers/area 2018-2022 ----
# This data are related to the number of organizers for each area divided by activity
org_act_cin_area <- read_excel("Data/Number of show organizers in the regions.xlsx", sheet = 1)
org_act_tea_area <- read_excel("Data/Number of show organizers in the regions.xlsx", sheet = 2)
org_act_con_area <- read_excel("Data/Number of show organizers in the regions.xlsx", sheet = 3)
org_act_spo_area <- read_excel("Data/Number of show organizers in the regions.xlsx", sheet = 4)
org_act_bal_area <- read_excel("Data/Number of show organizers in the regions.xlsx", sheet = 5)
org_act_via_area <- read_excel("Data/Number of show organizers in the regions.xlsx", sheet = 6)
org_act_mos_area <- read_excel("Data/Number of show organizers in the regions.xlsx", sheet = 7)



# What we have to do:

# EDA
# 1. Understand the data structure
# 2. Understand the main characteristics of the data
# 3. Compare the data between the different categories
#   3.1 Cinema & Teatro
# 4. Understand the trend of the data by month
#   4.1 Cinema
#   4.2 Teatro
#   4.3 Generale
# 5. Understand the correlation between the different areas
# 6. Understand the correlation between the different activities
# 7. Understand the correlation between the places and the activities
# 8. Understand the correlation between the places and the entries (spettatori)
# 9. Understand the correlation between the expenses and entries
# 10. Understand the correlation between the number shows and ????
# 11. Understand the correlation between the different macro areas

# MODELS
# Regression and model part between the regional data 
# (Cinema, Teatro, Generale, Organizzatori, Luoghi)
# 1. Creazione modelli
#   1.1 Regressione lineare
#   1.2 Regressione lineare multipla
#   1.3 Regressione logistica
# 2. Selezione modello
#   2.1 Forward 
#   2.2 Backward
#   2.3 Stepwise
#   2.4 BIC - AIC - ecc....
# 3. Analisi residui 
#   3.1 Test
#   3.2 Leverage
#   3.3 Collinearità
#   3.4 Analisi degli outliers 
# 4. Forecasting ....
# 
# Idea:
# Classificazione di un cinema per definire se è efficiente ovvero:
# il rapporto spesa spettatori è maggiore della media


# Esplorative data analysis ----
# This part is dedicated to the explorative data analysis of the data loaded above
# The aim is to understand the data structure and the main characteristics of the data
## 1. Understand the data structure ----
# Summary of the models to understand the data structure
summary(shows_cine_area)
summary(entries_cine_area)
summary(expense_cine_area)
summary(pla_act_cin_area)
summary(pla_act_tea_area)
summary(pla_act_con_area)
summary(pla_act_spo_area)
summary(pla_act_bal_area)
summary(pla_act_via_area)
summary(pla_act_mos_area)
summary(shows_area)
summary(entries_area)
summary(expense_area)
summary(shows_activity)
summary(entries_activity)
summary(expense_activity)
summary(theatre_month)
summary(cinema_month)
summary(total_month)
summary(org_act_cin_area)
summary(org_act_tea_area)
summary(org_act_con_area)
summary(org_act_spo_area)
summary(org_act_bal_area)
summary(org_act_via_area)
summary(org_act_mos_area)

## 2. Understand the main characteristics of the data ----
# Data transformation to understand the main characteristics of the data
total_month$Spesa.del.pubblico <- as.numeric(gsub(",", ".", total_month$Spesa.del.pubblico))
cinema_month$Spesa.del.pubblico <- as.numeric(gsub(",", ".", cinema_month$Spesa.del.pubblico))
theatre_month$Spesa.del.pubblico <- as.numeric(gsub(",", ".", theatre_month$Spesa.del.pubblico))

## NA values in the data ----
# Check if there are NA values in the data
is.na(total_month)
is.na(cinema_month)
is.na(theatre_month)

# Since we have some NA in the monthly data
# We have to remove them from the data to avoid problems in the analysis by sobstituting them with the regression estimation of the column.
for (i in 2:ncol(total_month)) {
  total_month[,i][is.na(total_month[,i])] <- lm(total_month[,i] ~ total_month$Numero.spettacoli, 
                                                data = total_month)$fitted.values
}

for (i in 2:ncol(total_month)) {
  total_month[,i][is.na(total_month[,i])] <- lm(total_month[,i] ~ total_month$Ingressi, 
                                                data = total_month)$fitted.values
}

for (i in 2:ncol(cinema_month)) {
  cinema_month[,i][is.na(cinema_month[,i])] <- lm(cinema_month[,i] ~ cinema_month$Numero.spettacoli, 
                                                  data = cinema_month)$fitted.values
}

for (i in 2:ncol(cinema_month)) {
  cinema_month[,i][is.na(cinema_month[,i])] <- lm(cinema_month[,i] ~ cinema_month$Ingressi, 
                                                  data = cinema_month)$fitted.values
}

for (i in 2:ncol(theatre_month)) {
  theatre_month[,i][is.na(theatre_month[,i])] <- lm(theatre_month[,i] ~ theatre_month$Numero.spettacoli, 
                                                    data = theatre_month)$fitted.values
}

for (i in 2:ncol(theatre_month)) {
  theatre_month[,i][is.na(theatre_month[,i])] <- lm(theatre_month[,i] ~ theatre_month$Ingressi, 
                                                    data = theatre_month)$fitted.values
}

# Check if the NA values have been removed
is.na(total_month)
is.na(cinema_month)
is.na(theatre_month)
# Ok, no more NA


# My part: ------
# the task is understand the correlation between the expenses and entries
# The data are entries_area, entries_cine_area, entries_theatre_area, expense_area, expense_cine_area, expense_theatre_area
# Create the code to do it

# Calculate correlation between entries and expenses for each area

# Transform the data by inverting the rows and columns

# Transpose the data frame
entries_activity_t <- as.data.frame(t(entries_activity))

# Set the column names of the transposed data frame to the elements in the first column
colnames(entries_activity_t) <- entries_activity_t[1, ]

# Remove the first row of the transposed data frame
entries_activity_t <- entries_activity_t[-1, ]

# Transpose the data frame
expense_activity_t <- as.data.frame(t(expense_activity))

# Set the column names of the transposed data frame to the elements in the first column
colnames(expense_activity_t) <- expense_activity_t[1, ]

# Remove the first row of the transposed data frame
expense_activity_t <- expense_activity_t[-1, ]

# Transform in numeric boh the data set
expense_activity_t <- as.data.frame(sapply(expense_activity_t, as.numeric))
entries_activity_t <- as.data.frame(sapply(entries_activity_t, as.numeric))


cor_entries_expense_cine <- cor(expense_activity_t$"Attività cinematografica", entries_activity_t$"Attività cinematografica")
cor_entries_expense_thea <- cor(expense_activity_t$"Attività teatrale", entries_activity_t$"Attività teatrale")
cor_entries_expense_con <- cor(expense_activity_t$"Attività concertistica", entries_activity_t$"Attività concertistica")
cor_entries_expense_spo <- cor(expense_activity_t$"Attività sportiva", entries_activity_t$"Attività sportiva")
cor_entries_expense_bal <- cor(expense_activity_t$`Attività di ballo e concertini`, entries_activity_t$`Attività di ballo e concertini`)
cor_entries_expense_via <- cor(expense_activity_t$"Attrazioni dello spettacolo viaggiante", entries_activity_t$"Attrazioni dello spettacolo viaggiante")
cor_entries_expense_mos <- cor(expense_activity_t$"Mostre ed esposizioni", entries_activity_t$"Mostre ed esposizioni")

# Print the correlation coefficients
print(paste("Correlation between cinema entries and cinema expenses: ", cor_entries_expense_cine))
print(paste("Correlation between theatre entries and theatre expenses: ", cor_entries_expense_thea))
print(paste("Correlation between concert entries and concert expenses: ", cor_entries_expense_con))
print(paste("Correlation between sport entries and sport expenses: ", cor_entries_expense_spo))
print(paste("Correlation between dance entries and dance expenses: ", cor_entries_expense_bal))
print(paste("Correlation between travelling show entries and travelling show expenses: ", cor_entries_expense_via))
print(paste("Correlation between exhibitions entries and exhibitions expenses: ", cor_entries_expense_mos))

# Plot the results with corrplot ----
# Load the library
corrplot(cor(expense_activity_t, entries_activity_t), method = "number")



corrplot(cor(expense_activity_t, entries_activity_t), method = "number", type = "upper", tl.col = "black", tl.srt = 45, diag = FALSE, addCoef.col = "black", addCoefasPercent = TRUE)




# Understand the correlation between the number shows and ????
# Understand the correlation between the different macro areas




