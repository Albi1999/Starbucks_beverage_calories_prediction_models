# Statistical Learning Project ----

# Load the libraries ----
library("readxl")

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


# Esplorative data analysis ----
# This part is dedicated to the explorative data analysis of the data loaded above

