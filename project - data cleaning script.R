# Data cleaning script for Machine Learning group project
# Camille Kirsch
# 02.16.2021

library(tidyverse)
imm <- read_csv("~cdkma/Documents/Machine Learning/data/imm_df.csv")

# Select only columns which measure total cases (and index column)
imm_clean <- imm %>%
  select(`Custody/AOR/Facility`, contains("Total confirmed")) %>%
  mutate_all(funs(replace_na(., 0)))

# Find column index of last date we want to include
which(colnames(imm_clean)=="02/01/21:Total confirmed COVID-19 cases")

# Truncate tibble at 02/01/2021 and rename this 'total_cases'
imm_clean <- imm_clean[1:196]
colnames(imm_clean)[196] <- "total_cases"
colnames(imm_clean)[1] <- "Facility"

# Create a first_case column; also, a days_infected and a per_day column
imm_clean <- imm_clean %>%
  mutate(Facility = trimws(Facility), Facility = tolower(Facility),
         first_case = {
           tmp <- select(., contains('confirmed'))
           ifelse(rowSums(tmp) == 0, NA, max.col(tmp != 0, ties.method = 'first'))
           }, days_infected = 196-first_case,
         per_day = total_cases/days_infected)

# Sort by total cases
# Also, move State, total_case, first_case, days_infected columns to front
imm_clean <- imm_clean %>%
  arrange(desc(total_cases)) %>%
  relocate(c(total_cases, first_case, days_infected, per_day), 
           .after = "Facility")

# Create a sparse version with just relevant variables
imm_sparse <- imm_clean %>%
  select(Facility, total_cases, first_case, days_infected, per_day)

# Read in static data; clean that data
static <- read_csv("~cdkma/Documents/Machine Learning/data/ice_static.csv")
colnames(static)[13] <- "adp_09_20"
colnames(static)[19] <- c("adp_01_21")

stat_clean <- static %>%
  mutate(Facility = trimws(Facility), Facility = tolower(Facility),
         adp_20 = `FY20 Male Crim` + `FY20 Male Non-Crim` + `FY20 Female Crim`
                + `FY20 Female Non-Crim`,
         adp_21 = `FY21 Male Crim` + `FY21 Male Non-Crim` + `FY21 Female Crim`
         + `FY21 Female Non-Crim`,
  ) %>%
  select(Facility, `ICE Dedicated /Non-Dedicated`, Address, 
         `ICE Field Office`, State, `Contract Type`, `Facility Operator`,
         `FY20 ALOS`, `FY21 ALOS`, adp_20, adp_21, adp_09_20, adp_01_21,
         `FY20 Guaranteed Min`, `FY21 Guaranteed Min`)

colnames(stat_clean) <- c("Facility", "ded_non", "address", "field_office", 
                          "state", "contract", "operator", "alos_20", "alos_21",
                          "adp_20", "adp_21", "adp_09_20", "adp_01_21","min_20", 
                          "min_21")

# impute variables
stat_clean <- stat_clean %>%
  mutate(adp_20 = ifelse(!is.na(adp_20), adp_20, adp_09_20),
         adp_21 = ifelse(!is.na(adp_21), adp_21, adp_01_21),
         adp_20 = ifelse(is.na(adp_20), adp_21, adp_20),
         adp_21 = ifelse(is.na(adp_21), adp_20, adp_21),
         weighted_adp = ifelse(!is.na(adp_20) | !is.na(adp_21), 
                               0.64*adp_20 + 0.36*adp_21, adp_20),
         min_20 = ifelse(is.na(min_20), 0, min_20),
         min_21 = ifelse(is.na(min_21), 0, min_21),
         weighted_min =  0.64*min_20 + 0.36*min_21,
         alos_20 = ifelse(is.na(alos_20), alos_21, alos_20),
         alos_21 = ifelse(is.na(alos_21), alos_20, alos_21),
         weighted_alos = ifelse(!is.na(alos_20) | !is.na(alos_21),
                                0.64*alos_20 + 0.36*alos_21, alos_20))

# create a sparse static dataset
sparse_static <- stat_clean %>%
  select(Facility, ded_non, address, field_office, state, contract, 
         operator, weighted_alos, weighted_adp, weighted_min)

# Merge static and covid data
final <- left_join(imm_sparse, sparse_static, by = "Facility")

# Drop problematic cases and unnecessary columns
final <- final %>%
  filter(!is.na(state), !is.na(weighted_adp), weighted_adp >= 1) %>%
  select(-first_case, -address) %>%
  mutate(days_infected = ifelse(is.na(days_infected), 0, days_infected),
         per_day = if_else(is.na(per_day), 0, per_day))

# Create Y
final <- final %>% 
  mutate(per_capita = total_cases/weighted_adp) %>%
  relocate(per_capita, .after = Facility)

# Add in state-specific info
states <- read_csv("~cdkma/Documents/Machine Learning/data/states.csv")
states <- states %>%
  select(-comments)
final <- left_join(final, states, by = "state")

# Add in county info
counties <- read_csv("~cdkma/Documents/Machine Learning/data/counties.csv")
final <- left_join(final, counties, by = "Facility")

# Final NA wrangling
final <- final %>%
  mutate(house = ifelse(is.na(house), "None", house),
         weighted_alos = 
           ifelse(is.na(weighted_alos), 
                  median(final$weighted_alos, na.rm = TRUE),
                  weighted_alos))

# Output clean dataset
write_csv(final, 
          "~cdkma/Documents/Machine Learning/data/final_data.csv")
