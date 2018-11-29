setwd("~/GitHub/ECON-8010-Group-Project/programs/prepped_data")
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/programs/prepped_data")

# Load libraries
library(tidyverse)

# Load prepped data
load('yearly_unemployment_data.rda')#
load('gini_index_data.rda')# 
load('bachelors_degree_or_higher_data.rda')#
load('civilian_labor_force_data.rda')#
load('per_capita_personal_income_data.rda')#
load('population_data.rda')#
load('real_gdp_data.rda')#
load('state_min_wage_data.rda')#
load('union_participation_data.rda')#
load('homeownership_rate_data.rda')#


#obs = current population survey sample size
#covered = covered by collective bargaining
#members = number of members in a union

# mutate union_participation to get percent of union employees
union_participation_data <- union_participation_data %>% mutate()

# Join data frames together to make modeling data frame
union_df <- union_participation_data %>% filter(Sector == 'Total') %>% select(State, year, X.Mem, X.Cov)
colnames(union_df) <- c('state_name', 'year', 'percent_union_members', 'percent_collective')

gini_df <- gini_index_data %>% select(state_name, year, gini_index)

# join all data sets together
modeling_data <- union_df %>% 
  inner_join(gini_df) %>% 
  inner_join(population_data) %>% 
  inner_join(real_gdp_data) %>% 
  inner_join(state_min_wage_data) %>% 
  inner_join(per_capita_personal_income_data) %>% 
  inner_join(civilian_labor_force_data) %>% 
  inner_join(bachelors_degree_or_higher_data) %>% 
  inner_join(yearly_unemployment_data) %>% 
  inner_join(homeownership_rate_data)

# save as tibble
modeling_data <- as.tibble(modeling_data)

# mutate data to add log and squared terms 
modeling_data <- modeling_data %>% mutate(percent_union_members_logged = log(percent_union_members),
                                          percent_union_members_squared = percent_union_members^2,
                                          percent_collective_logged = log(percent_collective),
                                          percent_collective_squared = percent_collective^2,
                                          population_logged = log(population),
                                          population_squared = population^2,
                                          gdp_in_millions_logged = log(gdp_in_millions),
                                          gdp_in_millions_squared = gdp_in_millions^2,
                                          state_min_wage_rate_logged = log(state_min_wage_rate),
                                          state_min_wage_rate_squared = state_min_wage_rate^2,
                                          per_capital_personal_income_logged = log(per_capital_personal_income),
                                          per_capital_personal_income_squared = per_capital_personal_income^2,
                                          yearly_avg_clf_logged = log(yearly_avg_clf),
                                          yearly_avg_clf_squared = yearly_avg_clf^2,
                                          perc_w_bach_deg_or_higher_logged = log(perc_w_bach_deg_or_higher),
                                          perc_w_bach_deg_or_higher_squared = perc_w_bach_deg_or_higher^2,
                                          yearly_avg_unemply_rate_logged = log(yearly_avg_unemply_rate),
                                          yearly_avg_unemply_rate_squared = yearly_avg_unemply_rate^2,
                                          homeownership_rate_logged = log(homeownership_rate),
                                          homeownership_rate_squared = homeownership_rate^2)

# mutate data to lag values by one year
modeling_data <- modeling_data %>%
  group_by(state_name) %>% 
  mutate(lagged_percent_union_members = dplyr::lag(percent_union_members, n = 1, default = NA),
         lagged_percent_collective = dplyr::lag(percent_collective, n = 1, default = NA),
         lagged_population = dplyr::lag(population, n = 1, default = NA),
         lagged_gdp_in_millions = dplyr::lag(gdp_in_millions, n = 1, default = NA),
         lagged_state_min_wage_rate = dplyr::lag(state_min_wage_rate, n = 1, default = NA),
         lagged_per_capital_personal_income = dplyr::lag(per_capital_personal_income, n = 1, default = NA),
         lagged_yearly_avg_clf = dplyr::lag(yearly_avg_clf, n = 1, default = NA),
         lagged_perc_w_bach_deg_or_higher = dplyr::lag(perc_w_bach_deg_or_higher, n = 1, default = NA),
         lagged_yearly_avg_unemply_rate = dplyr::lag(yearly_avg_unemply_rate, n = 1, default = NA),
         lagged_homeownership_rate = dplyr::lag(homeownership_rate, n = 1, default = NA),
         lagged_percent_union_members_logged = dplyr::lag(percent_union_members_logged, n = 1, default = NA),
         lagged_percent_union_members_squared = dplyr::lag(percent_union_members_squared, n = 1, default = NA),
         lagged_percent_collective_logged = dplyr::lag(percent_collective_logged, n = 1, default = NA),
         lagged_percent_collective_squared = dplyr::lag(percent_collective_squared, n = 1, default = NA),
         lagged_population_logged = dplyr::lag(population_logged, n = 1, default = NA),
         lagged_population_squared = dplyr::lag(population_squared, n = 1, default = NA),
         lagged_gdp_in_millions_logged = dplyr::lag(gdp_in_millions_logged, n = 1, default = NA),
         lagged_gdp_in_millions_squared = dplyr::lag(gdp_in_millions_squared, n = 1, default = NA),
         lagged_state_min_wage_rate_logged = dplyr::lag(state_min_wage_rate_logged, n = 1, default = NA),
         lagged_state_min_wage_rate_squared = dplyr::lag(state_min_wage_rate_squared, n = 1, default = NA),
         lagged_per_capital_personal_income_logged = dplyr::lag(per_capital_personal_income_logged, n = 1, default = NA),
         lagged_per_capital_personal_income_squared = dplyr::lag(per_capital_personal_income_squared, n = 1, default = NA),
         lagged_yearly_avg_clf_logged = dplyr::lag(yearly_avg_clf_logged, n = 1, default = NA),
         lagged_yearly_avg_clf_squared = dplyr::lag(yearly_avg_clf_squared, n = 1, default = NA),
         lagged_perc_w_bach_deg_or_higher_logged = dplyr::lag(perc_w_bach_deg_or_higher_logged, n = 1, default = NA),
         lagged_perc_w_bach_deg_or_higher_squared = dplyr::lag(perc_w_bach_deg_or_higher_squared, n = 1, default = NA),
         lagged_yearly_avg_unemply_rate_logged = dplyr::lag(yearly_avg_unemply_rate_logged, n = 1, default = NA),
         lagged_yearly_avg_unemply_rate_squared = dplyr::lag(yearly_avg_unemply_rate_squared, n = 1, default = NA),
         lagged_homeownership_rate_logged = dplyr::lag(homeownership_rate_logged, n = 1, default = NA),
         lagged_homeownership_rate_squared = dplyr::lag(homeownership_rate_squared, n = 1, default = NA))

# QA Lagged Variables
modeling_data %>% select(percent_union_members, lagged_percent_union_members)
modeling_data %>% select(population, lagged_population)
modeling_data %>% select(gdp_in_millions, lagged_gdp_in_millions)


# save data
save(modeling_data, file = "modeling_data.rda")


