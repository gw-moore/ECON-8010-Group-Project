# setup envrionmnent 
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")
library(tidyverse)
library(plm)

# load data
load('programs/prepped_data/modeling_data.rda')

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
                                          homeownership_rate_squared = homeownership_rate^2
                                          )

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

# QA
modeling_data %>% select(percent_union_members, lagged_percent_union_members)
modeling_data %>% select(population, lagged_population)
modeling_data %>% select(gdp_in_millions, lagged_gdp_in_millions)


# make formual for models

nonlagged_formula <- gini_index ~ percent_union_members + population + gdp_in_millions + state_min_wage_rate + per_capital_personal_income +  perc_w_bach_deg_or_higher + yearly_avg_unemply_rate + homeownership_rate

lagged_formula <- gini_index ~ lagged_percent_union_members + lagged_gdp_in_millions + lagged_state_min_wage_rate + lagged_perc_w_bach_deg_or_higher + lagged_yearly_avg_unemply_rate + lagged_homeownership_rate

# fit models

# pooled regression model
pooled_reg <- plm(data = modeling_data, formula = lagged_formula, index = c('state_name', 'year'), model = 'pooling')

summary(pooled_reg)

# fixed effects model
fix_effects_reg <- plm(data = modeling_data, formula = lagged_formula, index = c('state_name', 'year'), model = 'within', effect = 'individual')
summary(fix_effects_reg)

# fixed effect with time
fix_effects_time <- plm(data = modeling_data, formula = gini_index ~ lagged_percent_union_members + lagged_gdp_in_millions + lagged_state_min_wage_rate + lagged_perc_w_bach_deg_or_higher + lagged_yearly_avg_unemply_rate + lagged_homeownership_rate + factor(year) + factor(state_name), index = c('state_name', 'year'), model = 'within', effect = 'individual')

summary(fix_effects_time)

# print out fixed effect for states
fixef(fix_effects_reg)

# compare fixed and fixed time
pFtest(fix_effects_time, fix_effects_reg)

# compare pooled and fixed effects
pFtest(fix_effects_reg, pooled_reg) 
phtest(fix_effects_reg, pooled_reg)

# test for time-fixed effects
plmtest(fix_effects_reg, c("time"), type=("bp"))
