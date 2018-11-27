setwd("~/GitHub/ECON-8010-Group-Project/programs/prepped_data")

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

modeling_data <- as.tibble(modeling_data)

# save data
save(modeling_data, file = "modeling_data.rda")


