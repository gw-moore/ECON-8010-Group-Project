setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")

# Load libraries
library(tidyverse)

# Load prepped data
load('programs/prepped_data/yearly_unemployment_data.rda')
load('programs/prepped_data/gini_index_data.rda')
load('programs/prepped_data/bachelors_degree_or_higher_data.rda')
load('programs/prepped_data/civilian_labor_force_data.rda')
load('programs/prepped_data/per_capita_personal_income_data.rda')
load('programs/prepped_data/population_data.rda')
load('programs/prepped_data/real_gdp_data.rda')
load('programs/prepped_data/state_min_wage_data.rda')
load('programs/prepped_data/union_participation_data.rda')

# mutate union_participation to get percent of union employees
union_participation_data <- union_participation_data %>% mutate()

# Join data frames together to make modeling data frame
union_df <- union_participation_data %>% filter(Sector == 'Total') %>% select(State, year, X.Mem)
colnames(union_df) <- c('state_name', 'year', 'percent_union_members')

gini_df <- gini_index_data %>% select(state_name, year, gini_index)
  
modeling_data <- union_df %>% 
  inner_join(gini_df) 

# join modeling data to population_data
modeling_data <- modeling_data %>% 