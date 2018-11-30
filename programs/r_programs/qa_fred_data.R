# setup envrionmnent 
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/programs/prepped_data")
library(tidyverse)

# import QA data
ny_rgdp <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_rgdp.csv", stringsAsFactors = F)
ny_civilian_labor_force <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_civilian_labor_force.csv", stringsAsFactors = F)
ny_per_capita_personal_income <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_per_capita_personal_income.csv", stringsAsFactors = F)
ny_perc_bach_or_higher <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_perc_bach_or_higher.csv", stringsAsFactors = F)
ny_pop <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_pop.csv", stringsAsFactors = F)
ny_state_min_wage <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_state_min_wage.csv", stringsAsFactors = F)
ny_unemployment_rate <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_unemployment_rate.csv", stringsAsFactors = F)
ny_homeownership_rate <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/qa_fred_data/ny_homeownership_rate.csv", stringsAsFactors = F)

# import api data
load('yearly_unemployment_data.rda')#
load('bachelors_degree_or_higher_data.rda')#
load('civilian_labor_force_data.rda')#
load('per_capita_personal_income_data.rda')#
load('population_data.rda')#
load('real_gdp_data.rda')#
load('state_min_wage_data.rda')#
load('homeownership_rate_data.rda')#

# edit QA files so that format matches data pulled from FRED API

######################
# GDP
df <- ny_rgdp
# Change column name of df
colnames(df) <- c('year', 'gdp_in_millions')
# strip off unneed characters from year
df$year <- gsub('-01-01', '', df$year)
# add state name 
df$state_name <- 'New York'
# change year to number
df$year <- as.numeric(df$year)
# removed unneed years
df <- df %>% filter(year > 2005)
# assign back to original table name
ny_rgdp <- df

######################
# CLF
df <- ny_civilian_labor_force
# Change column name of df
colnames(df) <- c('year', 'civilian_labor_force')
# strip off unneed characters from year
df$year <- gsub('-\\d*', '', df$year)
# add state name 
df$state_name <- 'New York'
# average by year
df <- df %>% group_by(year) %>% mutate(yearly_avg_clf = mean(civilian_labor_force))
# drop CLF var
df <- df %>% select(-civilian_labor_force) %>% distinct()
# change year to number
df$year <- as.numeric(df$year)
# removed unneeded years
df <- df %>% filter(year > 2005)
df <- df %>% filter(year != 2018)
# assign back to original table name
ny_civilian_labor_force <- df

########################
# ny_unemployment_rate
df <- ny_unemployment_rate
# Change column name of df
colnames(df) <- c('year', 'unemployment_rate')
# strip off unneed characters from year
df$year <- gsub('-\\d*', '', df$year)
# add state name 
df$state_name <- 'New York'
# average by year
df <- df %>% group_by(year) %>% mutate(yearly_avg_unemply_rate = mean(unemployment_rate))
# drop CLF var
df <- df %>% select(-unemployment_rate) %>% distinct()
# change year to number
df$year <- as.numeric(df$year)
# removed unneeded years
df <- df %>% filter(year > 2005)
df <- df %>% filter(year != 2018)
# assign back to original table name
ny_unemployment_rate <- df

########################
# PCPI
df <- ny_per_capita_personal_income
# Change column name of df
colnames(df) <- c('year', 'per_capital_personal_income')
# strip off unneed characters from year
df$year <- gsub('-01-01', '', df$year)
# add state name 
df$state_name <- 'New York'
# change year to number
df$year <- as.numeric(df$year)
# removed unneeded years
df <- df %>% filter(year > 2005)
df <- df %>% filter(year != 2018)
# convert measure to number
df$per_capital_personal_income <- as.numeric(df$per_capital_personal_income)
# assign back to original table name
ny_per_capita_personal_income <- df

########################
# % w/ bach or higher
df <- ny_perc_bach_or_higher
# Change column name of df
colnames(df) <- c('year', 'perc_w_bach_deg_or_higher')
# strip off unneed characters from year
df$year <- gsub('-01-01', '', df$year)
# add state name 
df$state_name <- 'New York'
# change year to number
df$year <- as.numeric(df$year)
# removed unneeded years
df <- df %>% filter(year > 2005)
df <- df %>% filter(year != 2018)
# assign back to original table name
ny_perc_bach_or_higher <- df

########################
# population
df <- ny_pop
# Change column name of df
colnames(df) <- c('year', 'population')
# strip off unneed characters from year
df$year <- gsub('-01-01', '', df$year)
# add state name 
df$state_name <- 'New York'
# change year to number
df$year <- as.numeric(df$year)
# removed unneeded years
df <- df %>% filter(year > 2005)
df <- df %>% filter(year != 2018)
# assign back to original table name
ny_pop <- df

########################
# state_min_wage_rate
df <- ny_state_min_wage
# Change column name of df
colnames(df) <- c('year', 'state_min_wage_rate')
# strip off unneed characters from year
df$year <- gsub('-01-01', '', df$year)
# add state name 
df$state_name <- 'New York'
# change year to number
df$year <- as.numeric(df$year)
# removed unneeded years
df <- df %>% filter(year > 2005)
df <- df %>% filter(year != 2018)
# assign back to original table name
ny_state_min_wage <- df

########################
# ny_homeownership_rate
df <- ny_homeownership_rate
# Change column name of df
colnames(df) <- c('year', 'homeownership_rate')
# strip off unneed characters from year
df$year <- gsub('-01-01', '', df$year)
# add state name 
df$state_name <- 'New York'
# change year to number
df$year <- as.numeric(df$year)
# removed unneeded years
df <- df %>% filter(year > 2005)
df <- df %>% filter(year != 2018)
# assign back to original table name
ny_homeownership_rate <- df


# compare csvs to api data
load('homeownership_rate_data.rda')#

# real_gdp_data
setequal(real_gdp_data %>% filter(state_name == 'New York'), ny_rgdp)
# yearly_unemployment_data
setequal(yearly_unemployment_data %>% ungroup() %>% filter(state_name == 'New York'), ny_unemployment_rate)
# bachelors_degree_or_higher_data
setequal(bachelors_degree_or_higher_data %>% filter(state_name == 'New York'), ny_perc_bach_or_higher)
# civilian_labor_force_data
setequal(civilian_labor_force_data %>% filter(state_name == 'New York'), ny_civilian_labor_force)
# per_capita_personal_income_data
setequal(per_capita_personal_income_data %>% filter(state_name == 'New York'), ny_per_capita_personal_income)
# population_data
setequal(population_data %>% filter(state_name == 'New York'), ny_pop)
# state_min_wage_data
setequal(state_min_wage_data %>% filter(state_name == 'New York'), ny_state_min_wage)
# state_min_wage_data
setequal(homeownership_rate_data %>% filter(state_name == 'New York'), ny_homeownership_rate)

