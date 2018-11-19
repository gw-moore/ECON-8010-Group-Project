# Set working directory
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")

# Load packages
library(jsonlite)
library(tidyverse)

# Load data
state_abb <- state.abb
state_name <- state.name

#######################
## REAL GDP          ##
#######################

## DATA IN IS MILLIONS OF DOLLARS

# create api series codes
gdp_series_code <- rep('RGSP', 50)

# Join together state abb and series code to make fred series code
fred_api_codes <- paste0(state_abb, gdp_series_code)

# create data frame of fred code and state name
codes_df <- data_frame(fred_api_codes, state_name)
colnames(codes_df) <- c('fred_api_code', 'state_name')

# Initilize empty list
datalist <- list()

# For loop to loop over msa_codes and query GDP data from FRED
for(fred_api_code in fred_api_codes) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', fred_api_code,'&api_key=a2541dacf2fe0876e9ad7748fc97a381&file_type=json')
  
  # Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  # Convert json_list to data frame
  df <- as_data_frame(json_list$observations)
  # Remove first two columns
  df <- df[,-c(1:2)]
  # Change column name of df
  colnames(df) <- c('year', 'gdp_in_millions')
  # strip off unneed characters from year
  df$year <- gsub('-01-01', '', df$year)
  # change year to numeric
  df$year <- as.numeric(df$year)
  # Add State code and name to df
  df$fred_api_code <- fred_api_code
  # Convert gdp column to numeric
  df$gdp_in_millions <- as.numeric(df$gdp_in_millions)
  
  # Save df to list
  datalist[[fred_api_code]] <- df
}

# Extract data from datalist into dataframe
real_gdp_data <- do.call(rbind, datalist)

# join state name to real_gdp_data
real_gdp_data <- real_gdp_data %>% 
  inner_join(codes_df)

# drop fred_api_code
real_gdp_data$fred_api_code <- NULL

# remove row names
rownames(real_gdp_data) <- NULL

# drop unneed years
real_gdp_data <- real_gdp_data %>% filter(year > 2005)

# Save data frame
save(real_gdp_data, file = "programs/prepped_data/real_gdp_data.rda")



##########################################
##  PER CAPITA PERSONAL IMCOME         ##
##########################################

## DATA IS IN DOLARS

# create api series codes
pcpi_series_code <- rep('PCPI', 50)

# Join together state abb and series code to make fred series code
fred_api_codes <- paste0(state_abb, pcpi_series_code)

# create data frame of fred code and state name
codes_df <- data_frame(fred_api_codes, state_name)
colnames(codes_df) <- c('fred_api_code', 'state_name')

# Initilize empty list
datalist <- list()

# For loop to loop over msa_codes and query GDP data from FRED
for(fred_api_code in fred_api_codes) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', fred_api_code,'&api_key=a2541dacf2fe0876e9ad7748fc97a381&file_type=json')
  
  # Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  # Convert json_list to data frame
  df <- as_data_frame(json_list$observations)
  # Remove first two columns
  df <- df[,-c(1:2)]
  # remove null values
  df <- df %>% filter(!value == '.')
  # Change column name of df
  colnames(df) <- c('year', 'per_capita_personal_income')
  # strip off unneed characters from year
  df$year <- gsub('-01-01', '', df$year)
  # change year to numeric
  df$year <- as.numeric(df$year)
  # Add State code and name to df
  df$fred_api_code <- fred_api_code
  # Convert gdp column to numeric
  df$per_capita_personal_income <- as.numeric(df$per_capita_personal_income)
  
  # Save df to list
  datalist[[fred_api_code]] <- df
}

# Extract data from datalist into dataframe
per_capita_personal_income_data <- do.call(rbind, datalist)

# join state name to data
per_capita_personal_income_data <- per_capita_personal_income_data %>% 
  inner_join(codes_df)

# drop fred_api_code
per_capita_personal_income_data$fred_api_code <- NULL

# remove row names
rownames(per_capita_personal_income_data) <- NULL

# drop unneeded years
per_capita_personal_income_data <- per_capital_personal_income_data %>% filter(year > 2005)

# Save data frame
save(per_capita_personal_income_data, file = "programs/prepped_data/per_capita_personal_income_data.rda")


##########################################
##  STATE MINIMUM WAGE                  ##
##########################################

## DATA IS IN DOLARS PER HOUR

# create api series codes
min_wage_series_code <- rep('STTMINWG', 50)

# Join together state abb and series code to make fred series code
fred_api_codes <- paste0(min_wage_series_code, state_abb)

# create data frame of fred code and state name
codes_df <- data_frame(fred_api_codes, state_name)
colnames(codes_df) <- c('fred_api_code', 'state_name')

# Extra step for min wage data
# alabama, louisiana, mississippi, south carolina, and tennessee do not have state minimum wage
# removing these states from fred_api_code so for loop and run without failing
# then will add back these states with hard coded min wage values of 7.25, the federal min wage
codes_df <- codes_df %>% filter(!state_abb %in% c('AL', 'TN', 'MS', 'LA', 'SC'))

fred_api_codes <- pull(codes_df, fred_api_code)

# Initilize empty list
datalist <- list()

# For loop to loop over msa_codes and query GDP data from FRED
for(fred_api_code in fred_api_codes) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', fred_api_code,'&api_key=a2541dacf2fe0876e9ad7748fc97a381&file_type=json')
  
  # Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  # Convert json_list to data frame
  df <- as_data_frame(json_list$observations)
  # Remove first two columns
  df <- df[,-c(1:2)]
  # remove null values
  df <- df %>% filter(!value == '.')
  # Change column name of df
  colnames(df) <- c('year', 'state_min_wage_rate')
  # strip off unneed characters from year
  df$year <- gsub('-01-01', '', df$year)
  # change year to numeric
  df$year <- as.numeric(df$year)
  # Add State code and name to df
  df$fred_api_code <- fred_api_code
  # Convert gdp column to numeric
  df$state_min_wage_rate <- as.numeric(df$state_min_wage_rate)
  
  # Save df to list
  datalist[[fred_api_code]] <- df
}

# Extract data from datalist into dataframe
state_min_wage_data <- do.call(rbind, datalist)

# join state name to data
state_min_wage_data <- state_min_wage_data %>% 
  inner_join(codes_df)

# drop fred_api_code
state_min_wage_data$fred_api_code <- NULL

# remove row names
rownames(state_min_wage_data) <- NULL

# drop unneeded years
state_min_wage_data <- state_min_wage_data %>% filter(year > 2005)

# add back removed states
years <- data_frame(c(2006:2017))
years$fake <- 1
state_names <- data_frame(c('Alabama', 'Louisiana', 'Mississippi', 'South Carolina' , 'Tennessee'))
state_names$fake <- 1
removed_states_data <- full_join(years, state_names) %>% select(-fake)
colnames(removed_states_data) <- c('year', 'state_name')
removed_states_data <- arrange(removed_states_data, state_name, year)
min_wages <- data_frame(rep(c(5.15, 5.85, 6.55, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25, 7.25), 5))
removed_states_data <- cbind(removed_states_data, min_wages)
colnames(removed_states_data) <- c('year', 'state_name', 'state_min_wage_rate')

# rbind removed states to full data frame
state_min_wage_data <- rbind(state_min_wage_data, removed_states_data)

# rearrange
state_min_wage_data <- arrange(state_min_wage_data, state_name, year)

# Save data frame
save(state_min_wage_data, file = "programs/prepped_data/state_min_wage_data.rda")



################################################
##  PERCENT WITH BACHELOR'S DEGREE OR HIGHER  ##
################################################

## DATA IS IN PERCENTS

# create api series codes
bach_deg_code <- rep('GCT1502', 50)

# Join together state abb and series code to make fred series code
fred_api_codes <- paste0(bach_deg_code, state_abb)

# create data frame of fred code and state name
codes_df <- data_frame(fred_api_codes, state_name)
colnames(codes_df) <- c('fred_api_code', 'state_name')

# Initilize empty list
datalist <- list()

# For loop to loop over msa_codes and query GDP data from FRED
for(fred_api_code in fred_api_codes) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', fred_api_code,'&api_key=a2541dacf2fe0876e9ad7748fc97a381&file_type=json')
  
  # Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  # Convert json_list to data frame
  df <- as_data_frame(json_list$observations)
  # Remove first two columns
  df <- df[,-c(1:2)]
  # remove null values
  df <- df %>% filter(!value == '.')
  # Change column name of df
  colnames(df) <- c('year', 'perc_w_bach_deg_or_higher')
  # strip off unneed characters from year
  df$year <- gsub('-01-01', '', df$year)
  # change year to numeric
  df$year <- as.numeric(df$year)
  # Add State code and name to df
  df$fred_api_code <- fred_api_code
  # Convert gdp column to numeric
  df$perc_w_bach_deg_or_higher <- as.numeric(df$perc_w_bach_deg_or_higher)
  
  # Save df to list
  datalist[[fred_api_code]] <- df
}

# Extract data from datalist into dataframe
bachelors_degree_or_higher_data <- do.call(rbind, datalist)

# join state name to data
bachelors_degree_or_higher_data <- bachelors_degree_or_higher_data %>% 
  inner_join(codes_df)

# drop fred_api_code
bachelors_degree_or_higher_data$fred_api_code <- NULL

# remove row names
rownames(bachelors_degree_or_higher_data) <- NULL

# drop unneeded years
bachelors_degree_or_higher_data <- bachelors_degree_or_higher_data %>% filter(year > 2005)

# Save data frame
save(bachelors_degree_or_higher_data, file = "programs/prepped_data/bachelors_degree_or_higher_data.rda")


################################################
##  RESIDENT POPULATION   ##
################################################

## DATA IN THOUSUNDS OF PERSONS, NOT SEASONLLY ADJUSTED

# create api series codes
pop_code <- rep('POP', 50)

# Join together state abb and series code to make fred series code
fred_api_codes <- paste0(state_abb, pop_code)

# create data frame of fred code and state name
codes_df <- data_frame(fred_api_codes, state_name)
colnames(codes_df) <- c('fred_api_code', 'state_name')

# Initilize empty list
datalist <- list()

# For loop to loop over msa_codes and query GDP data from FRED
for(fred_api_code in fred_api_codes) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', fred_api_code,'&api_key=a2541dacf2fe0876e9ad7748fc97a381&file_type=json')
  
  # Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  # Convert json_list to data frame
  df <- as_data_frame(json_list$observations)
  # Remove first two columns
  df <- df[,-c(1:2)]
  # remove null values
  df <- df %>% filter(!value == '.')
  # Change column name of df
  colnames(df) <- c('year', 'population')
  # strip off unneed characters from year
  df$year <- gsub('-01-01', '', df$year)
  # change year to numeric
  df$year <- as.numeric(df$year)
  # Add State code and name to df
  df$fred_api_code <- fred_api_code
  # Convert gdp column to numeric
  df$population <- as.numeric(df$population)
  
  # Save df to list
  datalist[[fred_api_code]] <- df
}

# Extract data from datalist into dataframe
population_data <- do.call(rbind, datalist)

# join state name to data
population_data <- population_data %>% 
  inner_join(codes_df)

# drop fred_api_code
population_data$fred_api_code <- NULL

# remove row names
rownames(population_data) <- NULL

# drop unneeded years
population_data <- population_data %>% filter(year > 2005)

# Save data frame
save(population_data, file = "programs/prepped_data/population_data.rda")
