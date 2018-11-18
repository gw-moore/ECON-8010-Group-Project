# Set working directory
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")

# Load packages
library(jsonlite)
library(tidyverse)

# create api series codes
# Load data
state_abb <- state.abb
state_name <- state.name
n <- rep('N', 50)
gdp <- rep('GSP', 50)

# Join together
fred_api_code <- paste0(state_abb, n, gdp)

# create data frame of fred code and state name
codes_df <- data_frame(fred_api_code, state_name)

#########
## GDP ##
#########

# Initilize empty list
datalist <- list()

# For loop to loop over msa_codes and query GDP data from FRED
for(fred_code in fred_api_codes) {
  # Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=', fred_code,'&api_key=a2541dacf2fe0876e9ad7748fc97a381&file_type=json')
  
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
  df$fred_api_code <- fred_code
  # Convert gdp column to numeric
  df$gdp_in_millions <- as.numeric(df$gdp_in_millions)
  
  # Save df to list
  datalist[[fred_code]] <- df
}

# Extract data from datalist into dataframe
gdp_data <- do.call(rbind, datalist)

# join state name to gdp_data
gdp_data <- gdp_data %>% 
  inner_join(codes_df)

# drop fred_api_code
gdp_data$fred_api_code <- NULL

# drop unneed years
bad_years <- c('1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005')

gdp_data <- gdp_data %>% filter(!year %in% bad_years)

# Save data frame
save(gdp_data, file = "programs/prepped_data/gdp_data.rda")
