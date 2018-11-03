#Set working directory
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")
#Load packages
library(tidyverse)
library(devtools)
devtools::install_github("keberwein/blscrapeR")
library(blscrapeR)

####################################################
##BLS API ###
####################################################

##Setup API key
set_bls_key("441f2f4e7c144801839f361660ea0233")
# First time, reload your enviornment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("BLS_KEY")

# The BLS assign state codes alphabeticially starting at 01
state_names <- c('Alabama',	'Alaska',	'Arizona',	'Arkansas',	'California',	'Colorado',	'Connecticut',	'Delaware',	'District of Columbia',	'Florida',	'Georgia',	'Hawaii',	'Idaho',	'Illinois',	'Indiana',	'Iowa',	'Kansas',	'Kentucky',	'Louisiana',	'Maine',	'Maryland',	'Massachusetts',	'Michigan',	'Minnesota',	'Mississippi',	'Missouri',	'Montana',	'Nebraska',	'Nevada',	'New Hampshire',	'New Jersey',	'New Mexico',	'New York',	'North Carolina',	'North Dakota',	'Ohio',	'Oklahoma',	'Oregon',	'Pennsylvania',	'Rhode Island',	'South Carolina',	'South Dakota',	'Tennessee',	'Texas',	'Utah',	'Vermont',	'Virginia',	'Washington',	'West Virginia',	'Wisconsin',	'Wyoming',	'Puerto Rico'
)
state_codes <- c( '01',	'02',	'04',	'05',	'06',	'08',	'09',	'10',	'11',	'12',	'13',	'15',	'16',	'17',	'18',	'19',	'20',	'21',	'22',	'23',	'24',	'25',	'26',	'27',	'28',	'29',	'30',	'31',	'32',	'33',	'34',	'35',	'36',	'37',	'38',	'39',	'40',	'41',	'42',	'44',	'45',	'46',	'47',	'48',	'49',	'50',	'51',	'53',	'54',	'55',	'56',	'72')

state_lookup <- data.frame(state_names, state_codes)

# The BLS uses series codes to identify data
# Each code begins with alpha characters, then a state code, then a long numeric of mostly zeros
# I will loop through the state_lookup date frame and pull data for each state and store it in a list

###############################################
# Unemployment data - Not seasonally adjusted #
###############################################

# Initilize empty list to store results for each state
unemply_datalist <- list()

# For loop to loop through each state code in state_lookup
for(i in 1:nrow(state_lookup)){

# Setup series names
series_prefix <- 'LAUST'
data_series_string_00 <- paste0(series_prefix, state_lookup[i,2], '0000000000003')
data_series_string_01 <- paste0(series_prefix, state_lookup[i,2], '0000000000004')
data_series_string_02 <- paste0(series_prefix, state_lookup[i,2], '0000000000005')
data_series_string_03 <- paste0(series_prefix, state_lookup[i,2], '0000000000006')

data_series_vec <- c(data_series_string_00, data_series_string_01, data_series_string_02, data_series_string_03)

# Call to BLS API
df <- bls_api(data_series_vec, startyear = 2008, endyear = 2016, registrationKey = Sys.getenv("BLS_KEY")) %>%
  spread(seriesID, value) %>%
  dateCast()

#Drop unneeded columns
df <- df %>% select(-period, -footnotes, -date)

# Rename columns
colnames(df) <- c('year', 'month', 'unemployment_rate', 'unemployment', 'employment' , 'labor_force')

df$state <- state_lookup[i,1]

# Store results from API call
unemply_datalist[[i]] <- df

}

#Extract data from datalist into dataframe
unemployment_data_raw <- do.call(rbind, unemply_datalist)

#Save data frame for future loading to save time
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input")
save(unemployment_data_raw, file = "unemployment_data_raw.rda")

# Yearly unemployment metrics - Avg of year/state
yearly_unemployment_data <- unemployment_data_raw %>% 
  group_by(year, state) %>% 
  mutate(avg_unemployment_rate = mean(unemployment_rate),
         avg_unemployment = mean(unemployment),
         avg_employment = mean(employment),
         avg_labor_force = mean(labor_force)) %>% 
  ungroup() %>% 
  distinct(state, year, avg_unemployment_rate, avg_unemployment, avg_employment, avg_labor_force)

save(yearly_unemployment_data, file = "yearly_unemployment_data.rda")

