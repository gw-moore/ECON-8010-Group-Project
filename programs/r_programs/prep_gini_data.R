setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")

# Load libraries
library(tidyverse)

# Read in gini index csvs
gini_2006 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_06_EST_B19083.csv", header=TRUE)
gini_2007 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_07_1YR_B19083.csv", header=TRUE)
gini_2008 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_08_1YR_B19083.csv", header=TRUE)
gini_2009 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_09_1YR_B19083.csv", header=TRUE)
gini_2010 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_10_1YR_B19083.csv", header=TRUE)
gini_2011 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_11_1YR_B19083.csv", header=TRUE)
gini_2012 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_12_1YR_B19083.csv", header=TRUE)
gini_2013 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_13_1YR_B19083.csv", header=TRUE)
gini_2014 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_14_1YR_B19083.csv", header=TRUE)
gini_2015 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_15_1YR_B19083.csv", header=TRUE)
gini_2016 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_16_1YR_B19083.csv", header=TRUE)
gini_2017 <- read.csv("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project/input/gini_index/ACS_17_1YR_B19083.csv", header=TRUE)

gini_index_data <- tibble()

# function to edit data
edit_table <- function(table_name, year){
  # assign table to function varible
  data <- table_name
  # remove first row
  data <- data[-1,]
  # add year variable
  data$year <- year
  # change name of columns
  colnames(data) <- c('geo_id', 'geo_id2', 'state_name', 'gini_index', 'gini_std_err', 'year')
  # save to datalist
  return(data)
}

# call the function for each table
gini_2006 <- edit_table(gini_2006, 2006)
gini_2007 <- edit_table(gini_2007, 2007)
gini_2008 <- edit_table(gini_2008, 2008)
gini_2009 <- edit_table(gini_2009, 2009)
gini_2010 <- edit_table(gini_2010, 2010)
gini_2011 <- edit_table(gini_2011, 2011)
gini_2012 <- edit_table(gini_2012, 2012)
gini_2013 <- edit_table(gini_2013, 2013)
gini_2014 <- edit_table(gini_2014, 2014)
gini_2015 <- edit_table(gini_2015, 2015)
gini_2016 <- edit_table(gini_2016, 2016)
gini_2017 <- edit_table(gini_2017, 2017)

# bind all dataset together
gini_index_data <- rbind(gini_2006, gini_2007, gini_2008, gini_2009, gini_2010, gini_2011, gini_2012, gini_2013, gini_2014, gini_2015, gini_2016, gini_2017)

# save as tibble
gini_index_data <- as.tibble(gini_index_data)

# arrange by state and year
gini_index_data <- gini_index_data %>% arrange(state_name, year)

# save to prepped data
save(gini_index_data, file = 'programs/prepped_data/gini_index_data.rda')
