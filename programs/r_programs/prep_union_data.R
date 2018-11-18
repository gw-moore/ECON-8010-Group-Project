library(tidyverse)

# set wd

setwd("~/R Files/Econometrics Project/Cleaned Data")

# read in union participation data

up_2006 <- read.csv("union_participation_2006.csv")
up_2007 <- read.csv("union_participation_2007.csv")
up_2008 <- read.csv("union_participation_2008.csv")
up_2009 <- read.csv("union_participation_2009.csv")
up_2010 <- read.csv("union_participation_2010.csv")
up_2011 <- read.csv("union_participation_2011.csv")
up_2012 <- read.csv("union_participation_2012.csv")
up_2013 <- read.csv("union_participation_2013.csv")
up_2014 <- read.csv("union_participation_2014.csv")
up_2015 <- read.csv("union_participation_2015.csv")
up_2016 <- read.csv("union_participation_2016.csv")
up_2017 <- read.csv("union_participation_2017.csv")

# Adding year variable to each dataset
# Couldn't figure out a loop, so I did it the dumb shit way

year = 2006

up_2006$year = year

year = 2007

up_2007$year = year

year = 2008

up_2008$year = year

year = 2009

up_2009$year = year

year = 2010

up_2010$year = year

year = 2011

up_2011$year = year

year = 2012

up_2012$year = year

year = 2013

up_2013$year = year

year = 2014

up_2014$year = year

year = 2015

up_2015$year = year

year = 2016

up_2016$year = year

year = 2017

up_2017$year = year

# combine all data sets

union_participation_data <- rbind(up_2006, up_2007, up_2008, up_2009, up_2010, up_2011, up_2012, up_2013, up_2014, up_2015, up_2016, up_2017)

# save as tibble

as.tibble(union_participation_data)

# arrange by state and year

union_participation_data <- union_participation_data %>% arrange(State, year)

# save that shit

save(union_participation_data, file = 'union_participation_data.rda')


