#set working directory

setwd()

# Packages

#install.packages("ggmap")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("stargazer")
#install.packages("psych")
#install.packages("rdataviewer")
#install.packages("devtools")
#devtools::install_github("UrbanInstitute/urbnmapr")
#install.packages("ggjoy")
install.packages("ggridges")

library(ggplot2)
library(ggfortify)
library(changepoint)
library(strucchange)
library(ggpmisc)
library(tidyverse)
library(ggcorrplot)
library(directlabels)
library(ExPanDaR)
library(knitr)
library(naniar)
library(DataExplorer)
library(ggmap)
library(maps)
library(mapdata)
library(stargazer)
library(psych)
library(urbnmapr)
library(ggjoy)
library(ggridges)

##########################
# Structure
##########################

data<-modeling_data

a <- as.data.frame(describe(data[3:13]))
a

write.csv(a, file = "summary_stats.csv")

plot_histogram(data[1:13])
plot_density(data[3:13])

##########################
# Missing Values
##########################

gg_miss_var(data[1:13])+
  labs(y="Missing")

# No missing values

##########################
# Correlation Plot
##########################

corr <- round(cor(data[3:13]), 2)

corrplot <- ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation Plot", 
           ggtheme=theme_bw)

corrplot

###########################
# Scatter Plots
###########################

# gini vs percent_union_member
modeling_data %>% ggplot(aes(x = lagged_percent_union_members, y = gini_index, color = state_name)) +
  geom_point(size = 2, alpha = .5)+
  theme(legend.position = "none")

# gini vs gdp
modeling_data %>% ggplot(aes(x = lagged_gdp_in_millions, y = gini_index)) + geom_point(size = 4)

# gini vs percent with bach deg
modeling_data %>% ggplot(aes(x = lagged_perc_w_bach_deg_or_higher, y = gini_index, color = state_name)) +
  geom_point(size = 1, alpha = .5)+
  geom_smooth(method = 'lm', se = T, color = 'black') +
  theme(legend.position = "none")+
  xlab("% with Bachelor's or Higher")+
  ylab("Gini Coefficient")+
  ggtitle("College Education and Wealth Distribution")

# gini vs home ownership rate
modeling_data %>% ggplot(aes(x = homeownership_rate , y = gini_index)) + 
  geom_smooth(method = 'lm', se = F, color = 'black') + 
  geom_smooth(data = modeling_data, aes(color = state_name), method = 'lm', se = F) +
  geom_point(aes(x = homeownership_rate , y = gini_index, color = state_name, alpha = .1)) +
  theme(legend.position="none") +
  ggtitle('Homeownership and Wealth Distribution') +
  xlab('Homeownership Rate') +
  ylab('Gini Index')

# gini vs min wage
modeling_data %>% ggplot(aes(x = lagged_state_min_wage_rate, y = gini_index)) + geom_point(size = 4)

# gini vs avg unemply rate
modeling_data %>% ggplot(aes(x = lagged_yearly_avg_unemply_rate, y = gini_index)) + geom_line(size = 1)+
  facet_wrap(~state_name)

# gini vs homeownership
modeling_data %>% ggplot(aes(x = lagged_homeownership_rate, y = gini_index)) + geom_line(size = 1)+
  facet_wrap(~state_name)

ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" )

###########################
# Variables Mapped
##########################

# blank map of U.S.

ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# merge map data and modelilng data

map_data <- left_join(statedata,counties, by = "state_name")
mapping_data <- left_join(map_data, data, by = "state_name")

# % union Members

mapping_data %>%
  ggplot(aes(long, lat, group = group, fill = percent_union_members)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "")+
  xlab("")+
  ylab("")+
  ggtitle("% Union Members by State")

# Gini Index

mapping_data %>%
  ggplot(aes(long, lat, group = group, fill = gini_index)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "")+
  xlab("")+
  ylab("")+
  ggtitle("Gini Index by State")

# Bachelor's Degree or Higher

mapping_data %>%
  ggplot(aes(long, lat, group = group, fill = perc_w_bach_deg_or_higher)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Bachelor's Degree or Higher")


############################
# Time Plots
###########################

# Gini Index over time

ggplot(data, aes(x=year, y=gini_index))+
  geom_point(aes(color = percent_union_members))+
  geom_smooth(method="lm", data = data, size = .5, color = "black")+
  theme(legend.position="none")+
  xlab("Year")+
  ylab("Gini Index")+
  ggtitle("Gini Index over Time")

# Percent Bachelor's degree or higher over time

ggplot(data, aes(x=year, y=perc_w_bach_deg_or_higher))+
  geom_point(aes(color = state_name))+
  geom_smooth(method="lm", data = data, size = .5, color = "black")+
  theme(legend.position = "none")+
  xlab("Year")+
  ylab("Bachelor's Degrees or Higher (%)")+
  ggtitle("Bachelor's Degrees in the U.S.")

# Union Members over time

ggplot(data, aes(x=year, y=percent_union_members))+
  geom_point(aes(color = state_name))+
  geom_smooth(method="lm", data = data, size = .5, color = "black")+
  theme(legend.position = "none")+
  xlab("Year")+
  ylab("Union Members (%)")+
  ggtitle("Union Members in the U.S.")

###################################
# Distributions across states
###################################

# Education

ggplot(data, aes(x=perc_w_bach_deg_or_higher, y= state_name, color=state_name))+
  geom_joy(scale = 5, alpha = .5, panel_scaling = TRUE)+
  theme(legend.position="none")+
  xlab("% Bachelor's or Higher")+
  ylab("State")+
  ggtitle("Bachelor's Degrees across States")

# Gini

ggplot(data, aes(x=gini_index, y= state_name, color = state_name))+
  geom_joy(scale = 5, alpha = .5, panel_scaling = TRUE)+
  theme(legend.position="none")+
  xlab("Gini Index")+
  ylab("State")+
  ggtitle("Gini Index by State")

# % union Members

ggplot(data, aes(x=percent_union_members, y=state_name, color = state_name))+
  geom_joy(scale = 5, alpha = .5, panel_scaling = TRUE)+
  theme(legend.position="none")+
  xlab("% Union Members")+
  ylab("State")+
  ggtitle("Union Members by State")

# Homeownership by year

modeling_data %>% mutate(year_char = as.character(year)) %>% 
  ggplot(aes(x = homeownership_rate , y = year_char, alpha = .35)) + 
  geom_density_ridges() +
  theme(legend.position="none") +
  ggtitle('Distribution of Homeownership Rate by Year') +
  xlab('Homeownership Rate') +
  ylab('Year')

# Gini by year

modeling_data %>% mutate(year_char = as.character(year)) %>% 
  ggplot(aes(x = gini_index , y = year_char, alpha = .1)) + 
  geom_density_ridges() +
  theme(legend.position="none") +
  ggtitle('Gini Index by Year') +
  xlab('Gini Index') +
  ylab('Year')

# Union by Year

modeling_data %>% mutate(year_char = as.character(year)) %>% 
  ggplot(aes(x = percent_union_members , y = year_char, alpha = .1)) + 
  geom_density_ridges() +
  theme(legend.position="none") +
  ggtitle('Union Members by Year') +
  xlab('Union Members (%)') +
  ylab('Year')

# Bachelor's

modeling_data %>% mutate(year_char = as.character(year)) %>% 
  ggplot(aes(x = perc_w_bach_deg_or_higher , y = year_char, alpha = .1)) + 
  geom_density_ridges() +
  theme(legend.position="none") +
  ggtitle('Education over the Years') +
  xlab('Bachelor Degree or Higher (%)') +
  ylab('Year')
