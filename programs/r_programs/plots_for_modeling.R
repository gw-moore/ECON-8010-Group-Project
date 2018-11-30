# setup envrionmnent 
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")
library(corrplot)
library(tidyverse)

###########################
# Correlation plot
###########################

varible_correlations <- cor(modeling_data[,3:12])
varible_correlations <- round(varible_correlations,4)
corrplot(varible_correlations, method = "circle", type = 'upper', diag = F, tl.col = "black", title = 'Correlation of Variables')


#################
# scatter plots
#################

# gini vs percent_union_member
modeling_data %>% ggplot(aes(x = lagged_percent_union_members, y = gini_index)) + geom_point(size = 4)
# gini vs gdp
modeling_data %>% ggplot(aes(x = lagged_gdp_in_millions, y = gini_index)) + geom_point(size = 4)
# gini vs percent with bach deg
modeling_data %>% ggplot(aes(x = lagged_perc_w_bach_deg_or_higher, y = gini_index)) + geom_point(size = 4)
# gini vs min wage
modeling_data %>% ggplot(aes(x = lagged_state_min_wage_rate, y = gini_index)) + geom_point(size = 4)
# gini vs avg unemply rate
modeling_data %>% ggplot(aes(x = lagged_yearly_avg_unemply_rate, y = gini_index)) + geom_point(size = 4)
# gini vs homeownership
modeling_data %>% ggplot(aes(x = lagged_homeownership_rate, y = gini_index)) + geom_point(size = 4)

#############################
# Hetrogeneity plots
#############################

# gini vs percent union members
modeling_data %>% ggplot(aes(x = lagged_percent_union_members, y = gini_index)) + 
  geom_smooth(method = 'lm', se = F, color = 'black') + 
  geom_smooth(data = modeling_data, aes(color = state_name), method = 'lm', se = F) +
  geom_point(aes(x = percent_union_members , y = gini_index, color = state_name, alpha = .1)) +
  theme(legend.position="none") +
  ggtitle('Heterogeneity Bias Across States') +
  xlab('Percent of Workforce who are Union Memeber') +
  ylab('Gini Index')

# gini vs gdp
modeling_data %>% ggplot(aes(x = lagged_gdp_in_millions , y = gini_index)) + 
  geom_smooth(method = 'lm', se = F, color = 'black') + 
  geom_smooth(data = modeling_data, aes(color = state_name), method = 'lm', se = F) +
  geom_point(aes(x = percent_union_members , y = gini_index, color = state_name, alpha = .1)) +
  theme(legend.position="none") +
  ggtitle('Heterogeneity Bias Across States') +
  xlab('GDP') +
  ylab('Gini Index')

# gini vs percent w/ bach or higher
modeling_data %>% ggplot(aes(x = lagged_perc_w_bach_deg_or_higher , y = gini_index)) + 
  geom_smooth(method = 'lm', se = F, color = 'black') + 
  geom_smooth(data = modeling_data, aes(color = state_name), method = 'lm', se = F) +
  geom_point(aes(x = perc_w_bach_deg_or_higher , y = gini_index, color = state_name, alpha = .1)) +
  theme(legend.position="none") +
  ggtitle('Heterogeneity Bias Across States') +
  xlab('Percent w/ Bach Degree or Higher') +
  ylab('Gini Index')

# gini vs housing value
modeling_data %>% ggplot(aes(x = homeownership_rate , y = gini_index)) + 
  geom_smooth(method = 'lm', se = F, color = 'black') + 
  geom_smooth(data = modeling_data, aes(color = state_name), method = 'lm', se = F) +
  geom_point(aes(x = homeownership_rate , y = gini_index, color = state_name, alpha = .1)) +
  theme(legend.position="none") +
  ggtitle('Heterogeneity Bias Across States') +
  xlab('Homeownership Rate') +
  ylab('Gini Index')

# gini vs min wage
modeling_data %>% ggplot(aes(x = state_min_wage_rate , y = gini_index)) + 
  geom_smooth(method = 'lm', se = F, color = 'black') + 
  geom_smooth(data = modeling_data, aes(color = state_name), method = 'lm', se = F) +
  geom_point(aes(x = state_min_wage_rate , y = gini_index, color = state_name, alpha = .1)) +
  theme(legend.position="none") +
  ggtitle('Heterogeneity Bias Across States') +
  xlab('State Minimum Wage') +
  ylab('Gini Index')