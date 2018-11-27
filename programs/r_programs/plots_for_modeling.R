check_years <- modeling_data %>% group_by(state_name) %>% summarise(num_years = n_distinct(year))

library("corrplot")

res <- cor(modeling_data[,3:12])
res <- round(res,2)

#Correlation plot

corrplot(res, method = "circle")

modeling_data %>% ggplot(aes(x = percent_union_members , y = gini_index)) + geom_smooth(method = 'lm')
modeling_data %>% ggplot(aes(x = percent_union_members , y = gini_index, color = state_name)) + 
  geom_smooth(method = 'lm') +
  geom_smooth(aes(x = percent_union_members, y = gini_index), method = 'lm')

# gini vs percent union members
modeling_data %>% ggplot(aes(x = percent_union_members , y = gini_index)) + 
  geom_smooth(method = 'lm', se = F, color = 'black') + 
  geom_smooth(data = modeling_data, aes(color = state_name), method = 'lm', se = F) +
  geom_point(aes(x = percent_union_members , y = gini_index, color = state_name, alpha = .1)) +
  theme(legend.position="none") +
  ggtitle('Heterogeneity Bias Across States') +
  xlab('Percent of Workforce who are Union Memeber') +
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
