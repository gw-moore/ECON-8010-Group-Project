# setup envrionmnent 
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")
library(tidyverse)
library(plm)
library(lmtest)
library(car)
library(sandwich)
library(pls)
options("scipen"=10, "digits"=8)

# load data
load('programs/prepped_data/modeling_data.rda')

######
# calculate RGDP growth
modeling_data <- modeling_data %>%  mutate(rgdp_growth_rate = (gdp_in_millions - lagged_gdp_in_millions) / lagged_gdp_in_millions)

#############################
# fit models
#############################

# pooled regression model
pooled_reg <- plm(data = modeling_data, formula = gini_index ~ lagged_percent_union_members + lagged_gdp_in_millions + rgdp_growth_rate + lagged_state_min_wage_rate + lagged_perc_w_bach_deg_or_higher + lagged_yearly_avg_unemply_rate + lagged_yearly_avg_unemply_rate_squared + lagged_homeownership_rate, index = c('state_name', 'year'), model = 'pooling')

summary(pooled_reg)

# fixed effects model using PLM
fix_effects_reg <- plm(data = modeling_data, formula = gini_index ~ lagged_percent_union_members + lagged_gdp_in_millions + lagged_state_min_wage_rate + lagged_perc_w_bach_deg_or_higher + lagged_yearly_avg_unemply_rate + lagged_yearly_avg_unemply_rate_squared + homeownership_rate, index = c('state_name', 'year'), model = 'within', effect = 'twoways')

# print summary
summary(fix_effects_reg)

# test to see if state and year fixed effects are statistically siginificant 
plmtest(fix_effects_reg, c("individual"), type=("bp"))
plmtest(fix_effects_reg, c("time"), type=("bp"))

# print out fixed effects for states and years
fixef(fix_effects_reg,effect="individual")
fixef(fix_effects_reg,effect="time")

####################################
# compare models
####################################

# compare pooled and fixed effects
pFtest(fix_effects_reg, pooled_reg)

####################################
# Asumption test
####################################

# testing for hetroskedasticy
bptest(fix_effects_reg, data = modeling_data, studentize=F)

residual_values <- data_frame(fix_effects_reg$residual)
fitted_values <- data_frame(fix_effects_reg$model[[1]] - fix_effects_reg$residuals)
fits_res_values <- cbind(residual_values, fitted_values)
colnames(fits_res_values) <- c('residual', 'fitted_values')
rm(list = c('residual_values', 'fitted_values'))

fits_res_values %>% ggplot(aes(y = residual, x = fitted_values)) + geom_point() +
  ggtitle('Fits vs Residuals - Fixed Effects Model') +
  xlab('Fitted Values') +
  ylab('Residuals')
# our model violates hetroskedasticy

# testing for serial correlation in the errors
pbgtest(fix_effects_reg)
# our model violates serial correlation assumptions

###############################################################################
# we have both serial correlation and hetroskedastisity. Must obtain robust standard error
coeftest(fix_effects_reg, vcov = vcovHC(fix_effects_reg, method = 'arellano'))

# testing for multicolinearity
# using pooled ols model because vif doesn't work for plm models. Also, multicollinearity is only
# about the independent varibles, there is no need to coltrol for individual effects to estimate vif
vif <- as.data.frame(vif(pooled_reg))
write.csv(vif, 'outputs/vif.csv')

# fit lsdv model
lsdv_mod <- lm(formula = gini_index ~ lagged_percent_union_members + lagged_gdp_in_millions + lagged_gdp_in_millions_squared + lagged_state_min_wage_rate + lagged_perc_w_bach_deg_or_higher + lagged_yearly_avg_unemply_rate + lagged_yearly_avg_unemply_rate_squared + homeownership_rate + factor(state_name) + factor(year) - 1, data = modeling_data)

summary(lsdv_mod)



