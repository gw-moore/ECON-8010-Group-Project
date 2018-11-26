# setup envrionmnent 
setwd("~/School/Fall_2018/Econometrics/ECON-8010-Group-Project")
library(tidyverse)
library(plm)

# load data
load('programs/prepped_data/modeling_data.rda')

# fit models

# pooled regression model
pooled_reg <- plm(data = modeling_data, gini_index ~ percent_union_members + population + gdp_in_millions + state_min_wage_rate + per_capital_personal_income + yearly_avg_clf + perc_w_bach_deg_or_higher + yearly_avg_unemply_rate, index = c('state_name', 'year'), model = 'pooling')

summary(pooled_reg)

# fixed effects model
fix_effects_reg <- plm(data = modeling_data, gini_index ~ percent_union_members + population + gdp_in_millions + state_min_wage_rate + per_capital_personal_income + yearly_avg_clf + perc_w_bach_deg_or_higher + yearly_avg_unemply_rate, index = c('state_name', 'year'), model = 'within')

summary(fix_effects_reg)
  
# random effect model
rand_effect_reg <- plm(data = modeling_data, gini_index ~ percent_union_members + population + gdp_in_millions + state_min_wage_rate + per_capital_personal_income + yearly_avg_clf + perc_w_bach_deg_or_higher + yearly_avg_unemply_rate, index = c('state_name', 'year'), model = 'random')

summary(rand_effect_reg)

  
# husman test
# h.null = random effects model is the correct model
# h.alt = fixed effect model is a better fit
phtest(rand_effect_reg, fix_effects_reg)
