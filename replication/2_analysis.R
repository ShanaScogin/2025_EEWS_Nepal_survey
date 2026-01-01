# this is the main file with the analysis for the paper
# the data vis can be found in the file entitled data vis

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(lmtest) # for rse 
library(sandwich) # for rse


###############################
# read in data and look at it
###############################
data <- read.csv("replication/data_recoded.csv")[, -1] # nrow = 567

with(data, table(exp1_treat, exp1_implement_code))
with(data, table(exp1_treat, exp1_useful_code))
with(data, table(exp1_treat, exp_1_interest_code))
with(data, table(exp1_treat, exp_1_interest_code_check))

rowSums(with(data, table(exp1_treat, exp1_implement_code)))
rowSums(with(data, table(exp1_treat, exp1_useful_code)))
rowSums(with(data, table(exp1_treat, exp_1_interest_code)))

# creating the low time dataframe for a check
d <- subset(data, time > 3)

###############################
# Main analysis set up: run regressions without covariates
###############################
# implement
model_implement <- lm(exp1_implement_code ~ as.factor(exp1_treat), data = data)
summary(model_implement)
length(model_implement$residuals)
p_values_implement <- c(summary(model_implement)$coefficients[2, 4],
                        summary(model_implement)$coefficients[3, 4],
                        summary(model_implement)$coefficients[4, 4])
p_values_implement
p_adjusted <- p.adjust(p_values_implement) # holm is default
p_adjusted

marginaleffects::plot_predictions(model_implement, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()

# useful
model_useful <- lm(exp1_useful_code ~ as.factor(exp1_treat), data = data)
summary(model_useful)
length(model_useful$residuals)
p_values_useful <- c(summary(model_useful)$coefficients[2, 4],
                     summary(model_useful)$coefficients[3, 4],
                     summary(model_useful)$coefficients[4, 4])
p_values_useful
p_adjusted <- p.adjust(p_values_useful) # holm is the default
p_adjusted

marginaleffects::plot_predictions(model_useful, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()

# check with low minute filtered out
model_useful_low <- lm(exp1_useful_code ~ as.factor(exp1_treat), data = d)
summary(model_useful_low)
length(model_useful_low$residuals)
p_values_useful_low <- c(summary(model_useful_low)$coefficients[2, 4],
                     summary(model_useful_low)$coefficients[3, 4],
                     summary(model_useful_low)$coefficients[4, 4])
p_values_useful_low
p_adjusted_low <- p.adjust(p_values_useful_low) # holm is the default
p_adjusted_low # still passes 

# interest
model_interest <- lm(exp_1_interest_code ~ as.factor(exp1_treat), data = data)
summary(model_interest)
length(model_interest$residuals)
p_values_interest <- c(summary(model_interest)$coefficients[2, 4],
                       summary(model_interest)$coefficients[3, 4],
                       summary(model_interest)$coefficients[4, 4])
p_values_interest
p_adjusted <- p.adjust(p_values_interest) # holm is default
p_adjusted # pass
marginaleffects::plot_predictions(model_interest, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()

# checking with really low time filtered out
model_interest_low <- lm(exp_1_interest_code ~ as.factor(exp1_treat), data = d)
summary(model_interest_low)
p_values_interest_low <- c(summary(model_interest_low)$coefficients[2, 4],
                       summary(model_interest_low)$coefficients[3, 4],
                       summary(model_interest_low)$coefficients[4, 4])
p_values_interest_low
p_adjusted_low <- p.adjust(p_values_interest_low) # holm is the default
p_adjusted_low # doesn't hold

model_interest_check <- lm(exp_1_interest_code_check ~ as.factor(exp1_treat), data = data)
summary(model_interest_check)
p_values_interest_check <- c(summary(model_interest_check)$coefficients[2, 4],
                       summary(model_interest_check)$coefficients[3, 4],
                       summary(model_interest_check)$coefficients[4, 4])
p_values_interest_check
p_adjusted_check <- p.adjust(p_values_interest_check) # holm is default
p_adjusted_check # this holds up


###############################
# Main analysis: Run main regressions with robust standard errors
###############################

# create rse
model_implement_robust <- lmtest::coeftest(model_implement, 
                                        vcov = vcovHC(model_implement, type="HC3"), # HC3 is default
                                        data = data_exp)
model_useful_robust <- lmtest::coeftest(model_useful, 
                                           vcov = vcovHC(model_useful, type="HC3"), # HC3 is default
                                           data = data_exp)
model_interest_robust <- lmtest::coeftest(model_interest, 
                                           vcov = vcovHC(model_interest, type="HC3"), # HC3 is default
                                           data = data_exp)
model_implement_robust
model_useful_robust
model_interest_robust

# check p-values with multiple comparison corrections
# implement
p_values_implement_rse <- c(model_implement_robust[2, 4],
                           model_implement_robust[3, 4],
                           model_implement_robust[4, 4])
p_values_implement_rse
p_values_adj_implement_rse <- p.adjust(p_values_implement_rse) # holm is the default
p_values_adj_implement_rse 

# useful
p_values_useful_rse <- c(model_useful_robust[2, 4],
                            model_useful_robust[3, 4],
                            model_useful_robust[4, 4])
p_values_useful_rse
p_values_adj_useful_rse <- p.adjust(p_values_useful_rse) # holm is the default
p_values_adj_useful_rse # holds

# interest
p_values_interest_rse <- c(model_interest_robust[2, 4],
                            model_interest_robust[3, 4],
                            model_interest_robust[4, 4])
p_values_interest_rse
p_values_adj_interest_rse <- p.adjust(p_values_interest_rse) # holm is the default
p_values_adj_interest_rse # holds

###############################
# Additional specifications: run regressions with covariates under 20 NAs
###############################
# implement
model_implement_cov <- lm(exp1_implement_code ~ as.factor(exp1_treat) +
                            experience_score +
                            gender_code +
                            rural_urban_code +
                            eews_know_code +
                            anxiety_code +
                            education_code +
                            caste_ethnicity_code 
                          , data = data)
summary(model_implement_cov)
marginaleffects::plot_predictions(model_implement_cov, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_implement_cov <- c(summary(model_implement_cov)$coefficients[2, 4],
                         summary(model_implement_cov)$coefficients[3, 4],
                         summary(model_implement_cov)$coefficients[4, 4])
p_values_implement_cov
p_adjusted <- p.adjust(p_values_implement_cov) # holm is the default
p_adjusted

# useful
model_useful_cov <- lm(exp1_useful_code ~ as.factor(exp1_treat) +
                         experience_score +
                         gender_code +
                         rural_urban_code +
                         eews_know_code +
                         anxiety_code +
                         education_code +
                         caste_ethnicity_code
                       , 
                       data = data)
summary(model_useful_cov)
marginaleffects::plot_predictions(model_useful_cov, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_useful_cov <- c(summary(model_useful_cov)$coefficients[2, 4],
                     summary(model_useful_cov)$coefficients[3, 4],
                     summary(model_useful_cov)$coefficients[4, 4])
p_values_useful_cov
p_adjusted <- p.adjust(p_values_useful_cov) # holm is the default
p_adjusted

# interest
model_interest_cov <- lm(exp_1_interest_code ~ as.factor(exp1_treat) +
                       experience_score +
                       gender_code +
                       rural_urban_code +
                       eews_know_code +
                       anxiety_code +
                       education_code +
                       caste_ethnicity_code 
                     ,
                     data = data)
summary(model_interest_cov)
marginaleffects::plot_predictions(model_interest_cov, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_interest_cov <- c(summary(model_interest_cov)$coefficients[2, 4],
                         summary(model_interest_cov)$coefficients[3, 4],
                         summary(model_interest_cov)$coefficients[4, 4])
p_values_interest_cov
p_adjusted <- p.adjust(p_values_interest_cov) # holm is the default
p_adjusted

###############################
# Secondary analysis: Run secondary (w/ cov) regressions with robust standard errors
###############################

# create rse for cov
model_implement_cov_robust <- lmtest::coeftest(model_implement_cov, 
                                           vcov = vcovHC(model_implement_cov, type="HC3"), # HC3 is default
                                           data = data_exp)
model_useful_cov_robust <- lmtest::coeftest(model_useful_cov, 
                                        vcov = vcovHC(model_useful_cov, type="HC3"), # HC3 is default
                                        data = data_exp)
model_interest_cov_robust <- lmtest::coeftest(model_interest_cov, 
                                          vcov = vcovHC(model_interest_cov, type="HC3"), # HC3 is default
                                          data = data_exp)
model_implement_cov_robust
model_useful_cov_robust 
model_interest_cov_robust

# check p-values with multiple comparison corrections
# implement
p_values_implement_cov_rse <- c(model_implement_cov_robust[2, 4],
                                model_implement_cov_robust[3, 4],
                                model_implement_cov_robust[4, 4])
p_values_implement_cov_rse
p_values_adj_implement_cov_rse <- p.adjust(p_values_implement_cov_rse) # holm is the default
p_values_adj_implement_cov_rse 

# useful
p_values_useful_cov_rse <- c(model_useful_cov_robust[2, 4],
                             model_useful_cov_robust[3, 4],
                             model_useful_cov_robust[4, 4])
p_values_useful_cov_rse
p_values_adj_useful_cov_rse <- p.adjust(p_values_useful_cov_rse) # holm is the default
p_values_adj_useful_cov_rse # doesn't hold

# interest
p_values_interest_cov_rse <- c(model_interest_cov_robust[2, 4],
                               model_interest_cov_robust[3, 4],
                               model_interest_cov_robust[4, 4])
p_values_interest_cov_rse
p_values_adj_interest_cov_rse <- p.adjust(p_values_interest_cov_rse) # holm is the default
p_values_adj_interest_cov_rse # holds


###############################
# Additional specifications: run regressions with covariates all (under 20 and over 20 NAs)
###############################
# implement
model_implement_cov_all <- lm(exp1_implement_code ~ as.factor(exp1_treat) +
                            experience_score +
                            gender_code +
                            rural_urban_code +
                            eews_know_code +
                            anxiety_code +
                            education_code +
                            caste_ethnicity_code +
                            married_code +
                            age_code
                          , data = data)
summary(model_implement_cov_all)
marginaleffects::plot_predictions(model_implement_cov_all, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_implement_cov_all <- c(summary(model_implement_cov_all)$coefficients[2, 4],
                            summary(model_implement_cov_all)$coefficients[3, 4],
                            summary(model_implement_cov_all)$coefficients[4, 4])
p_values_implement_cov_all
p_adjusted <- p.adjust(p_values_implement_cov_all) # holm is the default
p_adjusted

# useful
model_useful_cov_all <- lm(exp1_useful_code ~ as.factor(exp1_treat) +
                         experience_score +
                         gender_code +
                         rural_urban_code +
                         eews_know_code +
                         anxiety_code +
                         education_code +
                         caste_ethnicity_code +
                         married_code +
                         age_code
                       , 
                       data = data)
summary(model_useful_cov_all)
marginaleffects::plot_predictions(model_useful_cov_all, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_useful_cov_all <- c(summary(model_useful_cov_all)$coefficients[2, 4],
                         summary(model_useful_cov_all)$coefficients[3, 4],
                         summary(model_useful_cov_all)$coefficients[4, 4])
p_values_useful_cov_all
p_adjusted <- p.adjust(p_values_useful_cov_all) # holm is the default
p_adjusted

# interest
model_interest_cov_all <- lm(exp_1_interest_code ~ as.factor(exp1_treat) +
                           experience_score +
                           gender_code +
                           rural_urban_code +
                           eews_know_code +
                           anxiety_code +
                           education_code +
                           caste_ethnicity_code  +
                           married_code +
                           age_code
                         ,
                         data = data)
summary(model_interest_cov_all)
marginaleffects::plot_predictions(model_interest_cov_all, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_interest_cov_all <- c(summary(model_interest_cov_all)$coefficients[2, 4],
                           summary(model_interest_cov_all)$coefficients[3, 4],
                           summary(model_interest_cov_all)$coefficients[4, 4])
p_values_interest_cov_all
p_adjusted <- p.adjust(p_values_interest_cov_all) # holm is the default
p_adjusted

###############################
# Secondary analysis: Run secondary (w/ cov + 20nas) regressions with robust standard errors
###############################

# create rse for cov
model_implement_cov_robust_all <- lmtest::coeftest(model_implement_cov_all, 
                                               vcov = vcovHC(model_implement_cov_all, type="HC3"), # HC3 is default
                                               data = data_exp)
model_useful_cov_robust_all <- lmtest::coeftest(model_useful_cov_all, 
                                            vcov = vcovHC(model_useful_cov_all, type="HC3"), # HC3 is default
                                            data = data_exp)
model_interest_cov_robust_all <- lmtest::coeftest(model_interest_cov_all, 
                                              vcov = vcovHC(model_interest_cov_all, type="HC3"), # HC3 is default
                                              data = data_exp)
model_implement_cov_robust_all
model_useful_cov_robust_all # doesn't hold guessing NAs
model_interest_cov_robust_all

# check p-values with multiple comparison corrections
# implement
p_values_implement_cov_rse_all <- c(model_implement_cov_robust_all[2, 4],
                                model_implement_cov_robust_all[3, 4],
                                model_implement_cov_robust_all[4, 4])
p_values_implement_cov_rse_all
p_values_adj_implement_cov_rse_all <- p.adjust(p_values_implement_cov_rse_all) # holm is the default
p_values_adj_implement_cov_rse_all

# useful
p_values_useful_cov_rse_all <- c(model_useful_cov_robust_all[2, 4],
                             model_useful_cov_robust_all[3, 4],
                             model_useful_cov_robust_all[4, 4])
p_values_useful_cov_rse_all # doesn't hold
p_values_adj_useful_cov_rse_all <- p.adjust(p_values_useful_cov_rse_all) # holm is the default
p_values_adj_useful_cov_rse_all 

# interest
p_values_interest_cov_rse_all <- c(model_interest_cov_robust_all[2, 4],
                               model_interest_cov_robust_all[3, 4],
                               model_interest_cov_robust_all[4, 4])
p_values_interest_cov_rse_all
p_values_adj_interest_cov_rse_all <- p.adjust(p_values_interest_cov_rse_all) # holm is the default
p_values_adj_interest_cov_rse_all # holds


###############################
# Additional specifications: run regressions with only unbalanced covars
###############################
# implement
model_implement_balance <- lm(exp1_implement_code ~ as.factor(exp1_treat) +
                                caste_ethnicity_code 
                              , data = data)
summary(model_implement_balance)
marginaleffects::plot_predictions(model_implement_balance, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_implement_balance <- c(summary(model_implement_balance)$coefficients[2, 4],
                                summary(model_implement_balance)$coefficients[3, 4],
                                summary(model_implement_balance)$coefficients[4, 4])
p_values_implement_balance
p_adjusted <- p.adjust(p_values_implement_balance) # holm is the default
p_adjusted

# useful
model_useful_balance <- lm(exp1_useful_code ~ as.factor(exp1_treat) +
                             caste_ethnicity_code
                           , 
                           data = data)
summary(model_useful_balance)
marginaleffects::plot_predictions(model_useful_balance, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_useful_balance <- c(summary(model_useful_balance)$coefficients[2, 4],
                             summary(model_useful_balance)$coefficients[3, 4],
                             summary(model_useful_balance)$coefficients[4, 4])
p_values_useful_balance
p_adjusted <- p.adjust(p_values_useful_balance) # holm is the default
p_adjusted # holds

# interest
model_interest_balance <- lm(exp_1_interest_code ~ as.factor(exp1_treat) +
                               caste_ethnicity_code
                             ,
                             data = data)
summary(model_interest_balance)
marginaleffects::plot_predictions(model_interest_balance, 
                                  by = c("exp1_treat"), 
                                  type = "response",
                                  vcov = TRUE,
                                  conf_level = 0.90) +
  theme_minimal()
p_values_interest_balance <- c(summary(model_interest_balance)$coefficients[2, 4],
                               summary(model_interest_balance)$coefficients[3, 4],
                               summary(model_interest_balance)$coefficients[4, 4])
p_values_interest_balance
p_adjusted <- p.adjust(p_values_interest_balance) # holm is the default
p_adjusted # holds

###############################
# Secondary analysis: Run secondary on balance concerned ones
###############################

# create rse for cov
model_implement_balance_robust <- lmtest::coeftest(model_implement_balance, 
                                                   vcov = vcovHC(model_implement_balance, type="HC3"), # HC3 is default
                                                   data = data_exp)
model_useful_balance_robust <- lmtest::coeftest(model_useful_balance, 
                                                vcov = vcovHC(model_useful_balance, type="HC3"), # HC3 is default
                                                data = data_exp)
model_interest_balance_robust <- lmtest::coeftest(model_interest_balance, 
                                                  vcov = vcovHC(model_interest_balance, type="HC3"), # HC3 is default
                                                  data = data_exp)
model_implement_balance_robust
model_useful_balance_robust # holds
model_interest_balance_robust # holds

# check p-values with multiple comparison corrections
# implement
p_values_implement_balance_rse <- c(model_implement_balance_robust[2, 4],
                                    model_implement_balance_robust[3, 4],
                                    model_implement_balance_robust[4, 4])
p_values_implement_balance_rse
p_values_adj_implement_balance <- p.adjust(p_values_implement_balance_rse) # holm is the default
p_values_adj_implement_balance 

# useful
p_values_useful_balance_rse <- c(model_useful_balance_robust[2, 4],
                                 model_useful_balance_robust[3, 4],
                                 model_useful_balance_robust[4, 4])
p_values_useful_balance_rse 
p_values_adj_useful_balance <- p.adjust(p_values_useful_balance_rse) # holm is the default
p_values_adj_useful_balance # holds 

# interest
p_values_interest_balance_rse <- c(model_interest_balance_robust[2, 4],
                                   model_interest_balance_robust[3, 4],
                                   model_interest_balance_robust[4, 4])
p_values_interest_balance_rse
p_values_adj_interest_balance <- p.adjust(p_values_interest_balance_rse) # holm is the default
p_values_adj_interest_balance # holds 




