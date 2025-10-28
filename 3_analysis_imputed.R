# this file is the analysis and data vis for the imputed full
# covariate specification that is included in the appendix
# the variables imputed include: gender, married, caste_ethnicity, district,
# rural_urban, age, education, anxiety, eews_know, time (as predictor not imputed).
# they were imputed prior to being recoded for analysis
# this file also has the data vis for this portion 

library(mice)
library(miceadds) # for model stats for imputed lms
library(tidyverse)
library(lmtest) # for rse
library(sandwich) # for rse
library(broom) # specifically to manually pool after rse
library(patchwork) # for stacking plots

################
# switch this if do not want to save the graphics
################
save <- 0 # 1 is save 0 is no

#################
# Reading in data 
################
imputed_long <- read.csv("replication/data_imputed_recoded.csv")[, -1] # nrow = 3402
mids_imputed_long <- as.mids(imputed_long)

source("replication/6_data_vis.r") # this has df_plot_implement, etc (for all three) that is needed below


############
# analysis: lm
#############
######
# implement
fit_implement <- with(mids_imputed_long, lm(exp1_implement_code ~ as.factor(exp1_treat) +
                                             experience_score +
                                             gender_code +
                                             rural_urban_code +
                                             eews_know_code +
                                             anxiety_code +
                                             education_code +
                                             caste_ethnicity_code  +
                                             married_code +
                                             age_code))
fit_implement

pool.fit <- mice::pool(fit_implement)
summary(pool.fit)
pool.fit$glanced
pool.r.squared(pool.fit, adjusted = FALSE) 
pool.r.squared(pool.fit, adjusted = TRUE) 
# for the stats
# https://stackoverflow.com/questions/73471013/how-to-get-r2-f-statistics-and-p-value-for-pooled-models-with-imputed-data
anova_fit <- mi.anova(mi.res = mids_imputed_long, 
                      formula="exp1_implement_code ~ as.factor(exp1_treat) +
                             experience_score +
                             gender_code +
                             rural_urban_code +
                             eews_know_code +
                             anxiety_code +
                             education_code +
                             caste_ethnicity_code  +
                             married_code +
                             age_code" )
anova_fit$r.squared  ## r2 - not adjusted
(fval <- mean(round(anova_fit$anova.table$`F value`, 2), na.rm=TRUE) ) ## f-stat
df_mod <- anova_fit$anova.table$df1[- nrow(anova_fit$anova.table)]  ## df for model
df_res <- el(fit_implement$analyses)$df.residual  ## df resid
c(df_mod, df_res)
pf(q = fval, 
   df1 = sum(df_mod), 
   df2 = df_res, lower.tail=FALSE)  ## model p-value

######
# useful
fit_useful <- with(mids_imputed_long, lm(exp1_useful_code ~ as.factor(exp1_treat) +
                                              experience_score +
                                              gender_code +
                                              rural_urban_code +
                                              eews_know_code +
                                              anxiety_code +
                                              education_code +
                                              caste_ethnicity_code  +
                                              married_code +
                                              age_code))
fit_useful

pool.fit <- mice::pool(fit_useful)
summary(pool.fit)
pool.fit$glanced
pool.r.squared(pool.fit, adjusted = FALSE) 
pool.r.squared(pool.fit, adjusted = TRUE) 
# for the stats
# https://stackoverflow.com/questions/73471013/how-to-get-r2-f-statistics-and-p-value-for-pooled-models-with-imputed-data
anova_fit <- mi.anova(mi.res = mids_imputed_long, 
                      formula="exp1_useful_code ~ as.factor(exp1_treat) +
                             experience_score +
                             gender_code +
                             rural_urban_code +
                             eews_know_code +
                             anxiety_code +
                             education_code +
                             caste_ethnicity_code  +
                             married_code +
                             age_code" )
anova_fit$r.squared  ## r2 - not adjusted
(fval <- mean(round(anova_fit$anova.table$`F value`, 2), na.rm=TRUE) ) ## f-stat
df_mod <- anova_fit$anova.table$df1[- nrow(anova_fit$anova.table)]  ## df for model
df_res <- el(fit_useful$analyses)$df.residual  ## df resid
c(df_mod, df_res)
pf(q = fval, 
   df1 = sum(df_mod), 
   df2 = df_res, lower.tail=FALSE)  ## model p-value

######
# interest
fit_interest <- with(mids_imputed_long, lm(exp_1_interest_code ~ as.factor(exp1_treat) +
                          experience_score +
                          gender_code +
                          rural_urban_code +
                          eews_know_code +
                          anxiety_code +
                          education_code +
                          caste_ethnicity_code  +
                          married_code +
                          age_code))
fit_interest

pool.fit <- mice::pool(fit_interest)
summary(pool.fit)
pool.fit$glanced
pool.r.squared(pool.fit, adjusted = FALSE) 
pool.r.squared(pool.fit, adjusted = TRUE) 
# for the stats
# https://stackoverflow.com/questions/73471013/how-to-get-r2-f-statistics-and-p-value-for-pooled-models-with-imputed-data
anova_fit <- mi.anova(mi.res = mids_imputed_long, 
         formula="exp_1_interest_code ~ as.factor(exp1_treat) +
                             experience_score +
                             gender_code +
                             rural_urban_code +
                             eews_know_code +
                             anxiety_code +
                             education_code +
                             caste_ethnicity_code  +
                             married_code +
                             age_code" )
anova_fit$r.squared  ## r2 - not adjusted
(fval <- mean(round(anova_fit$anova.table$`F value`, 2), na.rm=TRUE) ) ## f-stat
df_mod <- anova_fit$anova.table$df1[- nrow(anova_fit$anova.table)]  ## df for model
df_res <- el(fit_interest$analyses)$df.residual  ## df resid
c(df_mod, df_res)
pf(q=fval, 
   df1=sum(df_mod), 
   df2=df_res, lower.tail=FALSE)  ## model p-value

############
# analysis: rse
#############
# implement
fit_implement_rse <- with(mids_imputed_long, coeftest(lm(exp1_implement_code ~ as.factor(exp1_treat) +
                                                        experience_score +
                                                        gender_code +
                                                        rural_urban_code +
                                                        eews_know_code +
                                                        anxiety_code +
                                                        education_code +
                                                        caste_ethnicity_code  +
                                                        married_code +
                                                        age_code), 
                                                   vcov = vcovHC(lm(exp1_implement_code ~ as.factor(exp1_treat) +
                                                                       experience_score +
                                                                       gender_code +
                                                                       rural_urban_code +
                                                                       eews_know_code +
                                                                       anxiety_code +
                                                                       education_code +
                                                                       caste_ethnicity_code  +
                                                                       married_code +
                                                                       age_code), type="HC3")))
summary(mice::pool(fit_implement_rse))


p_values_implement <- c(summary(mice::pool(fit_implement_rse))[2, 6],
                     summary(mice::pool(fit_implement_rse))[3, 6],
                     summary(mice::pool(fit_implement_rse))[4, 6])
p_values_implement
p_adjusted <- p.adjust(p_values_implement) # holm is the default
p_adjusted 

# useful
fit_useful_rse <- with(mids_imputed_long, coeftest(lm(exp1_useful_code ~ as.factor(exp1_treat) +
                                   experience_score +
                                   gender_code +
                                   rural_urban_code +
                                   eews_know_code +
                                   anxiety_code +
                                   education_code +
                                   caste_ethnicity_code  +
                                   married_code +
                                   age_code), 
                                   vcovHC(lm(exp1_useful_code ~ as.factor(exp1_treat) +
                                               experience_score +
                                               gender_code +
                                               rural_urban_code +
                                               eews_know_code +
                                               anxiety_code +
                                               education_code +
                                               caste_ethnicity_code  +
                                               married_code +
                                               age_code), type="HC3")))
summary(mice::pool(fit_useful_rse))

p_values_useful <- c(summary(mice::pool(fit_useful_rse))[2, 6],
                     summary(mice::pool(fit_useful_rse))[3, 6],
                     summary(mice::pool(fit_useful_rse))[4, 6])
p_values_useful
p_adjusted <- p.adjust(p_values_useful) # holm is the default
p_adjusted # T3 just holds. would be curious to see it with experience also imputed
           # but at this point seems a function of power and can't do much about that

# interest
fit_interest_rse <- with(mids_imputed_long, coeftest(lm(exp_1_interest_code ~ as.factor(exp1_treat) +
                                                        experience_score +
                                                        gender_code +
                                                        rural_urban_code +
                                                        eews_know_code +
                                                        anxiety_code +
                                                        education_code +
                                                        caste_ethnicity_code  +
                                                        married_code +
                                                        age_code), 
                                                     vcovHC(lm(exp_1_interest_code ~ as.factor(exp1_treat) +
                                                                 experience_score +
                                                                 gender_code +
                                                                 rural_urban_code +
                                                                 eews_know_code +
                                                                 anxiety_code +
                                                                 education_code +
                                                                 caste_ethnicity_code  +
                                                                 married_code +
                                                                 age_code), type="HC3")))
summary(mice::pool(fit_interest_rse))

p_values_interest <- c(summary(mice::pool(fit_interest_rse))[2, 6],
                     summary(mice::pool(fit_interest_rse))[3, 6],
                     summary(mice::pool(fit_interest_rse))[4, 6])
p_values_interest
p_adjusted <- p.adjust(p_values_interest) # holm is the default
p_adjusted #  holds

#########################################
############
# data vis
#############
# first doing the confidence intervals
# https://stats.stackexchange.com/questions/200477/compute-95-confidence-interval-for-predictions-using-a-pooled-model-after-multi

calculate_pooled_cis <- function(pooled_obj, conf_level = 0.95) {
  if (!inherits(pooled_obj, "mipo")) {
    stop("Input must be a 'mipo' object generated by the `mice::pool()` function.")
  }
  
  # extract summary stats from the pooled object
  pooled_summary <- summary(pooled_obj)
  
  # calc alpha and critical t-value
  alpha <- 1 - conf_level
  critical_t <- qt(1 - alpha / 2, df = pooled_summary$df)
  
  # compute bounds
  lower_bound <- pooled_summary$estimate - critical_t * pooled_summary$std.error
  upper_bound <- pooled_summary$estimate + critical_t * pooled_summary$std.error
  
  # df
  conf_intervals <- data.frame(
    term = pooled_summary$term,
    estimate = pooled_summary$estimate,
    se = pooled_summary$std.error,
    df = pooled_summary$df,
    lower = lower_bound,
    upper = upper_bound
  )
  
  return(conf_intervals)
}

#############
# preparing data for cov specifications with imputed data for appendix
##########
# implement
df_plot_implement_cov <- data.frame(
  outcome = c("T1: Technical\nInformation", 
              "T2: Limitations", 
              "T3: Combined"),
  estimate = c(summary(mice::pool(fit_implement_rse))[2, 2], 
               summary(mice::pool(fit_implement_rse))[3, 2], 
               summary(mice::pool(fit_implement_rse))[4, 2]),
  se_rse = c(summary(mice::pool(fit_implement_rse))[2, 3], 
             summary(mice::pool(fit_implement_rse))[3, 3], 
             summary(mice::pool(fit_implement_rse))[4, 3]),
  lower_bound_rse = c(calculate_pooled_cis(mice::pool(fit_implement_rse))[2, 5], 
                      calculate_pooled_cis(mice::pool(fit_implement_rse))[3, 5], 
                      calculate_pooled_cis(mice::pool(fit_implement_rse))[4, 5]),
  upper_bound_rse = c(calculate_pooled_cis(mice::pool(fit_implement_rse))[2, 6], 
                      calculate_pooled_cis(mice::pool(fit_implement_rse))[3, 6], 
                      calculate_pooled_cis(mice::pool(fit_implement_rse))[4, 6])
)

# useful
df_plot_useful_cov <- data.frame(
  outcome = c("T1: Technical\nInformation", 
              "T2: Limitations", 
              "T3: Combined"),
  estimate = c(summary(mice::pool(fit_useful_rse))[2, 2], 
               summary(mice::pool(fit_useful_rse))[3, 2], 
               summary(mice::pool(fit_useful_rse))[4, 2]),
  se_rse = c(summary(mice::pool(fit_useful_rse))[2, 3], 
             summary(mice::pool(fit_useful_rse))[3, 3], 
             summary(mice::pool(fit_useful_rse))[4, 3]),
  lower_bound_rse = c(calculate_pooled_cis(mice::pool(fit_useful_rse))[2, 5], 
                      calculate_pooled_cis(mice::pool(fit_useful_rse))[3, 5], 
                      calculate_pooled_cis(mice::pool(fit_useful_rse))[4, 5]),
  upper_bound_rse = c(calculate_pooled_cis(mice::pool(fit_useful_rse))[2, 6], 
                      calculate_pooled_cis(mice::pool(fit_useful_rse))[3, 6], 
                      calculate_pooled_cis(mice::pool(fit_useful_rse))[4, 6])
)

# interest
df_plot_interest_cov <- data.frame(
  outcome = c("T1: Technical\nInformation", 
              "T2: Limitations", 
              "T3: Combined"),
  estimate = c(summary(mice::pool(fit_interest_rse))[2, 2], 
               summary(mice::pool(fit_interest_rse))[3, 2], 
               summary(mice::pool(fit_interest_rse))[4, 2]),
  se_rse = c(summary(mice::pool(fit_interest_rse))[2, 3], 
             summary(mice::pool(fit_interest_rse))[3, 3], 
             summary(mice::pool(fit_interest_rse))[4, 3]),
  lower_bound_rse = c(calculate_pooled_cis(mice::pool(fit_interest_rse))[2, 5], 
                      calculate_pooled_cis(mice::pool(fit_interest_rse))[3, 5], 
                      calculate_pooled_cis(mice::pool(fit_interest_rse))[4, 5]),
  upper_bound_rse = c(calculate_pooled_cis(mice::pool(fit_interest_rse))[2, 6], 
                      calculate_pooled_cis(mice::pool(fit_interest_rse))[3, 6], 
                      calculate_pooled_cis(mice::pool(fit_interest_rse))[4, 6])
)

# reordering the dfs for plotting
df_plot_implement_cov <- df_plot_implement_cov %>%
  mutate(order_factor = factor(outcome, levels = c("T1: Technical\nInformation", 
                                                   "T2: Limitations", 
                                                   "T3: Combined"))) # adjusting order
df_plot_useful_cov <- df_plot_useful_cov %>%
  mutate(order_factor = factor(outcome, levels = c("T1: Technical\nInformation", 
                                                   "T2: Limitations", 
                                                   "T3: Combined"))) # adjusting order
df_plot_interest_cov <- df_plot_interest_cov %>%
  mutate(order_factor = factor(outcome, levels = c("T1: Technical\nInformation", 
                                                   "T2: Limitations", 
                                                   "T3: Combined"))) # adjusting order

##########
# extra data combination to graph more specifications (all)
##########
# implement
df_plot_implement$Specification <- "Base"
df_plot_implement_cov$Specification <- "Covariates"

combined_data_implement <- bind_rows(df_plot_implement, df_plot_implement_cov)
combined_data_implement$Specification <- forcats::fct_relevel(combined_data_implement$Specification, 
                                                              "Covariates", "Base")

# useful
df_plot_useful$Specification <- "Base"
df_plot_useful_cov$Specification <- "Covariates"

combined_data_useful <- bind_rows(df_plot_useful, df_plot_useful_cov)
combined_data_useful$Specification <- forcats::fct_relevel(combined_data_useful$Specification, 
                                                           "Covariates", "Base")

# interest
df_plot_interest$Specification <- "Base"
df_plot_interest_cov$Specification <- "Covariates"

combined_data_interest <- bind_rows(df_plot_interest, df_plot_interest_cov)
combined_data_interest$Specification <- forcats::fct_relevel(combined_data_interest$Specification, 
                                                             "Covariates", "Base")

##########
# graphing more specifications
##########
# implement
p4 <- ggplot(combined_data_implement, aes(x = fct_rev(outcome), y = estimate,
                                          color = Specification)) +
  geom_errorbar(aes(ymin = lower_bound_rse, ymax = upper_bound_rse), width = 0.1, position = dodge) +
  geom_point(position = dodge) +  
  scale_color_manual(values = c("Base" = "black", 
                                "Covariates" = "gray")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "",
       x = "Outcome 1: \nDesire to implement\n",
       y = "Estimate") +
  guides(color = guide_legend(reverse = TRUE)) +
  ylim(-0.5, 0.5)  +
  theme_minimal(base_family = "Times New Roman") +
  theme(axis.text.x  = element_text(size = 14, family = "Times New Roman"),
        axis.title.x = element_text(size = 18, family = "Times New Roman"),
        axis.text.y  = element_text(size = 14, family = "Times New Roman"),
        axis.title.y = element_text(size = 18, family = "Times New Roman"),
        legend.text  = element_text(size = 14, family = "Times New Roman"),
        legend.title = element_text(size = 14, family = "Times New Roman"),
        text         = element_text(family = "Times New Roman")
  ) +
  coord_flip()
p4

# useful
p5 <- ggplot(combined_data_useful, aes(x = fct_rev(outcome), y = estimate,
                                       color = Specification)) +
  geom_errorbar(aes(ymin = lower_bound_rse, ymax = upper_bound_rse), width = 0.1, position = dodge) +
  geom_point(position = dodge) +  
  scale_color_manual(values = c("Base" = "black", 
                                "Covariates" = "gray")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "",
       x = "Outcome 2: \nOpinions on usefulnesse\n",
       y = "Estimate") +
  guides(color = guide_legend(reverse = TRUE)) +
  ylim(-0.5, 0.5)  +
  theme_minimal(base_family = "Times New Roman") +
  theme(axis.text.x  = element_text(size = 14, family = "Times New Roman"),
        axis.title.x = element_text(size = 18, family = "Times New Roman"),
        axis.text.y  = element_text(size = 14, family = "Times New Roman"),
        axis.title.y = element_text(size = 18, family = "Times New Roman"),
        legend.text  = element_text(size = 14, family = "Times New Roman"),
        legend.title = element_text(size = 14, family = "Times New Roman"),
        text         = element_text(family = "Times New Roman")
  ) +
  coord_flip()
p5

# interest
p6 <- ggplot(combined_data_interest, aes(x = fct_rev(outcome), y = estimate,
                                         color = Specification)) +
  geom_errorbar(aes(ymin = lower_bound_rse, ymax = upper_bound_rse), width = 0.1, position = dodge) +
  geom_point(position = dodge) +  
  scale_color_manual(values = c("Base" = "black", 
                                "Covariates" = "gray")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "",
       x = "Outcome 3: \nInterest in learning more\n",
       y = "Estimate") +
  guides(color = guide_legend(reverse = TRUE)) +
  ylim(-0.5, 0.5)  +
  theme_minimal(base_family = "Times New Roman") +
  theme(axis.text.x  = element_text(size = 14, family = "Times New Roman"),
        axis.title.x = element_text(size = 18, family = "Times New Roman"),
        axis.text.y  = element_text(size = 14, family = "Times New Roman"),
        axis.title.y = element_text(size = 18, family = "Times New Roman"),
        legend.text  = element_text(size = 14, family = "Times New Roman"),
        legend.title = element_text(size = 14, family = "Times New Roman"),
        text         = element_text(family = "Times New Roman")
  ) +
  coord_flip()

p6


##############
# plotting with patchwork
p4 / p5 / p6

if (save == 1) {
    ggsave("combined_plot_appendix_covariates.jpeg", p4 / p5 / p6, width = 8, height = 12,
           dpi = 300) 
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}

