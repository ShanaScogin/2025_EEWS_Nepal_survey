# this file as some ordered and categorical logits for the regressions and experiment

##############
# LOGISTIC REGRESSIONS
##############
library(tidyverse)
library(marginaleffects)
library(performance) # for pseudo r2
library(fmsb) # for pseudo r2 for logit


# read in data
data <- read.csv("replication/data_recoded.csv")[, -1] # nrow = 567

############################
# switch this - the flag for save data vis
# 1 for yes, 0 (or anything else) for no
save <- 0


###################
# IMPLEMENT
###################
# create new variable for transparency (to compare)
data$exp1_treat_factor <- as.factor(data$exp1_treat)
data$exp1_implement_factor <- as.factor(data$exp1_implement_code)

# model
model_implement_polr_log <- MASS::polr(exp1_implement_factor ~ exp1_treat_factor, 
                             data = data, 
                             Hess = TRUE,
                             method = c("logistic"))
summary(model_implement_polr_log)
nrow(model_implement_polr_log$fitted.values)
lmtest::coeftest(model_implement_polr_log)
performance::r2(model_implement_polr_log)


####
# slopes
slopes_obj <- avg_slopes(model_implement_polr_log, 
                         type = "probs")
head(slopes_obj)

p_logit_ordered_binned <- slopes_obj %>% 
  ggplot(., aes(x = fct_rev(term), 
                y = estimate, 
                color = contrast, 
                group = fct_rev(group))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  scale_color_grey(start = 0.2, end = 0.8) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(
    axis.title = element_text(size = 16),   
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 12)  
  ) +
  labs(
    title = "",
    x = "Treatment",
    y = "Estimate",
    color = "Group"
  ) 

p_logit_ordered_binned

if (save == 1) {
  ggsave("implement_slopes.jpeg", width = 11, height = 7.3,
         dpi = 300)
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}

##########
# ordered logit pred probs
##########
marginaleffects <- marginaleffects(model_implement_polr_log, type = "probs")
margeff_table <- summary(marginaleffects)
format(margeff_table, scientific = FALSE)

preds <- marginaleffects::predictions(
  model_implement_polr_log,
  newdata = datagrid(exp1_treat_factor = c(0, 1, 2, 3))
)

preds$group <- factor(preds$group , 
                      levels = c("1",
                                 "2",
                                 "3",
                                 "4",
                                 "5"))

preds %>% 
  ggplot(aes(x = factor(exp1_treat_factor), y = estimate)) +
  scale_color_grey(start = 0.2, end = 0.8) +  
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.05, 
                position = position_dodge(width = 0.5)) + 
  labs(x = "Treatment", y = "Estimate", color = "Group") + 
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),   
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 12)  
  ) +
  theme(legend.position = "none")

if (save == 1) {
  ggsave("implement_preds.jpeg", width = 12, height = 12,
         dpi = 300)
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}

###################
# USEFUL
###################
# create new variable for transparency (to compare)
data$exp1_treat_factor <- as.factor(data$exp1_treat)
data$exp1_useful_factor <- as.factor(data$exp1_useful_code)

# model
model_useful_polr_log <- MASS::polr(exp1_useful_factor ~ exp1_treat_factor, 
                                       data = data, 
                                       Hess = TRUE,
                                       method = c("logistic"))
summary(model_useful_polr_log)
nrow(model_useful_polr_log$fitted.values)
lmtest::coeftest(model_useful_polr_log)
performance::r2(model_useful_polr_log)

####
# slopes
slopes_obj <- avg_slopes(model_useful_polr_log, 
                         type = "probs")
head(slopes_obj)

p_logit_ordered_binned <- slopes_obj %>% 
  ggplot(., aes(x = fct_rev(term), 
                y = estimate, 
                color = contrast, 
                group = fct_rev(group))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  scale_color_grey(start = 0.2, end = 0.8) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(
    axis.title = element_text(size = 16),   
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 12)  
  ) +
  labs(
    title = "",
    x = "Treatment",
    y = "Estimate",
    color = "Group"
  ) 

p_logit_ordered_binned

if (save == 1) {
  ggsave("useful_slopes.jpeg", width = 11, height = 7.3,
         dpi = 300)
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}

##########
# ordered logit pred probs
##########
marginaleffects <- marginaleffects(model_useful_polr_log, type = "probs")
margeff_table <- summary(marginaleffects)
format(margeff_table, scientific = FALSE)

preds <- marginaleffects::predictions(
  model_useful_polr_log,
  newdata = datagrid(exp1_treat_factor = c(0, 1, 2, 3))
)

preds$group <- factor(preds$group , 
                      levels = c("1",
                                 "2",
                                 "3",
                                 "4",
                                 "5"))

preds %>% 
  ggplot(aes(x = factor(exp1_treat_factor), y = estimate)) +
  scale_color_grey(start = 0.2, end = 0.8) +  
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.05, 
                position = position_dodge(width = 0.5)) + 
  labs(x = "Treatment", y = "Estimate", color = "Group") + 
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),   
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 12)  
  ) +
  theme(legend.position = "none")
# +
# theme(legend.position = "bottom")

if (save == 1) {
  ggsave("useful_preds.jpeg", width = 12, height = 12,
         dpi = 300)
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}

###################
# INTEREST (regular logit)
###################
# create new variable for transparency (to compare)
data$exp1_treat_factor <- as.factor(data$exp1_treat)
data$exp1_interest_factor <- as.factor(data$exp_1_interest_code)

# model
model_interest_log <- glm(exp1_interest_factor ~ exp1_treat_factor, family = binomial(link = "logit"), data)
summary(model_interest_log)

length(model_interest_log$residuals)
lmtest::coeftest(model_interest_log)
fmsb::NagelkerkeR2(model_interest_log)

##########
# logit pred probs
##########
marginaleffects <- marginaleffects(model_interest_log, type = "response")
margeff_table <- summary(marginaleffects)
format(margeff_table, scientific = FALSE)

preds <- marginaleffects::predictions(
  model_interest_log,
  newdata = datagrid(exp1_treat_factor = c(0, 1, 2, 3))
)

preds %>% 
  ggplot(aes(x = factor(exp1_treat_factor), y = estimate)) +
  scale_color_grey(start = 0.2, end = 0.8) +  
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.05, 
                position = position_dodge(width = 0.5)) + 
  labs(x = "Treatment", y = "Estimate",) + 
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),   
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 12)  
  ) +
  theme(legend.position = "none")

if (save == 1) {
  ggsave("interest_preds.jpeg", width = 12, height = 12,
         dpi = 300)
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}

