# This file is for data visualization
# It includes the graph for the main paper and demographic/descriptive
# graphs for appendix. Data vis for the covars regressions for appendix 
# is in the imputed analysis file and correlation heatmap for appendix
# is in the descriptive relationships file

library(tidyverse)
library(ggpattern) # for data vis at the end
library(forcats) # for fct_reverse() in plots
library(patchwork) # for patchwork data vis (putting graphs together)
library(DescTools) # for gini simpson index 

################
# switch this if do not want to save the graphics
################
save <- 0 # 1 is save 0 is no

#################
# Reading in data and setting up some globals
################
data <- read.csv("replication/data_recoded.csv")[, -1] # nrow = 567
source("replication/2_analysis.r")

# setting objs for all
dodge <- position_dodge(width = 0.2)

#################
# Preparing plot dataframes
################
# implement
df_plot_implement <- data.frame(
  outcome = c("T1: Technical\nInformation", 
              "T2: Limitations", 
              "T3: Combined"),
  estimate = c(model_implement$coefficients[2], 
               model_implement$coefficients[3], 
               model_implement$coefficients[4]),
  se = c(coef(summary(model_implement))[, "Std. Error"][2], 
         coef(summary(model_implement))[, "Std. Error"][3], 
         coef(summary(model_implement))[, "Std. Error"][4]),
  lower_bound = c(confint(model_implement)[2, 1], 
                  confint(model_implement)[3, 1], 
                  confint(model_implement)[4, 1]),
  upper_bound = c(confint(model_implement)[2, 2], 
                  confint(model_implement)[3, 2], 
                  confint(model_implement)[4, 2]),
  se_rse = c(model_implement_robust[2, 2], 
             model_implement_robust[3, 2], 
             model_implement_robust[4, 2]),
  lower_bound_rse = c(confint(model_implement_robust)[2, 1], 
                  confint(model_implement_robust)[3, 1], 
                  confint(model_implement_robust)[4, 1]),
  upper_bound_rse = c(confint(model_implement_robust)[2, 2], 
                  confint(model_implement_robust)[3, 2], 
                  confint(model_implement_robust)[4, 2])
)

# useful
df_plot_useful <- data.frame(
  outcome = c("T1: Technical\nInformation", 
              "T2: Limitations", 
              "T3: Combined"),
  estimate = c(model_useful$coefficients[2], 
               model_useful$coefficients[3], 
               model_useful$coefficients[4]),
  se = c(coef(summary(model_useful))[, "Std. Error"][2], 
         coef(summary(model_useful))[, "Std. Error"][3], 
         coef(summary(model_useful))[, "Std. Error"][4]),
  lower_bound = c(confint(model_useful)[2, 1], 
                  confint(model_useful)[3, 1], 
                  confint(model_useful)[4, 1]),
  upper_bound = c(confint(model_useful)[2, 2], 
                  confint(model_useful)[3, 2], 
                  confint(model_useful)[4, 2]),
  se_rse = c(model_useful_robust[2, 2], 
             model_useful_robust[3, 2], 
             model_useful_robust[4, 2]),
  lower_bound_rse = c(confint(model_useful_robust)[2, 1], 
                      confint(model_useful_robust)[3, 1], 
                      confint(model_useful_robust)[4, 1]),
  upper_bound_rse = c(confint(model_useful_robust)[2, 2], 
                      confint(model_useful_robust)[3, 2], 
                      confint(model_useful_robust)[4, 2])
)

# interest
df_plot_interest <- data.frame(
  outcome = c("T1: Technical\nInformation", 
              "T2: Limitations", 
              "T3: Combined"),
  estimate = c(model_interest$coefficients[2], 
               model_interest$coefficients[3], 
               model_interest$coefficients[4]),
  se = c(coef(summary(model_interest))[, "Std. Error"][2], 
         coef(summary(model_interest))[, "Std. Error"][3], 
         coef(summary(model_interest))[, "Std. Error"][4]),
  lower_bound = c(confint(model_interest)[2, 1], 
                  confint(model_interest)[3, 1], 
                  confint(model_interest)[4, 1]),
  upper_bound = c(confint(model_interest)[2, 2], 
                  confint(model_interest)[3, 2], 
                  confint(model_interest)[4, 2]),
  se_rse = c(model_interest_robust[2, 2], 
             model_interest_robust[3, 2], 
             model_interest_robust[4, 2]),
  lower_bound_rse = c(confint(model_interest_robust)[2, 1], 
                      confint(model_interest_robust)[3, 1], 
                      confint(model_interest_robust)[4, 1]),
  upper_bound_rse = c(confint(model_interest_robust)[2, 2], 
                      confint(model_interest_robust)[3, 2], 
                      confint(model_interest_robust)[4, 2])
)

# reordering the dfs for plotting
df_plot_implement <- df_plot_implement %>%
  mutate(order_factor = factor(outcome, levels = c("T1: Technical\nInformation", 
                                                   "T2: Limitations", 
                                                   "T3: Combined"))) # adjusting order
df_plot_useful <- df_plot_useful %>%
  mutate(order_factor = factor(outcome, levels = c("T1: Technical\nInformation", 
                                                   "T2: Limitations", 
                                                   "T3: Combined"))) # adjusting order
df_plot_interest <- df_plot_interest %>%
  mutate(order_factor = factor(outcome, levels = c("T1: Technical\nInformation", 
                                                   "T2: Limitations", 
                                                   "T3: Combined"))) # adjusting order

#################
# Main effects data vis
################
# implement
p1 <- ggplot(df_plot_implement, aes(x = fct_rev(outcome), y = estimate)) +
  geom_errorbar(aes(ymin = lower_bound_rse, ymax = upper_bound_rse), width = 0.1) +
  geom_point() +  
  scale_color_manual(values = c("Technical" = "black", 
                                "Limitations" = "black", 
                                "Combined" = "black")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "",
       x = "Outcome 1: \nDesire to implement\n",
       y = "Estimate") +
  guides(color = guide_legend(reverse = TRUE)) +
  ylim(-0.5, 0.5)  +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x  = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.y  = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    legend.text  = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  ) +
  coord_flip()

p1

# useful
p2 <- ggplot(df_plot_useful, aes(x = fct_rev(outcome), y = estimate)) +
  geom_errorbar(aes(ymin = lower_bound_rse, ymax = upper_bound_rse), width = 0.1) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "",
       x = "Outcome 2:\nOpinions on usefulness\n",
       y = "Estimate") +
  guides(color = guide_legend(reverse = TRUE)) +
  ylim(-0.5, 0.5)  +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x  = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.y  = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    legend.text  = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  ) +
  coord_flip()

p2

# interest
p3 <- ggplot(df_plot_interest, aes(x = fct_rev(outcome), y = estimate)) +
  geom_errorbar(aes(ymin = lower_bound_rse, ymax = upper_bound_rse), width = 0.1) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "",
       x = "Outcome 3:\nInterest in learning more\n",
       y = "Estimate") +
  guides(color = guide_legend(reverse = TRUE)) +
  ylim(-0.5, 0.5)  +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x  = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 18, family = "Times New Roman"),
    axis.text.y  = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 18, family = "Times New Roman"),
    legend.text  = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  ) +
  coord_flip()

p3

##############
# plotting with patchwork
p1 / p2 / p3

if (save == 1) {
  
  # saving as jpeg
  ggsave("combined_plot_main.jpeg", p1 / p2 / p3, width = 8, height = 12,
         dpi = 800) 
  
  # saving as eps
  ggsave(
    "combined_plot_main.eps",
    p1 / p2 / p3,
    width = 8,
    height = 12,
    device = function(...) grDevices::cairo_ps(..., fallback_resolution = 800)
  )
  
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}



#################
#### more data vis to look at raw implement outcome
################
data$exp1_implement_code <- recode(data$exp1_implement,  
                                   `I feel strongly this SHOULD be implemented in Nepal.` = 4,
                                   `I think it is a good thing to implement but not a priority.` = 3,
                                   `I have no strong opinions about its implementation.`  = 2,
                                   `I think it probably shouldn’t be implemented, but I do not feel strongly about it.` = 1,
                                   `I feel strongly this should NOT be implemented in Nepal.` = 0)

# average
data %>% 
  select(exp1_treat, exp1_implement_code) %>% # if you're getting an 'error in select(...) detatch MASS
  group_by(exp1_treat) %>% 
  drop_na() %>%
  summarize(Mean = mean(exp1_implement_code, na.rm=TRUE), 
            std_dev = sd(exp1_implement_code, na.rm=TRUE),
            se = sd(exp1_implement_code, na.rm=TRUE) / sqrt(nrow(.))) %>% 
  ggplot(., aes(x = as.factor(exp1_treat), y = Mean)) +
  geom_col(fill = "gray", width=0.45) +
  #ylim(0, 4) +
  # scale_y_discrete(labels = c("1", "2", "3", "4","5")) +
  scale_x_discrete(labels = c("Control", "T1", "T2", "T3")) +
  labs(x = "Treatment", y = "Average Outcome") + #,
  # title = "National trust") +
  theme_minimal(base_family = "Times New Roman")  +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    # legend.text = element_text(size = 20),  
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  )  +
  theme(plot.title = element_text(hjust = 0.5, family = "Times New Roman"))

#################
#### data vis for the descriptives for appendix
################
# https://bookdown.org/wadetroberts/r-you-ready-for-r/descriptive-statistics-and-data-visualization.html
c("experience_score", "anxiety", "eews_know",
                      "receive_warning", "display", "behavior",
                      "small_behavior")


#### anxiety_code
d1 <- data %>% 
  ggplot(., aes(x = anxiety_code)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Anxiety\n(1 = lowest and 5 = highest)") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    # legend.text = element_text(size = 20),  
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d1

#### know_code
d2 <- data %>% 
  ggplot(., aes(x = eews_know_code)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Knowledge of EEWS\n(1 = none and 5 = highest)") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman")  +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d2

#### experience_score
d3 <- data %>% 
  select(., experience_score) %>% 
  ggplot(aes(x = experience_score)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  xlab("Earthquake experience\n(0 = none and 19 = highest)") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d3

#### experience_index
d3b <- data %>% 
  select(., experience_index) %>% 
  ggplot(aes(x = experience_index)) +
  geom_density() +
  scale_y_continuous(name = "Density") +
  xlab("Earthquake experience\n(0 = none and 1 = highest)") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d3b

# cutting out a govt hazards here, which is why there's a gap in the numbers

#### Behavior when earthquake hits
# percentage/proportion bar graph
d5 <- data %>% 
  filter(!is.na(behavior) & 
           behavior != "Skip" &
           behavior != "") %>%
  mutate(
    behavior = factor(
      behavior,
      levels = c("Other", 
                 "Stay calm and do nothing", 
                 "I don’t know what I would do",
                 "Try to reach my friends, family or anyone near me", 
                 "Take cover and wait for the shaking to stop",
                 "Run outside to open space") 
    )
  ) %>% 
  ggplot(., aes(x = behavior)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Projected behavior") +
  scale_x_discrete(labels = c("Run outside to open space" = 
                                "Run outside\nto open\nspace", 
                              "Stay calm and do nothing" = 
                                "Stay calm\nand do nothing", 
                              "Take cover and wait for the shaking to stop" = 
                                "Take cover\nand wait for\nthe shaking\nto stop", 
                              "Try to reach my friends, family or anyone near me" = 
                                "Try to reach\nmy friends,\nfamily or anyone\nnear me",
                              'I don’t know what I would do' =
                                "I don’t know\nwhat I would\ndo")) +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d5

#### receive warning
# percentage/proportion bar graph
d6 <- data %>%   
  filter(!is.na(receive_warning) & 
           receive_warning != "Skip" &
           receive_warning != "" &
           receive_warning != "NA") %>%
  mutate(
    receive_warning = factor(
      receive_warning,
      levels = c("I don't know", 
                 "Other",
                 "Radio message", 
                 "TV message",
                 "Social media (Facebook, Twitter)", 
                 "A specific device designed to receive an earthquake early warning",
                 "Public announcements (via a loudspeaker or siren)",
                 "Mobile phone alert (app)") 
    )
  ) %>% 
  ggplot(., aes(x = receive_warning)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Warning dissemination\npreference") +
  scale_x_discrete(labels = c("I don't know" =
                                "\nI don't\nknow",
                              "Other" =
                                "Other",
                              "Radio message" = 
                                "\nRadio", 
                              'TV message' =
                                "TV",
                              "Social media (Facebook, Twitter)" = 
                                "\nSocial\nmedia\n(Facebook,\nTwitter)",
                              "A specific device designed to receive an earthquake early warning" =
                                "EEWS\ndevice",
                              "Public announcements (via a loudspeaker or siren)" = 
                                "\nPublic\nannouncement\n(loudspeaker\nor siren)", 
                              "Mobile phone alert (app)" = 
                                "Mobile\napp")) +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman")  +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d6

#### opinion_ewws_need
d7 <- data %>% 
  filter(!is.na(opinion_ewws_need) & 
           opinion_ewws_need != "Skip" &
           opinion_ewws_need != "" &
           opinion_ewws_need != "NA") %>%
  mutate(
    opinion_ewws_need = factor(
      opinion_ewws_need,
      levels = c("I don’t understand", 
                 "I don’t know",
                 "For all earthquakes that will certainly cause damage.",
                 "For all earthquakes that may cause damage.", 
                 "For all earthquakes, I will certainly feel.",
                 "For all earthquakes, I may feel.",
                 "For all earthquakes, independent of whether the shaking could be felt or not.") 
    )
  ) %>% 
  select(., opinion_ewws_need) %>% 
  ggplot(aes(x = opinion_ewws_need)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  scale_x_discrete(labels = c("I don’t understand" = 
                                "\nI don't\nunderstand", 
                              "I don’t know" =
                                "I don’t\nknow", 
                              "For all earthquakes that will certainly cause damage." = 
                                "\nCertainly \ncause\ndamage",
                              "For all earthquakes that may cause damage." = 
                                "May\ncause\ndamage", 
                              "For all earthquakes, I will certainly feel." =
                                "\n Certainly\nfeel",
                              'For all earthquakes, I may feel.' =
                                "May feel",
                              "For all earthquakes, independent of whether the shaking could be felt or not." =
                                "\nAll\nwhether\nor not\nshaking will\nbe felt")) +
  xlab("Preferences for types\nof earthquakes\nfor alert") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    # legend.text = element_text(size = 20),  
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d7

# display
# Time available before shaking begins
# Magnitude of earthquake
# Expected intensity of ground shaking
# Epicenter (point of earthquake origin)
# Expected duration of shaking
# Recommended actions
# Other
# I don’t know
# Skip 

split_phrases <- strsplit(data$display, ",\\s*") # splits by comma and removes extra spaces
all_phrases <- unlist(split_phrases)
phrase_counts <- table(all_phrases)

data_phrase <- as.data.frame(phrase_counts)
colnames(data_phrase) <- c("phrase", "count")
data_phrase$proportion <- data_phrase$count / sum(data_phrase$count)

d8 <- data_phrase %>% 
  filter(!is.na(phrase) & 
           phrase != "Skip" &
           phrase != "" &
           phrase != "NA") %>%
  ggplot(., aes(x = reorder(phrase, count), y = count)) +
  geom_bar(stat = "identity") +
  labs(
    x = "\nPreferences for\ninformation on display",
    y = "Count"
  ) +
  scale_x_discrete(labels = c("Epicenter (point of earthquake origin)" = 
                                "Epicenter", 
                              "Expected duration of shaking" =
                                "Expected\nduration\nof shaking", 
                              "Expected intensity of ground shaking" = 
                                "Expected\nintensity of\nground shaking", 
                              "I don’t know" = 
                                "I don’t\nknow",
                              "I don’t understand" = 
                                "I don’t\nunderstand",
                              'Other' =
                                "Other",
                              "Recommended actions" =
                                "Recommended\nactions",
                              "Time available before shaking begins" =
                                "Time before\nshaking begins",
                              "Magnitude of earthquake" =
                                "Magnitude")) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    # legend.text = element_text(size = 20),  
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d8


# small behavior
d9 <- data %>% 
  filter(!is.na(small_behavior) & 
           small_behavior != "Skip" &
           small_behavior != "" &
           small_behavior != "NA") %>%
  mutate(
    small_behavior = factor(
      small_behavior,
      levels = c("I don't know",
                 "My behavior would be the same for all earthquakes.",
                 "I would change my behavior depending on how big the earthquake was expected to be.")
    )
  ) %>%
  select(., small_behavior) %>% 
  ggplot(aes(x = small_behavior)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  scale_x_discrete(labels = c("I would change my behavior depending on how big the earthquake was expected to be." = 
                                "Change depending\non size", 
                              "I don't know" =
                                "I don’t know", 
                              "My behavior would be the same for all earthquakes." =
                                "Same for all\nearthquakes")) +
  xlab("Projected changes in\nbehavior based on earthquake type") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d9

#############
# patching together
#################

(d6 + d7) / d8
d5 / d9
d1 + d2 + d3

if (save == 1) {
  ggsave("data_vis_prefs.jpeg", (d6 + d7) / d8, width = 14, height = 12,
         dpi = 300) 
  ggsave("data_vis_behavior.jpeg", d5 / d9, width = 12, height = 12,
         dpi = 300) 
  ggsave("data_vis_other.jpeg", (d1 + d2) / d3, width = 12, height = 12, # 4/16 note taking out d4 (trust in govt)
         dpi = 300) 
} else {
  message("Plot not saved (save != 1).")
}


#################
#### data vis for the demographics for appendix 
################
# "age", "gender",  "married"                  
# "education", "caste_ethnicity",      
# "rural_urban
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# age
# recoding the emptys and don't knows as skips
data$age <- ifelse(data$age == "", NA, 
                   ifelse(grepl("know", data$age) == TRUE,
                                              NA, ifelse(data$age == "76", 
                                                         "76+", data$age)))
d_age <- data %>% 
  filter(!is.na(age)) %>% 
  ggplot(., aes(x = age)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Age") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d_age
length(data[data$age %in%  c('51-55', '56-60', '61-65', '66-70', '71-75', '76+'), "age"]) / length(data$age)

# gender
# recoding the emptys and don't knows as skips
data$gender <- ifelse(data$gender == "", NA, 
                      ifelse(grepl("Skip", data$gender) == TRUE,
                             NA, data$gender))
d_gender <- data %>% 
  filter(!is.na(gender)) %>% 
  ggplot(., aes(x = gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Gender") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d_gender
length(data[data$gender %in%  c('Male'), "gender"]) / length(data$gender)

# married
# recoding the emptys and don't knows as skips
data$married <- ifelse(data$married == "", NA, 
                       ifelse(grepl("Skip", data$married) == TRUE,
                              NA, data$married))
data$married <- factor(data$married, 
                       levels = c("Separated", 
                                  "Widowed",
                                  "Divorced", 
                                  "Single (never married)",
                                  "Married"
                       ))
d_married <- data %>% 
  filter(!is.na(married)) %>% 
  ggplot(., aes(x = married)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Marital status") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  scale_x_discrete(labels = c("Single (never married)" = "Single\n(never\nmarried)")) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d_married
length(data[data$married %in%  c('Married'), "married"]) / length(data$married)

# education
# recoding the emptys and don't knows as skips & refactoring
data$education <- ifelse(data$education == "", NA, 
                         ifelse(grepl("Skip", data$education) == TRUE,
                                NA, data$education))
data$education <- factor(data$education, 
                         levels = c("No formal education", 
                                    "Pre-primary and/or primary",
                                    "Secondary", 
                                    "University (Bachelor)",
                                    "Post-graduate"
                         ))
d_education <- data %>% 
  filter(!is.na(education)) %>% 
  ggplot(., aes(x = education)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Education") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  scale_x_discrete(labels = c("No formal education" = "No formal\neducation",
                              "Pre-primary and/or primary" = "Pre-primary\nor primary",
                              "University (Bachelor)" = "University\n(Bachelor)",
                              "Post-graduate" = "Post-\ngraduate")) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d_education
length(data[data$education %in%  c('University (Bachelor)', 'Post-graduate'), "education"]) / length(data$education)

# caste_ethnicity
# recoding the emptys and don't knows as skips & refactoring
data$caste_ethnicity <- ifelse(data$caste_ethnicity == "", NA, 
                               ifelse(grepl("Skip", data$caste_ethnicity) == TRUE,
                                      NA, data$caste_ethnicity))
data$caste_ethnicity <- factor(data$caste_ethnicity, 
                               levels = c("Bahun", 
                                          "Chhetri",
                                          "Gurung", 
                                          "Limbu",
                                          "Madhesh",
                                          "Magar",
                                          "Newar (Newa)",
                                          "Rai",
                                          "Tamang",
                                          "Thakuri",
                                          "Tharu",
                                          "Yadav",
                                          "Other"
                               ))
d_caste_ethnicity <- data %>% 
  filter(!is.na(caste_ethnicity)) %>% 
  ggplot(., aes(x = caste_ethnicity)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Caste/ethnicity (alphabetical order)") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  scale_x_discrete(labels = c("Newar (Newa)" = "Newar\n(Newa)")) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d_caste_ethnicity
length(data[data$caste_ethnicity %in%  c('Bahun', 'Chhetri'), "caste_ethnicity"]) / length(data$caste_ethnicity)

# rural urban
# recoding the emptys and don't knows as skips & refactoring
data$rural_urban <- ifelse(data$rural_urban == "", NA, 
                           ifelse(grepl("Skip", data$rural_urban) == TRUE,
                                  NA, data$rural_urban))
data$rural_urban <- factor(data$rural_urban, 
                           levels = c("Something else", 
                                      "Town",
                                      "Village", 
                                      "City"
                           ))
d_rural_urban <- data %>% 
  filter(!is.na(rural_urban)) %>% 
  ggplot(., aes(x = rural_urban)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Rural/urban") +
  scale_y_continuous(labels = scales::percent_format(), name = "Proportion") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),   
    title = element_text(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 16, family = "Times New Roman")  
  ) 
d_rural_urban
length(data[data$rural_urban %in%  c('City'), "rural_urban"]) / length(data$rural_urban)

#############
# patching together
#################
# "age", "gender",  "married"                  
# "education", "caste_ethnicity",      
# "rural_urban

(d_married + d_gender) / 
  d_education / d_rural_urban /
d_age / d_caste_ethnicity

(d_married + d_gender) / (d_education + d_rural_urban) /
  d_age / d_caste_ethnicity

if (save == 1) {
  ggsave("data_vis_demographics.jpeg", (d_married + d_gender) / (d_education + d_rural_urban) /
           d_age / d_caste_ethnicity, width = 14, height = 15,
         dpi = 300) 
} else {
  message("Plot not saved (save != 1).")
}

################
# demographic and descriptive tables
#################
# table for demographic summary
main_table <- data %>%
  # group_by(exp1_treat) %>%
  mutate(
    gender = as.factor(gender),
    age = as.factor(age),
    married = as.factor(married),
    education = as.factor(education),
    caste_ethnicity = as.factor(caste_ethnicity),
    rural_urban_code = as.factor(rural_urban_code)) %>% 
  summarize(
    length_gender = length(na.omit(gender)),        
            mode_gender = mode(gender),
            length(data[data$gender %in%  c('Male'), "gender"]) / length(data$gender),
            unalike_gender = DescTools::GiniSimpson((gender)),
    length_age = length(na.omit(age)),       
            mode_age = mode(age),
            percent_age = length(data[data$age %in%  c('21-25'), "age"]) / length(data$age),
            unalike_age = DescTools::GiniSimpson((age)),
    length_married = length(na.omit(married)),       
            mode_married = mode(married),
            percent_married = length(data[data$married %in%  c('Single (never married)'), "married"]) / length(data$married),
            unalike_married = DescTools::GiniSimpson((married)),
    length_edu = length(na.omit(education)),
            mode_edu = mode(education),
            percent_edu = length(data[data$education %in%  c('University (Bachelor)'), "education"]) / length(data$education),
            unalike_edu = DescTools::GiniSimpson((education)),
    length_ethnicity = length(na.omit(caste_ethnicity)),
            mode_ethnicity = mode(caste_ethnicity),
            length(data[data$caste_ethnicity %in%  c('Bahun'), "caste_ethnicity"]) / length(data$caste_ethnicity),
            unalike_ethnicity = DescTools::GiniSimpson((caste_ethnicity)),
    length_rural_urban = length(na.omit(rural_urban_code)),
            mode_rural_urban = mode(rural_urban),
            length(data[data$rural_urban %in%  c('City'), "rural_urban"]) / length(data$rural_urban),
            unalike_rural_urban = DescTools::GiniSimpson((rural_urban))
    ) # nas are the ones who stopped or skipped
matrix(main_table, ncol = 4, byrow = TRUE)

# table for preferences and experiences
data$anxiety <- ifelse(data$anxiety == "", NA, 
                      ifelse(grepl("Skip", data$anxiety) == TRUE,
                             NA, data$anxiety))
data$eews_know <- ifelse(data$eews_know == "", NA, 
                      ifelse(grepl("Skip", data$eews_know) == TRUE,
                             NA, data$eews_know))
data$receive_warning <- ifelse(data$receive_warning == "", NA, 
                       ifelse(grepl("Skip", data$receive_warning) == TRUE,
                              NA, data$display))
data$display <- ifelse(data$display == "", NA, 
                      ifelse(grepl("Skip", data$display) == TRUE,
                             NA, data$display))
data$behavior <- ifelse(data$behavior == "", NA, 
                      ifelse(grepl("Skip", data$behavior) == TRUE,
                             NA, data$behavior))
data$small_behavior <- ifelse(data$small_behavior == "", NA, 
                      ifelse(grepl("Skip", data$small_behavior) == TRUE,
                             NA, data$small_behavior))
data$past_experience <- ifelse(data$past_experience == "", NA, 
                                  ifelse(grepl("Skip", data$past_experience) == TRUE,
                                         NA, data$past_experience))
data$opinion_ewws_need <- ifelse(data$opinion_ewws_need == "", NA, 
                               ifelse(grepl("Skip", data$opinion_ewws_need) == TRUE,
                                      NA, data$opinion_ewws_need))

vars_for_table <- c(
  "anxiety", "eews_know",
  "receive_warning", "display", "behavior",
  "small_behavior", "opinion_ewws_need")
results_table <- data %>%
  pivot_longer(all_of(vars_for_table), names_to = "variable", values_to = "value") %>%
  count(variable, value, name = "count") %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(variable, desc(count)) %>%
  ungroup()
results_table

data %>%
  summarize(
    anxiety = length(na.omit(anxiety)),
    eews_know = length(na.omit(eews_know)),
    behavior = length(na.omit(behavior)),
    small_behavior = length(na.omit(small_behavior)),
    experience = length(na.omit(past_experience)),
    recieve_warning = length(na.omit(receive_warning)),
    warning_info = length(na.omit(display)),
    threshold = length(na.omit(opinion_ewws_need))
    
  )

# NOTE: past experience and preference in display info not mutually exclusive
# and are summmarized separately below

##################
# # experience
################
# removing commas out of long items
problematic_items <- c(
  "Know family or friends who experienced injury, damage, or loss from an earthquake" = 
    "Know family or friends",
  "Personally have felt strong shaking (i.e. where walking steadily is difficult. Furniture and appliances may move on smooth surfaces, and objects fall from walls and shelves.)" =
    "Personally have felt strong shaking",
  "Personally have experienced injury, damage, or loss from an earthquake" =
    "Personally have experienced injury",
  "Have seen the effects of the earthquake on TV, social media, etc." =
    "Have seen the effects of",
  "Observed earthquake loss in my neighborhood, village, or city" =
    "Observed"
)

# replace problematic items in the text
for (item in names(problematic_items)) {
  # escape the problematic item to handle special characters in the regex
  escaped_item <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", item)
  # replace only full matches of the problematic item
  data$past_experience <- gsub(
    escaped_item,
    problematic_items[[item]],
    data$past_experience
  )
}

unique_options_exp <- data$past_experience %>%
  str_split(",") %>%       
  unlist() %>%             
  unique() %>%             
  sort()                   
unique_options_exp

option_counts_exp <- data$past_experience %>%
  str_split(",") %>%      
  unlist() %>%            
  tibble(option = .) %>%   
  count(option, name = "count") %>%
  mutate(percentage = count / nrow(data) * 100) %>% 
  arrange(desc(count))    
option_counts_exp


##################
# # preference
################

unique_options_pref <- data$display %>%
  str_split(",") %>%       
  unlist() %>%             
  unique() %>%             
  sort()                   
unique_options_pref

option_counts_pref <- data$display %>%
  str_split(",") %>%      
  unlist() %>%            
  tibble(option = .) %>%   
  count(option, name = "count") %>%
  mutate(percentage = count / length(unlist(str_split(data$past_experience, ","))) * 100) %>%
  arrange(desc(count))    
option_counts_pref
