# this file looks at the data before doing the analysis
# it also does balance tests etc
# it is not necessary for the analysis or data vis
# but gives a better idea of the data

library(tidyverse)
library(mice) # for checking balance with imputed data
library(miceadds) # for anova test
library(RItools) # for balance


##############
# read in data
data <- read.csv("replication/data_recoded.csv") # nrow = 567
d <- subset(data, time > 3)

# some indicators of data
median(data$time)
min(data$time)
max(data$time) # long ones just left open and came back to it

# not including open ended question in the public data since it needs to be checked for any
# potentially identifying information that was entered by users
# nrow(subset(data, open_end != "")) / nrow(data) 
# nrow(subset(data, open_end != "" & open_end != "No" & open_end != "chaina" )) / nrow(data) # cannot put Devanagari for some reason

data %>% # proportions
  count(UserLanguage) %>% # 0.7848325
  mutate(prop = n / sum(n)) # 0.2151675

# check out the NAs in the treats and outcomes
nrow(data[is.na(data$exp1_treat), ]) # 16
nrow(data[is.na(data$exp1_implement_code), ]) # 30
nrow(data[is.na(data$exp1_useful_code), ]) # 31
nrow(data[is.na(data$exp_1_interest_code), ]) # 54

nrow(data) # 567
nrow(data[!is.na(data$exp1_treat) &
            !is.na(data$exp1_implement_code), ]) # 537
nrow(data[is.na(data$exp1_treat) |
            is.na(data$exp1_implement_code), ]) # 30 # matches fit in analysis.r
nrow(data[!is.na(data$exp1_treat) &
            !is.na(data$exp1_useful_code), ]) # 536
nrow(data[is.na(data$exp1_treat) |
            is.na(data$exp1_useful_code), ]) # 31 # matches fit in analysis.r
nrow(data[!is.na(data$exp1_treat) &
            !is.na(data$exp_1_interest_code), ]) # 513
nrow(data[is.na(data$exp1_treat) |
            is.na(data$exp_1_interest_code), ]) # 54 # matches fit in analysis.r

##############
# visual count for treatments
data %>% 
  select(exp1_treat) %>% # if you're getting an 'error in select(...) detatch MASS
  drop_na() %>%
  ggplot(., aes(x = as.factor(exp1_treat))) +
  geom_bar(stat="count", position = "dodge", fill = "gray", width=0.45) +
  theme_minimal() 
# lower in 0 and 2 - might cause power issues - 1 and 3 highest

# nonvisual exploration
with(data, table(exp1_treat, exp1_implement_code)) # table
with(data, table(exp1_treat, exp1_useful_code)) # table
with(data, table(exp1_treat, exp_1_interest_code)) # table

rowSums(with(data, table(exp1_treat, exp1_implement_code))) # sums for each treatment
rowSums(with(d, table(exp1_treat, exp1_implement_code))) # sums for each treatment with higher time counts

rowSums(with(data, table(exp1_treat, exp1_useful_code))) # sums for each treatment
rowSums(with(d, table(exp1_treat, exp1_useful_code))) # sums for each treatment with higher time counts

rowSums(with(data, table(exp1_treat, exp_1_interest_code))) # sums for each treatment
rowSums(with(d, table(exp1_treat, exp_1_interest_code))) # sums for each treatment with higher time counts

data %>% # proportions
  count(exp1_treat) %>%
  mutate(prop = n / sum(n)) 
# they are in the same ballpark - more T1 and T3s than control and T2

# counts (and NAs) by covar
data %>% 
  count(experience_score) 
data %>% 
  count(gender_code) # 9 NAs
data %>% 
  count(rural_urban_code) # 9 NAs
data %>% 
  count(eews_know_code) # 11 NAs
data %>% 
  count(anxiety_code) # 12 NAs
data %>%
  count(education_code) # 17 NAs
data %>%
  count(caste_ethnicity_code) # 19 NAs
data %>% 
  count(married_code) # 23 NAs
data %>% 
  count(age_code) # 24 NAs


# proportions by covar
data %>% # proportions
  count(age_code) %>% 
  mutate(prop = n / sum(n)) 
data %>% # proportions
  count(rural_urban_code) %>% 
  mutate(prop = n / sum(n)) 
data %>% # proportions
  count(gender_code) %>% # this must be a literacy thing - they all got deleted
  mutate(prop = n / sum(n)) 
data %>% # proportions
  count(education_code) %>% #
  mutate(prop = n / sum(n)) 
data %>% # proportions
  count(caste_ethnicity_code) %>% #
  mutate(prop = n / sum(n)) 
data %>% # proportions
  count(married_code) %>% #
  mutate(prop = n / sum(n)) 

####################
# taking a look at balance by covars and testing that randomization worked
####################
########
# first for basic table
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

bal_table <- data %>%
  group_by(exp1_treat) %>%
  summarize(ave_gender = mean(gender_code, na.rm = TRUE),
            ave_age_bin = mean(age_code, na.rm = TRUE),
            ave_edu = mean(education_code, na.rm = TRUE),
            ave_experience_score = mean(experience_score, na.rm = TRUE),
            ave_anxiety = mean(anxiety_code, na.rm = TRUE),
            ave_ethnicity = mean(caste_ethnicity_code, na.rm = TRUE,),
            ave_rural_urban = mean(rural_urban_code, na.rm = TRUE,),
            ave_married = mean(married_code, na.rm = TRUE,),
            ave_eews_know = mean(eews_know_code, na.rm = TRUE,)
            ) # nas are the ones who stopped or skipped
bal_table

########
# now for model with treatment as outcome 
model <- lm(exp1_treat ~
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
summary(model)
# nothing significant, so that means these covars don't predict the treatment
# this suggests randomization worked

##############
##### MAIN BALANCE: using hansen and bowers package
##############
covars <- c(
  "experience_score", # treating as continuous bc > 10 categories
  "gender_code", # unordered categorical
  "rural_urban_code",  # unordered categorical
  "eews_know_code", # ordered
  "anxiety_code", # ordered
  "education_code", # ordered
  "caste_ethnicity_code", # unordered categorical
  "married_code", # unordered categorical
  "age_code" # ordered
)

# coerce common encodings 
df_bal <- data %>%
  mutate(
    treat_numeric = exp1_treat,
    treat_factor = factor(exp1_treat, 
                          levels = c(0,1,2,3), 
                          labels = c("Control","T1","T2","T3")),
    gender_code = factor(gender_code),
    rural_urban_code = factor(rural_urban_code),
    eews_know_code = ordered(eews_know_code),
    caste_ethnicity_code = factor(caste_ethnicity_code),
    married_code = factor(married_code),
    experience_score = experience_score,
    anxiety_code     = ordered(anxiety_code),
    education_code   = ordered(education_code),
    age_code         = ordered(age_code)
  )

sum(is.na(df_bal$gender_code))
sum(is.na(df_bal$age_code))

# have to filter out NAs from treat here bc doesn't cut them out
# there are arguments to impute those, but we have not done this
df_bal_use <- df_bal %>% # 551
  dplyr::filter(!is.na(treat_numeric)) %>%
  dplyr::mutate(treat_numeric = as.integer(treat_numeric))

fmla_num <- stats::as.formula( paste("treat_numeric ~", 
                                     paste(covars, collapse = " + ")) )

xb <- RItools::xBalance(
  fmla_num,
  data   = df_bal_use,
  strata = list(unstrat = NULL),
  report = c("adj.means","std.diffs","z.scores","chisquare.test","p.values")
)
xb

# pairwise Control vs one arm (binary balanceTest)
pair_bt <- function(arm_label) {
  d <- dplyr::filter(df_bal, treat_factor %in% c("Control", arm_label))
  d <- droplevels(d)
  d$Z <- as.integer(d$treat_factor == arm_label)
  
  RItools::balanceTest(
    stats::as.formula(paste("Z ~", paste(covars, collapse = " + "))),
    data = d
  )
}

bt_T1 <- pair_bt("T1")
bt_T2 <- pair_bt("T2")
bt_T3 <- pair_bt("T3")

# vis all
plot(xb, statistic = "std.diff", absolute = TRUE, ggplot = TRUE)

# vis each pair
plot(bt_T1, statistic = "std.diff", absolute = TRUE, groups = rep("", nrow(as.data.frame(bt_T1))))
plot(bt_T2, statistic = "std.diff", absolute = TRUE, groups = rep("", nrow(as.data.frame(bt_T1))))
plot(bt_T3, statistic = "std.diff", absolute = TRUE, groups = rep("", nrow(as.data.frame(bt_T1))))

# helpers to compute and summarize max absval smd across Control vs T1/T2/T3
tidy_std_diff <- function(xb_obj) {
  df <- as.data.frame(xb_obj)
  df$var <- rownames(df)
  rownames(df) <- NULL
  
  nm <- names(df)
  
  #### find the std.diff column (handles "std.diff", "std.diff.unstrat", casing variants)
  std_cols <- nm[grepl("std\\.diff", nm, ignore.case = TRUE)]
  
  if (length(std_cols) == 0L) {
    stop(
      "Could not find a standardized-difference column in xBalance output.\n",
      "Available columns are:\n  ",
      paste(nm, collapse = ", ")
    )
  }
  
  #### pick the first match (could also pick specific ones if were stratified but mine aren't)
  std_col <- std_cols[1]
  
  dplyr::as_tibble(df) %>%
    dplyr::transmute(
      var,
      std.diff = .data[[std_col]]
    ) %>%
    dplyr::filter(!grepl("\\.NATRUE$", var)) %>%
    dplyr::mutate(
      base_var = sub("(NA|[0-9]+)$", "", var),
      abs_smd  = abs(std.diff)
    ) %>%
    dplyr::group_by(base_var) %>%
    dplyr::summarise(max_abs_smd = max(abs_smd, na.rm = TRUE), .groups = "drop")
}

# run the three pairwise comparisons (Control vs T1/T2/T3)
pair_xb <- function(data, arm_label, covars) {
  #### build a binary indicator Z for the chosen arm vs Control
  d <- dplyr::filter(data, treat_factor %in% c("Control", arm_label))
  d <- droplevels(d)
  d$Z <- as.integer(d$treat_factor == arm_label)
  
  #### run xBalance for the binary comparison (gives std.diffs per column of the design matrix)
  fmla <- stats::as.formula(paste("Z ~", paste(covars, collapse = " + ")))
  RItools::xBalance(
    fmla,
    data   = d,
    strata = list(unstrat = NULL),
    report = c("adj.means","std.diffs","z.scores","chisquare.test","p.values")
  )
}

xb_T1 <- pair_xb(df_bal, "T1", covars)
xb_T2 <- pair_xb(df_bal, "T2", covars)
xb_T3 <- pair_xb(df_bal, "T3", covars)

smd_T1 <- tidy_std_diff(xb_T1) %>% dplyr::rename(max_abs_smd_T1 = max_abs_smd)
smd_T2 <- tidy_std_diff(xb_T2) %>% dplyr::rename(max_abs_smd_T2 = max_abs_smd)
smd_T3 <- tidy_std_diff(xb_T3) %>% dplyr::rename(max_abs_smd_T3 = max_abs_smd)

smd_summary <- smd_T1 %>%
  dplyr::full_join(smd_T2, by = "base_var") %>%
  dplyr::full_join(smd_T3, by = "base_var") %>%
  dplyr::mutate(
    max_abs_smd_overall = pmax(max_abs_smd_T1, max_abs_smd_T2, max_abs_smd_T3, na.rm = TRUE),
    flag_0_1 = max_abs_smd_overall > 0.10, # i.e., not stat sig if TRUE at 0.1
    flag_0_05 = max_abs_smd_overall > 0.05 # i.e., not stat sig if TRUE at 0.05
  )  %>% 
  dplyr::arrange(dplyr::desc(max_abs_smd_overall))

smd_summary # nothing stat sig

# note on this: I calculated the SMDs on my own apart from this and they
# were a bit different, I think because of different NA handling, but
# trends were the same, so I think this looks good


