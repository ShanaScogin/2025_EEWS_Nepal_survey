# this file is to run the analysis on imputed data
# this is due to the issue of missing data and observations dropping out
# which, with our small sample size, is not ideal
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/

library(mice)
library(tidyverse)

save <- 0 # 1 is save anything else is no

###############
# read in data and check out the NAs
data_impute <- read.csv("replication/data_uncoded.csv")[, -1] # nrow = 567
sapply(data_impute, function(x) sum(is.na(x))) # they're all empty ""
#data_impute <- data_impute[, setdiff(colnames(data_impute), "X"), drop = FALSE]

# code skips as NAs
data_impute[data_impute == ""] <- NA
data_impute[data_impute == "Skip"] <- NA
sapply(data_impute, function(x) sum(is.na(x))) # fixed 

##############
# imputation
###############
# turning variables into correct format
data_impute <- data_impute %>%
  mutate(
    UserLanguage = as.factor(UserLanguage),
    age = as.factor(age), 
    gender = as.factor(gender),
    married = as.factor(married),
    education = as.factor(education),
    caste_ethnicity = as.factor(caste_ethnicity),
    district = as.factor(district),
    rural_urban = as.factor(rural_urban),
    anxiety = as.factor(anxiety),
    eews_know = as.factor(eews_know),
    opinion_ewws_need = as.factor(opinion_ewws_need),
    receive_warning = as.factor(receive_warning),
    behavior = as.factor(behavior),
    small_behavior = as.factor(small_behavior),
    time = as.numeric(time)
  )

# get MICE set up
init <- mice(data_impute, maxit = 0) 
meth <- init$method
predM <- init$predictorMatrix

# now remove the variables that will not be used as predictors
vect_exclude_both <- c("UserLanguage", "RecordedDate", "exp1_treat",
                  "ask_again",  
                  "exp_1_interest",
                  "exp_1_useful", "exp1_implement",
                  "past_experience",
                  "display", "caste_ethnicity_aru",
                  "opinion_ewws_need", 
                  "receive_warning", "behavior", 
                  "small_behavior" #,
                  # "open_end","govt_trust_hazards", "trust_most_text"
                  )
length(vect_exclude_both)

predM[, vect_exclude_both] = 0
meth[vect_exclude_both] = ""

vect_imputing_cat <- c("gender", "married",
                  "caste_ethnicity", "district",
                   "rural_urban") 
vect_imputing_ord <- c("age", "education",
                       "anxiety", "eews_know")
vect_imputing_cont <- c("time")

length(vect_imputing_cat)
length(vect_imputing_ord)
length(vect_imputing_cont)

sum(length(vect_exclude_both), length(vect_imputing_cat),
    length(vect_imputing_ord), length(vect_imputing_cont))
ncol(data_impute)

# specify methods for missing values
meth[vect_imputing_cat]="pmm" # could maybe do poly or polyreg?
meth[vect_imputing_ord]="polyreg" 
meth[vect_imputing_cont]="norm" 

# run imputation
set.seed(98249)
imputed <- mice(data_impute, 
                method = meth, 
                predictorMatrix = predM, 
                m = 5)
imputed_complete <- complete(imputed, action = "long", include = TRUE)
sapply(imputed_complete, function(x) sum(is.na(x)))

#########
if (save == 1) {
  write.csv(imputed_complete, "replication/data_imputed.csv")
  message("df saved")
} else {
  message("df not saved (save != 1).")
}
# rm(list = ls())

############
# checking the data
#############
# https://www.rdocumentation.org/packages/mice/versions/3.17.0/topics/mice
# first plot the imputed data 
plot(imputed) # plot shows no real trends

# next, going to increase iterations to look at trends closer
imp40 <- mice.mids(imputed, maxit=35, print=F)
plot(imp40)

# could also look at the imputed data compared to real values for plausibility



