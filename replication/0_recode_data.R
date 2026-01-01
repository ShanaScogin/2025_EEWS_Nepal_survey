# this file recodes the data
# it isn't necessary to run since I included the data
# but it is included for transparency

library(tidyverse)

############################
# switch this - the flag for imputed or not
# 1 for yes, 0 (or anything else) for no
imputed_yes <- 0


###################
# reading in the uncoded data
if (imputed_yes == 1) {
  data <- read.csv("replication/data_imputed.csv")[, -1] # nrow = 567
} else {
  data <- read.csv("replication/data_uncoded.csv")[, -1] # nrow = 567
}
sapply(data, function(x) sum(is.na(x))) 

###################
# coding exp1 data: implementation
###################
data$exp1_implement_code <- recode(data$exp1_implement,  
                                   `I feel strongly this SHOULD be implemented in Nepal.` = "5",
                                   `I think it is a good thing to implement but not a priority.` = "4",
                                   `I have no strong opinions about its implementation.`  = "3",
                                   `I think it probably shouldn’t be implemented, but I do not feel strongly about it.` = "2",
                                   `I feel strongly this should NOT be implemented in Nepal.` = "1")
# there are skips - changing to nas with numeric
data$exp1_implement_code <- as.numeric(data$exp1_implement_code)

###################
# coding exp1 data: useful (pre and post)
###################
# coding exp1 data: useful
data$exp1_useful_code <- recode(data$exp_1_useful,  
                                `Useful` = "5",
                                `Somewhat useful` = "4",
                                `Neither useful nor useless` = "3",
                                `Somewhat useless`  = "2",
                                `Useless` = "1")
# changing skips nas with numeric
data$exp1_useful_code <- as.numeric(data$exp1_useful_code)

###################
# coding exp1 data: implementation
###################
data$exp_1_interest_code <- recode(data$exp_1_interest,  
                                   `Ask me again at the end of the survey` = "1",
                                   `Yes` = "1",
                                   `No` = "0")
data$exp_1_interest_code_check <- recode(data$exp_1_interest,  
                                         `Ask me again at the end of the survey` = "",
                                         `Yes` = "1",
                                         `No` = "0")
# changing skips nas with numeric
data$exp_1_interest_code <- as.numeric(data$exp_1_interest_code)
data$exp_1_interest_code_check <- as.numeric(data$exp_1_interest_code_check)


################
# recoding other descriptive variables
################
data$eews_know_code <- recode(data$eews_know,  
                              `I know what it is, how they work, and have formed detailed opinions about them.` = "5",
                              `I know what it is and how they work.` = "4",
                              `I have heard of it and know a bit about them.` = "3",
                              `I have heard of it but do not know anything about it.`  = "2",
                              `I have never heard of it.` = "1")
data$anxiety_code <- recode(data$anxiety,  
                       `Highly agree (Very worried)` = 5,
                       `Slightly agree ` = 4,
                       `Neither agree nor disagree` = 3,
                       `Slightly disagree `  = 2,
                       `Highly disagree (Not at all worried)` = 1)
data$caste_ethnicity_code <- recode(data$caste_ethnicity,  
                               `Bahun` = 0,
                               `Chhetri` = 0,
                               `Newar (Newa)` = 1,
                               `Madhesh`  = 2,
                               `Magar` = 2,
                               `Gurung` = 2,
                               `Limbu` = 2,
                               `Rai` = 2,
                               `Tamang` = 2,
                               `Thakuri` = 2,
                               `Tharu` = 2,
                               `Yadav` = 2,
                               `Other` = 2)
data$married_code <- recode(data$married,
                            Divorced = 0,
                            Separated = 0,
                            Widowed = 0,
                            `Single (never married)` = 0,
                            Married = 1)
data$gender_code <- recode(data$gender,
                            Male = 0,
                            Female = 1,
                            Other = 1)
data$rural_urban_code <- recode(data$rural_urban,
                           City = 0,
                           Town = 1,
                           Village = 2)
data$education_code <- recode(data$education,
                              `No formal education` = 0,
                              `Pre-primary and/or primary` = 1,
                              `Secondary` = 2,
                              `University (Bachelor)` = 3,
                              `Post-graduate` = 4)
data$age_code <- recode(data$age,
                        `18-20` = 0,
                        `21-25` = 0,
                        `26-30` = 1,
                        `31-35` = 1,
                        `36-40` = 2,
                        `41-45` = 2,
                        `46-50` = 3,
                        `51-55` = 3,
                        `56-60` = 4,
                        `61-65` = 4,
                        `66-70` = 5,
                        `71-75` = 5,
                        `76` = 5)

data$opinion_eews_need_code <- recode(data$opinion_ewws_need,
                        `For all earthquakes, independent of whether the shaking could be felt or not.` = 5,
                        `For all earthquakes, I may feel.` = 4,
                        `For all earthquakes, I will certainly feel.` = 3,
                        `For all earthquakes that may cause damage.` = 2,
                        `For all earthquakes that will certainly cause damage.` = 1
                        )
data$small_behavior_code <- recode(data$small_behavior,
                                   `I would change my behavior depending on how big the earthquake was expected to be.` = 1,
                                   `My behavior would be the same for all earthquakes.` = 0)

# receive_warning, behavior

# changing skips nas with numeric
data$eews_know_code <- as.numeric(data$eews_know_code)
data$opinion_eews_need_code <- as.numeric(data$opinion_eews_need_code)
data$small_behavior_code <- as.numeric(data$small_behavior_code)

##################
# counting and then summing past experience
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

# now creating count variable
data$experience_count <- sapply(strsplit(data$past_experience, ","), length)

# and now sum variable with overall exposure score
item_scores <- c(
  "Personally felt an earthquake before" = 1, 
  "Personally have felt strong shaking" = 2, 
  "Personally have experienced injury" = 5,
  "Know family or friends" = 4, 
  "Suffered financial loss due to an earthquake" = 2, 
  "Struggled mentally following an earthquake" = 2,
  "Observed" = 2,
  "Have seen the effects of" = 1, 
  "I haven’t experienced any of the above" = 0
)
data$experience_score <- sapply(
  strsplit(data$past_experience, ","),
  function(items) sum(item_scores[items], na.rm = TRUE)
)

data %>% # proportions
  count(experience_score) %>% #
  mutate(prop = n / sum(n))

data %>% # proportions
  count(past_experience) %>% #
  mutate(prop = n / sum(n))      

data$experience_index <- data$experience_score / max(data$experience_score)
         
summary(as.factor(data$experience_count))
summary(as.factor(data$experience_score))
summary(data$experience_index)

##################
# counting and then summing display
################
split_responses <- strsplit(data$display, ",\\s*")
unique_categories <- unique(unlist(split_responses))
# [1] "Time available before shaking begins"   "Magnitude of earthquake"                "Expected intensity of ground shaking"  
# [4] "Epicenter (point of earthquake origin)" "Expected duration of shaking"           "Recommended actions"                   
# [7] "Other"                                  "Skip"                                   "I don’t know"  
# doing a simple count (every item counts as one) unlike experiences
items <- c(  "Time available before shaking begins",
             "Magnitude of earthquake", 
             "Expected intensity of ground shaking",
             "Epicenter (point of earthquake origin)", 
             "Expected duration of shaking", 
             "Recommended actions",
             "Other")
item_scores <- c(
  "Time available before shaking begins" = 1, 
  "Magnitude of earthquake" = 1, 
  "Expected intensity of ground shaking" = 1,
  "Epicenter (point of earthquake origin)" = 1, 
  "Expected duration of shaking" = 1, 
  "Recommended actions" = 1,
  "Other" = 1 # I am assigning other to 1 since it is distinct from dk or skip
)

# assign display_score with NA for all "Skip"/"I don't know" and valid scores for the rest
data$display_score <- sapply(
  strsplit(data$display, ","),
  function(items) {
    items <- trimws(items)
    
    # Replace "Skip" and "I don’t know" with NA
    items[items %in% c("Skip", "I don’t know")] <- NA
    
    # Get the scores
    scores <- item_scores[items]
    
    # Return NA if all are NA
    if (all(is.na(scores))) {
      return(NA)
    } else {
      return(sum(scores, na.rm = TRUE))
    }
  }
)
summary(as.factor(data$display_score))
table(unlist(split_responses))

##########

if (imputed_yes == 1) {
  write.csv(data, "replication/data_imputed_recoded.csv")
  message("Imputed dataset saved")
  } else {
  write.csv(data, "replication/data_recoded.csv")
  message("Original dataset saved")
  }

rm(list = ls())

