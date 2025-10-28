# this file runs some simple regressions on the descriptive variables. Important to note that
# these are not causally identified and should not be interpreted as such. 
# These regressions thus describe the statistical
# relationship like a correlation. Regression allows us to be able to control for
# other variables and find more about the sensitivity of the estimate to different
# specifications.
# 
# The results presented in the appendix are the linear regressions without imputations.
# This file also includes the logit for models 2 and 3 (which follow the lm models)
# and the imputed data models. The imputed data show statistical significance where
# there was moderate significance with the nonimputed data

library(tidyverse)
library(ggpattern) # for data vis at the end
library(forcats) # for fct_reverse() in plots
library(patchwork) # for patchwork data vis (putting graphs together)
library(lmtest) # for hc3 standard errors
library(sandwich) # for hc3 standard errors
library(mice)
library(miceadds) # for model stats for imputed lms


################
# switch this if do not want to save the graphics
################
save <- 0 # 1 is save 0 is no

#################
# Reading in data for non imputed
################
data <- read.csv("replication/data_recoded.csv")[, -1] # nrow = 567

#################
# Doing some descriptive regressions
################
# first display_score
descriptive_model_display <- lm(display_score ~ 
             experience_score +
             gender_code +   
             rural_urban_code +
             eews_know_code +
             anxiety_code + 
             education_code +
             caste_ethnicity_code + 
             married_code +
             age_code, 
             data = data)
summary(descriptive_model_display)
length(descriptive_model_display$residuals)

descriptive_model_display_robust <- lmtest::coeftest(descriptive_model_display, 
                                           vcov = vcovHC(descriptive_model_display, type="HC3"), # HC3 is default
                                           data = data)
descriptive_model_display_robust

# second opinion need
descriptive_model_need <- lm(opinion_eews_need_code ~ 
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
summary(descriptive_model_need)
length(descriptive_model_need$residuals)

descriptive_model_need_robust <- lmtest::coeftest(descriptive_model_need, 
                                                     vcov = vcovHC(descriptive_model_need, type="HC3"), # HC3 is default
                                                     data = data)
descriptive_model_need_robust


# third if behavior changes with smaller earthquake
# `I would change my behavior depending on how big the earthquake was expected to be.` = 1,
# `My behavior would be the same for all earthquakes.` = 0
descriptive_model_small_behavior <- lm(small_behavior_code ~ 
                                         experience_score +
                                         gender_code +   
                                         rural_urban_code +
                                         eews_know_code +
                                         anxiety_code + 
                                         education_code +
                                         caste_ethnicity_code + 
                                         married_code +
                                         age_code, 
                                       data = data)
summary(descriptive_model_small_behavior)
length(descriptive_model_small_behavior$residuals)

descriptive_model_small_behavior_robust <- lmtest::coeftest(descriptive_model_small_behavior, 
                                                  vcov = vcovHC(descriptive_model_small_behavior, type="HC3"), # HC3 is default
                                                  data = data)
descriptive_model_small_behavior_robust


#################
# logits for second two
################
# second opinion need
model_need_polr_log <- MASS::polr(as.factor(opinion_eews_need_code) ~ 
                                         experience_score +
                                         gender_code +   
                                         rural_urban_code +
                                         eews_know_code +
                                         anxiety_code + 
                                         education_code +
                                         caste_ethnicity_code + 
                                         married_code +
                                         age_code, 
                                       data = data, 
                                       Hess = TRUE,
                                       method = c("logistic"))
summary(model_need_polr_log)
nrow(model_need_polr_log$fitted.values)
lmtest::coeftest(model_need_polr_log)
performance::r2(model_need_polr_log)

# third if behavior changes with smaller earthquake
model_small_behavior_polr_log <- glm(as.factor(small_behavior_code) ~ 
                                    experience_score +
                                    gender_code +   
                                    rural_urban_code +
                                    eews_know_code +
                                    anxiety_code + 
                                    education_code +
                                    caste_ethnicity_code + 
                                    married_code +
                                    age_code, 
                                    family = binomial(link = "logit"), data)
summary(model_small_behavior_polr_log)
lmtest::coeftest(model_small_behavior_polr_log)
performance::r2(model_small_behavior_polr_log)


#################
# Reading in data for imputed (only lms)
################
imputed_long <- read.csv("replication/data_imputed_recoded.csv")[, -1] # nrow = 3402
mids_imputed_long <- as.mids(imputed_long)
# much more efficient with more n - clearly losing some power in the above regressions

############
# analysis with imputed
#############
######
# display
descriptive_model_display_imputed <- with(mids_imputed_long, lm(display_score ~ 
                                              experience_score +
                                              gender_code +
                                              rural_urban_code +
                                              eews_know_code +
                                              anxiety_code +
                                              education_code +
                                              caste_ethnicity_code  +
                                              married_code +
                                              age_code))
descriptive_model_display_imputed

pool.fit <- mice::pool(descriptive_model_display_imputed)
options(scipen = 999)
summary(pool.fit)
pool.fit$glanced
pool.r.squared(pool.fit, adjusted = FALSE) 
pool.r.squared(pool.fit, adjusted = TRUE) 

# need earthquake threshold (size)
descriptive_model_need_imputed <- with(mids_imputed_long, lm(opinion_eews_need_code ~ 
                                                                  experience_score +
                                                                  gender_code +
                                                                  rural_urban_code +
                                                                  eews_know_code +
                                                                  anxiety_code +
                                                                  education_code +
                                                                  caste_ethnicity_code  +
                                                                  married_code +
                                                                  age_code))
descriptive_model_need_imputed

pool.fit <- pool(descriptive_model_need_imputed)
options(scipen = 999)
summary(pool.fit)
pool.fit$glanced
pool.r.squared(pool.fit, adjusted = FALSE) 
pool.r.squared(pool.fit, adjusted = TRUE) 


# change behavior
descriptive_model_behavior_imputed <- with(mids_imputed_long, lm(small_behavior_code ~ 
                                                                  experience_score +
                                                                  gender_code +
                                                                  rural_urban_code +
                                                                  eews_know_code +
                                                                  anxiety_code +
                                                                  education_code +
                                                                  caste_ethnicity_code  +
                                                                  married_code +
                                                                  age_code))
descriptive_model_behavior_imputed

pool.fit <- pool(descriptive_model_behavior_imputed)
options(scipen = 999)
summary(pool.fit)
pool.fit$glanced
pool.r.squared(pool.fit, adjusted = FALSE) 
pool.r.squared(pool.fit, adjusted = TRUE) 

###############################
# Basic correlations
###############################
cor.test(data$display_score, data$experience_score)
# this is quite bit in magnitude with only a correlation - 0.30
# however, as seen above, when put in controls, this lowers to 0.11, but still
# keeps its significance. It is thus sensitive and the magnitude shouldn't necessarily
# be trusted, but the robust significance suggests that this relationship might be
# worth looking into more
cor.test(data$rural_urban_code, data$small_behavior_code)

# table
vars <- c("opinion_eews_need_code", "experience_score", 
            "gender_code", "rural_urban_code",
            "eews_know_code", "anxiety_code",
            "education_code", "caste_ethnicity_code",
            "married_code", "age_code", "display_score", "small_behavior_code")
cmat <- stats::cor(data[vars], use = "pairwise.complete.obs", method = "pearson")
round(cmat, 2)

# build a matrix with upper triangle and stars
ct <- psych::corr.test(data[vars], use = "pairwise", method = "pearson", adjust = "none")
R  <- round(ct$r, 2)
P  <- ct$p

stars <- function(p) ifelse(p < .001, "***",
                            ifelse(p < .01,  "**",
                                   ifelse(p < .05,  "*", "")))

lab <- matrix(paste0(sprintf("%.2f", R), stars(P)), nrow = nrow(R), dimnames = dimnames(R))
lab[lower.tri(lab, diag = TRUE)] <- ""

lab %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  gt::gt() %>%
  gt::fmt_missing(columns = dplyr::everything(), missing_text = "") %>%
  gt::tab_header(title = "") # title if want to later

# data vis

#### function: upper-triangle correlation heat map (pearson, spearman, or polychoric)
corr_heatmap_upper <- function(df, vars, method = c("pearson","spearman","polychoric"),
                               labels = NULL, digits = 2) {
  method <- match.arg(method)
  
  #### select data and compute correlation based on method
  if (method %in% c("pearson","spearman")) {
    # numeric-only for pearson/spearman
    x <- df[, vars, drop = FALSE]
    stopifnot(all(vapply(x, is.numeric, logical(1))))
    R <- stats::cor(x, use = "pairwise.complete.obs", method = method)
    
    # pairwise p-values via cor.test
    vnames <- colnames(x)
    P <- matrix(NA_real_, nrow = length(vnames), ncol = length(vnames),
                dimnames = list(vnames, vnames))
    for (i in seq_along(vnames)) {
      for (j in seq_along(vnames)) {
        if (i != j) {
          keep <- stats::complete.cases(x[, i], x[, j])
          if (sum(keep) >= 3) {
            P[i, j] <- stats::cor.test(x[keep, i], x[keep, j],
                                       method = method, exact = FALSE)$p.value
          }
        }
      }
    }
    leg_name <- if (method == "pearson") "r (Pearson)" else "ρ (Spearman)"
  } else {
    # polychoric: coerce all to ordered factors and use hetcor
    # treats all as ordinal bc here the age is binned
    # if age weren't binned should probably keep continuious
    # also this is not fair to the actually categorical variables
    # but since we're not doing much with this other than showing where relationships lie
    # this will be fine for now
    x <- df[, vars, drop = FALSE]
    x <- data.frame(lapply(x, function(col) factor(col, ordered = TRUE)), check.names = FALSE)
    hc <- polycor::hetcor(x, ML = TRUE, std.err = TRUE, use = "pairwise.complete.obs")
    R  <- hc$correlations
    SE <- hc$std.errors
    Z  <- R / SE
    P  <- 2 * stats::pnorm(-abs(Z))
    leg_name <- "polychoric r"
  }
  
  #### build tidy upper-triangle only
  vnames <- colnames(R)
  idx <- seq_along(vnames)
  grid <- expand.grid(i = idx, j = idx)
  grid <- grid[grid$i <= grid$j, , drop = FALSE]  # upper triangle incl. diagonal
  grid$r <- R[cbind(grid$i, grid$j)]
  
  #### factor axes so the diagonal runs top-left to bottom-right
  # y reversed so upper triangle sits above the diagonal visually
  grid$xi <- factor(vnames[grid$j], levels = vnames) # columns (x)
  grid$yi <- factor(vnames[grid$i], levels = rev(vnames)) # rows (y reversed)
  
  #### optional custom labels: named character vector like c(orig1="label a", orig2="label b", ...)
  if (!is.null(labels)) {
    stopifnot(is.character(labels), !is.null(names(labels)))
    x_lab_map <- function(l) ifelse(l %in% names(labels), labels[l], l)
    y_lab_map <- x_lab_map
  } else {
    x_lab_map <- identity
    y_lab_map <- identity
  }
  
  #### text labels for cells (add stars/dagger on a new line for off-diagonals)
  stars_fun <- function(p) ifelse(is.na(p), "",
                                  ifelse(p < .001, "***",
                                         ifelse(p < .01,  "**",
                                                ifelse(p < .05,  "*", ""))))
  dagger_fun <- function(p) ifelse(!is.na(p) && p < .1 && p >= .05, "\u2020", "")
  p_vals <- P[cbind(grid$i, grid$j)]
  is_diag <- grid$i == grid$j
  
  # remove labels on the diagonal; keep stars+values only for off-diagonals
  lab_txt <- ifelse(
    is_diag, "",
    paste0(sprintf(paste0("%.", digits, "f"), grid$r), "\n",
           paste0(stars_fun(p_vals), dagger_fun(p_vals)))
  )
  
  # remove shading on the diagonal by setting fill to NA there
  grid$fill_r <- grid$r
  grid$fill_r[is_diag] <- NA_real_
  
  #### adaptive text size so values stay legible
  nvars <- length(vnames)
  txt_size <- if (nvars <= 8) 4.8 else if (nvars <= 12) 4.0 else if (nvars <= 16) 3.4 else 3.0
  
  #### plot (grayscale)
  p <- ggplot2::ggplot(grid, ggplot2::aes(x = xi, y = yi, fill = fill_r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = lab_txt), lineheight = 0.9,
                       size = txt_size, family = "Times New Roman") +
    ggplot2::scale_fill_gradient2(low = "gray90", mid = "white", high = "gray10",
                                  limits = c(-1, 1), midpoint = 0,
                                  name = leg_name,
                                  na.value = NA) +
    ggplot2::scale_x_discrete(labels = x_lab_map) +
    ggplot2::scale_y_discrete(labels = function(l) y_lab_map(l)) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_family = "Times New Roman") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 14),
      plot.margin  = ggplot2::margin(1, 1, 1, 1)
    )
  
  return(p)
}

custom_labels <- c(opinion_eews_need_code = "EEWS need", 
                   experience_score = "Experience", 
                   gender_code = "Gender",
                   rural_urban_code = "Rural/urban", 
                   eews_know_code = "EEWS know",
                   anxiety_code = "Anxiety",
                   education_code = "Education",
                   caste_ethnicity_code = "Caste/ethnicity",
                   married_code = "Married",
                   age_code = "Age",
                   display_score = "Display",
                   small_behavior_code = "Behavior")

# Pearson 
p1 <- corr_heatmap_upper(data, vars, method = "pearson", labels = custom_labels, digits = 2)

# Spearman 
p2 <- corr_heatmap_upper(data, vars, method = "spearman", labels = custom_labels, digits = 2)

# mixed 
# this one takes a while
p3 <- corr_heatmap_upper(data, vars, method = "polychoric", labels = custom_labels, digits = 2)

p1
p2
p3

if (save == 1) {
  ggsave("plot_corr_mixed.jpeg", p3, width = 8, height = 12,
         dpi = 300) 
  message("Plot saved")
} else {
  message("Plot not saved (save != 1).")
}

###############################
# checking out outcomes of earlier regressions
###############################
graphics::barplot(table(data$display_score)) 
graphics::barplot(table(data$opinion_eews_need_code))
graphics::barplot(table(data$small_behavior_code))
