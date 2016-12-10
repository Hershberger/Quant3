# if you want to rerun simulations then start here, otherwise go to line 23
library(causalA16)
library(data.table)
library(wfe)
library(lfe)

load("data/district_year.RData")
source("R/generate_fake_district_stats.R")

# subset data based on treatment
majority_district_year <- district_year[Treatment == "Maj", ]

individual_turnout_district_treatment <-
  turnout_by_subdistrict(majority_district_year[year == 2006,], 0.05)

individual_turnout_district_treatment[, c("coethnic_candidate", "coethnic_incumbent") := NULL]
setnames(individual_turnout_district_treatment, "coethnic_district", "treatment")

save(individual_turnout_district_treatment,
  file = "data/individual_turnout_district_treatment.RData")

# if you don't want to re-simulate data, start here
library(causalA16)
library(data.table)
library(wfe)
library(lfe)
load("data/individual_turnout_district_treatment.RData")

# make inverse propensity weights based on william/drew codesu
prob1 <- (sum(individual_turnout_district_treatment[, treatment == 1]) /
    nrow(individual_turnout_district_treatment))
prob0 <- (sum(individual_turnout_district_treatment[, treatment == 0]) /
    nrow(individual_turnout_district_treatment))
fmla <- treatment ~ turnout_1 + turnout_2 + turnout_3
mod <- glm(fmla, data = individual_turnout_district_treatment, family = "binomial")
individual_turnout_district_treatment$prop_score <- predict(mod, type = "response")
individual_turnout_district_treatment$weights <-
  ifelse(individual_turnout_district_treatment[, treatment == 1],
    prob1/individual_turnout_district_treatment$prop_score,
    prob0/individual_turnout_district_treatment$prop_score)

# group sepcific data
black_district_treatment <- individual_turnout_district_treatment[group == "Black"]
white_district_treatment <- individual_turnout_district_treatment[group == "White"]
asian_district_treatment <- individual_turnout_district_treatment[group == "Asian"]
latino_district_treatment <- individual_turnout_district_treatment[group == "Latino"]

# group specific weights
prob1_black <- (sum(black_district_treatment[, treatment == 1]) /
    nrow(black_district_treatment))
prob0_black <- (sum(black_district_treatment[, treatment == 0]) /
    nrow(black_district_treatment))
mod_black <- glm(fmla, data = black_district_treatment, family = "binomial")
black_district_treatment$group_prop_score <- predict(mod_black, type = "response")
black_district_treatment$group_weights <-
  ifelse(black_district_treatment[, treatment == 1],
    prob1_black/black_district_treatment$group_prop_score,
    prob0_black/black_district_treatment$group_prop_score)

prob1_white <- (sum(white_district_treatment[, treatment == 1]) /
    nrow(white_district_treatment))
prob0_white <- (sum(white_district_treatment[, treatment == 0]) /
    nrow(white_district_treatment))
mod_white <- glm(fmla, data = white_district_treatment, family = "binomial")
white_district_treatment$group_prop_score <- predict(mod_white, type = "response")
white_district_treatment$group_weights <-
  ifelse(white_district_treatment[, treatment == 1],
    prob1_white/white_district_treatment$group_prop_score,
    prob0_white/white_district_treatment$group_prop_score)

prob1_latino <- (sum(latino_district_treatment[, treatment == 1]) /
    nrow(latino_district_treatment))
prob0_latino <- (sum(latino_district_treatment[, treatment == 0]) /
    nrow(latino_district_treatment))
mod_latino <- glm(fmla, data = latino_district_treatment, family = "binomial")
latino_district_treatment$group_prop_score <- predict(mod_latino, type = "response")
latino_district_treatment$group_weights <-
  ifelse(latino_district_treatment[, treatment == 1],
    prob1_latino/latino_district_treatment$group_prop_score,
    prob0_latino/latino_district_treatment$group_prop_score)

prob1_asian <- (sum(asian_district_treatment[, treatment == 1]) /
    nrow(asian_district_treatment))
prob0_asian <- (sum(asian_district_treatment[, treatment == 0]) /
    nrow(asian_district_treatment))
mod_asian <- glm(fmla, data = asian_district_treatment, family = "binomial")
asian_district_treatment$group_prop_score <- predict(mod_asian, type = "response")
asian_district_treatment$group_weights <-
  ifelse(asian_district_treatment[, treatment == 1],
    prob1_asian/asian_district_treatment$group_prop_score,
    prob0_asian/asian_district_treatment$group_prop_score)

# weighted regression comparing group specific weights with weights given to all data
turnout_formula <- turnout_4 ~ treatment + perc_fem + perc_under30 + perc_55plus

texreg::texreg(list(
  glm(turnout_formula, weights = group_weights,
    family = "binomial", data = white_district_treatment),
  glm(turnout_formula, weights = group_weights,
    family = "binomial", data = black_district_treatment),
  # glm(turnout_formula, weights = group_weights,
  #   family = "binomial", data = asian_district_treatment),
  glm(turnout_formula, weights = group_weights,
    family = "binomial", data = latino_district_treatment)),
  reorder.coef = c(2:5, 1),
  caption = "Co-ethnic district Treatment, Weights Calculated by Group",
  label = "table:district1",
  digits = 4)

texreg::texreg(list(
  glm(turnout_formula, weights = weights,
    family = "binomial", data = white_district_treatment),
  glm(turnout_formula, weights = weights,
    family = "binomial", data = black_district_treatment),
  # glm(turnout_formula, weights = weights,
  #   family = "binomial", data = asian_district_treatment),
  glm(turnout_formula, weights = weights,
    family = "binomial", data = latino_district_treatment)),
  reorder.coef = c(2:5, 1),
  caption = "Co-ethnic district Treatment, Weights Not Calculated by Group",
  label = "table:district2",
  digits = 4)

# weighted state fixed effects
felm_1 <- felm(turnout_4 ~ treatment + perc_fem + perc_under30 + perc_55plus |
    state | 0 | state, data = white_district_treatment, weights = white_district_treatment$group_weights)
felm_2 <- felm(turnout_4 ~ treatment + perc_fem + perc_under30 + perc_55plus |
    state | 0 | state, data = black_district_treatment, weights = black_district_treatment$group_weights)
# felm_3 <- felm(turnout_4 ~ treatment + perc_fem + perc_under30 + perc_55plus |
#     state | 0 | state, data = asian_district_treatment, weights = asian_district_treatment$group_weights)
felm_4 <-felm(turnout_4 ~ treatment + perc_fem + perc_under30 + perc_55plus |
    state | 0 | state, data = latino_district_treatment, weights = latino_district_treatment$group_weights)

texreg::texreg(list(felm_1, felm_2, felm_4),
  digits = 4)
