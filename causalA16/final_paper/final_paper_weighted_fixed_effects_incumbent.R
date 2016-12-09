library(causalA16)
library(data.table)
library(wfe)
library(lfe)
library(survey)

load("data/district_year.RData")
source("R/generate_fake_district_stats.R")

# subset data based on treatment
incumbent_district_year <- district_year[Treatment == "Inc", ]

individual_turnout_incumbent_treatment <-
  turnout_by_subdistrict(incumbent_district_year[year == 2006,], 0.05)

individual_turnout_incumbent_treatment <-
  rbind(
    data.table(state = individual_turnout_incumbent_treatment$state,
      pre_district = individual_turnout_incumbent_treatment$pre_district,
      post_district = individual_turnout_incumbent_treatment$post_district,
      year = 1, turnout = individual_turnout_incumbent_treatment$turnout_1),
    data.table(state = individual_turnout_incumbent_treatment$state,
      pre_district = individual_turnout_incumbent_treatment$pre_district,
      post_district = individual_turnout_incumbent_treatment$post_district,
      year = 2, turnout = individual_turnout_incumbent_treatment$turnout_2),
    data.table(state = individual_turnout_incumbent_treatment$state,
      pre_district = individual_turnout_incumbent_treatment$pre_district,
      post_district = individual_turnout_incumbent_treatment$post_district,
      year = 3, turnout = individual_turnout_incumbent_treatment$turnout_3),
    data.table(state = individual_turnout_incumbent_treatment$state,
      pre_district = individual_turnout_incumbent_treatment$pre_district,
      post_district = individual_turnout_incumbent_treatment$post_district,
      year = 4, turnout = individual_turnout_incumbent_treatment$turnout_4))

save(individual_turnout_incumbent_treatment,
  file = "data/individual_turnout_incumbent_treatment.RData")

# clear workspace and reload here to keep R happy

library(wfe)
library(causalA16)
library(survey)

load("data/individual_turnout_incumbent_treatment.RData")

