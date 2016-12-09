library(data.table)
library(AER)
library(lfe)
library(texreg)
library(wfe)
library(causalA16)
district_year <- district_year
district_year_minority <- district_year[district_year$Group != "White"]
district_year_white <- district_year[district_year$Group == "White"]
district_year_black <- district_year[district_year$Group == "Black"]

# # Matched Regression # #

ols_1 <- lm(turnout_matched ~ repub_redistrict_2, data = district_year_minority)
screenreg(ols_1)

ols_2 <- lm(turnout_matched ~ repub_redistrict_2 + pres_election,
  data = district_year_minority)
screenreg(ols_2)

ols_3 <- lm(turnout_matched ~ pres_election + south,
  data = district_year_minority)
screenreg(ols_3)

ols_4 <- lm(turnout_matched ~ repub_redistrict_2 + pres_election + south,
  data = district_year)
screenreg(ols_4_minority)

ols_5 <- lm(turnout_unmatched ~ repub_redistrict_2 + pres_election + south
  + coethnic_district,
  data = district_year_minority)
summary(ols_5)

ols_6 <- lm(turnout_matched ~ pres_election + south
  + coethnic_district * repub_redistrict_2,
  data = district_year_minority)
summary(ols_6)

ols_7 <- lm(turnout_unmatched ~ repub_redistrict_2 + pres_election
  + coethnic_district,
  data = district_year_minority)
summary(ols_7)

ols_8 <- lm(turnout_matched ~ pres_election
  + coethnic_district * repub_redistrict_2,
  data = district_year_minority)
summary(ols_8)

texreg(list(ols_1, ols_2, ols_3, ols_4, ols_5, ols_6))

texreg(list(ols_7, ols_8))


# # Fixed Effect Analysis # #

felm_1 <- felm(turnout_unmatched ~  coethnic_district + repub_redistrict_2 |
    State + year | 0 | State,
  data = district_year_minority)
screenreg(felm_1)

felm_2 <- felm(turnout_matched ~  coethnic_district + repub_redistrict_2 |
    State + year | 0 | State,
  data = district_year_minority)
screenreg(felm_2)

texreg(list(felm_1, felm_2))

felm_3 <- felm(turnout_matched ~  coethnic_district + repub_redistrict_2
  + pres_election | State | 0 | year,
  data = district_year_minority)
screenreg(felm_3)

# # Instrumental Variable Analysis # #

two_stage_0 <- ivreg(turnout_matched ~ repub_redistrict_2 | south,
  data = district_year_minority)
summary(two_stage_0)

two_stage_1 <- ivreg(turnout_matched ~ repub_redistrict_2 + coethnic_district
  | south + coethnic_district,
  data = district_year_minority)
summary(two_stage_1)

two_stage_2 <- ivreg(turnout_matched ~ repub_redistrict_2 + pres_election
  + coethnic_district | south + pres_election + coethnic_district,
  data = district_year_minority)
summary(two_stage_2)

two_stage_3 <- ivreg(turnout_matched ~ repub_redistrict_2 * coethnic_district
  + pres_election | south * coethnic_district + pres_election,
  data = district_year_minority)
summary(two_stage_3)

screenreg(list(two_stage_0, two_stage_1, two_stage_2, two_stage_3))
texreg(list(two_stage_0, two_stage_1, two_stage_2, two_stage_3))


two_stage_0w <- ivreg(turnout_matched ~ repub_redistrict_2 | south,
  data = district_year_white)
summary(two_stage_0w)

two_stage_1w <- ivreg(turnout_matched ~ repub_redistrict_2 + coethnic_district
  | south + coethnic_district,
  data = district_year_white)
summary(two_stage_1w)

two_stage_2w <- ivreg(turnout_matched ~ repub_redistrict_2 + pres_election
  + coethnic_district | south + pres_election + coethnic_district,
  data = district_year_white)
summary(two_stage_2w)

two_stage_3w <- ivreg(turnout_matched ~ repub_redistrict_2 * coethnic_district
  + pres_election | south * coethnic_district + pres_election,
  data = district_year_white)
summary(two_stage_3w)

screenreg(list(two_stage_0w, two_stage_1w, two_stage_2w, two_stage_3w))
texreg(list(two_stage_0w, two_stage_1w, two_stage_2w, two_stage_3w))
