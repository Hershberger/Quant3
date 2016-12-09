library(causalA16)
library(data.table)
library(wfe)
library(lfe)
library(survey)

load("data/district_year.RData")

# subset data based on treatment
incumbent_district_year <- district_year[Treatment == "Inc", ]
candidate_district_year <- district_year[Treatment == "Cand",]
majority_district_year <- district_year[Treatment == "Maj", ]

