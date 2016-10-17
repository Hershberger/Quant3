library(causalA16)

options(stringsAsFactors = FALSE)

fraga_district_stats <-
  fread("inst/extdata/Fraga2016_DistrictStats.csv")
fraga_district_treatment <-
  fread("inst/extdata/Fraga2016_DistrictTreatmentResults.csv")
setDT(fraga_district_stats)

# make basic senate election data
senate_candidate_DT <- data.table(State =
    c("AL", "AZ", "CA", "FL", "IL", "MS", "NY", "NC", "SC", "TX"),
  sen_election_06 = c(0, 1, 1, 1, 0, 1, 1, 0, 0, 1),
  sen_election_08 = c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1),
  sen_election_10 = c(1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
  sen_election_12 = c(0, 1, 1, 1, 0, 1, 1, 0, 0, 1),
  sen_cand_white06 = c(NA, 1, 1, 1, NA, 1, 1, NA, NA, 1),
  sen_cand_white08 = c(1, NA, NA, NA, 1, 1, NA, 1, 1, 1),
  sen_cand_white10 = c(1, 1, 1, 1, 1, NA, 1, 1, 1, NA),
  sen_cand_white12 = c(NA, 1, 1, 1, NA, 1, 1, NA, NA, 1),
  sen_cand_black06 = c(NA, 0, 0, 0, NA, 1, 0, NA, NA, 0),
  sen_cand_black08 = c(1, NA, NA, NA, 0, 1, NA, 0, 0, 0),
  sen_cand_black10 = c(0, 0, 0, 1, 0, NA, 0, 0, 1, NA),
  sen_cand_black12 = c(NA, 0, 0, 0, NA, 0, 0, NA, NA, 0),
  sen_cand_latino06 = c(NA, 0, 0, 0, NA, 0, 0, NA, NA, 0),
  sen_cand_latino08 = c(0, NA, NA, NA, 0, 0, NA, 0, 0, 1),
  sen_cand_latino10 = c(0, 0, 0, 1, 0, NA, 0, 0, 0, NA),
  sen_cand_latino12 = c(NA, 1, 0, 0, NA, 0, 0, NA, NA, 1),
  sen_cand_asian06 = c(NA, 0, 0, 0, NA, 0, 0, NA, NA, 0),
  sen_cand_asian08 = c(0, NA, NA, NA, 0, 0, NA, 0, 0, 0),
  sen_cand_asian10 = c(0, 0, 0, 0, 0, NA, 0, 0, 0, NA),
  sen_cand_asian12 = c(NA, 0, 0, 0, NA, 0, 0, NA, NA, 0)
)

# add senate data to replication data
senate_candidate_DT <- setDT(merge(fraga_district_stats, senate_candidate_DT,
  by = "State", all = TRUE))

# make dataset with separate observations for different years
senate_election_DT <- rbind(
  data.table(State = senate_candidate_DT$State,
    PreDist = senate_candidate_DT$PreDist, PostDist = senate_candidate_DT$PostDist,
    Group = senate_candidate_DT$Group, Turnout_U = senate_candidate_DT$Turnout06_U,
    Turnout_M = senate_candidate_DT$Turnout06_M,
    sen_cand_white = senate_candidate_DT$sen_cand_white06,
    sen_cand_black = senate_candidate_DT$sen_cand_black06,
    sen_cand_latino = senate_candidate_DT$sen_cand_latino06,
    sen_cand_asian = senate_candidate_DT$sen_cand_asian06, year = 2006),
  data.table(State = senate_candidate_DT$State,
    PreDist = senate_candidate_DT$PreDist, PostDist = senate_candidate_DT$PostDist,
    Group = senate_candidate_DT$Group, Turnout_U = senate_candidate_DT$Turnout08_U,
    Turnout_M = senate_candidate_DT$Turnout08_M,
    sen_cand_white = senate_candidate_DT$sen_cand_white08,
    sen_cand_black = senate_candidate_DT$sen_cand_black08,
    sen_cand_latino = senate_candidate_DT$sen_cand_latino08,
    sen_cand_asian = senate_candidate_DT$sen_cand_asian08, year = 2008),
  data.table(State = senate_candidate_DT$State,
    PreDist = senate_candidate_DT$PreDist, PostDist = senate_candidate_DT$PostDist,
    Group = senate_candidate_DT$Group, Turnout_U = senate_candidate_DT$Turnout10_U,
    Turnout_M = senate_candidate_DT$Turnout10_M,
    sen_cand_white = senate_candidate_DT$sen_cand_white10,
    sen_cand_black = senate_candidate_DT$sen_cand_black10,
    sen_cand_latino = senate_candidate_DT$sen_cand_latino10,
    sen_cand_asian = senate_candidate_DT$sen_cand_asian10, year = 2010),
  data.table(State = senate_candidate_DT$State,
    PreDist = senate_candidate_DT$PreDist, PostDist = senate_candidate_DT$PostDist,
    Group = senate_candidate_DT$Group, Turnout_U = senate_candidate_DT$Turnout12_U,
    Turnout_M = senate_candidate_DT$Turnout12_M,
    sen_cand_white = senate_candidate_DT$sen_cand_white12,
    sen_cand_black = senate_candidate_DT$sen_cand_black12,
    sen_cand_latino = senate_candidate_DT$sen_cand_latino12,
    sen_cand_asian = senate_candidate_DT$sen_cand_asian12, year = 2012)
)

# define treatment condition for shared race with Senate candidate
senate_election_DT$sen_treatment <- 0
senate_election_DT$sen_treatment[senate_election_DT$Group == "White"
  & senate_election_DT$sen_cand_white == 1] <- 1
senate_election_DT$sen_treatment[senate_election_DT$Group == "Black"
  & senate_election_DT$sen_cand_black == 1] <- 1
senate_election_DT$sen_treatment[senate_election_DT$Group == "Latino"
  & senate_election_DT$sen_cand_latino == 1] <- 1
senate_election_DT$sen_treatment[senate_election_DT$Group == "Asian"
  & senate_election_DT$sen_cand_asian == 1] <- 1

# conduct naive difference in means tests across different conditions
mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 1]) -
  mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 0])

mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 1]) -
  mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 0])

mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 1 &
    senate_election_DT$year == 2006 | senate_election_DT$year == 2010]) -
  mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$year == 2006 | senate_election_DT$year == 2010])

mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 1 &
    senate_election_DT$year == 2006 | senate_election_DT$year == 2010]) -
  mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$year == 2006 | senate_election_DT$year == 2010])

mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 1 &
    senate_election_DT$year == 2008 | senate_election_DT$year == 2012]) -
  mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$year == 2008 | senate_election_DT$year == 2012])

mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 1 &
    senate_election_DT$year == 2008 | senate_election_DT$year == 2012]) -
  mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$year == 2008 | senate_election_DT$year == 2012])

mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 1 &
      senate_election_DT$Group == "Black"]) -
  mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$Group == "Black"])

mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 1 &
    senate_election_DT$Group == "Black"]) -
  mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$Group == "Black"])

mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 1 &
      senate_election_DT$Group == "Latino"]) -
  mean(senate_election_DT$Turnout_U[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$Group == "Latino"])

mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 1 &
    senate_election_DT$Group == "Latino"]) -
  mean(senate_election_DT$Turnout_M[senate_election_DT$sen_treatment == 0 &
      senate_election_DT$Group == "Latino"])

# get counts of observations
summary(is.na(senate_election_DT$Turnout_U))
summary(is.na(senate_election_DT$Turnout_M))

table(senate_election_DT$year)
table(senate_election_DT$Group)
