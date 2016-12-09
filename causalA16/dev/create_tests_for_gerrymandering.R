library(causalA16)
source("dev/generate_fake_district_stats.R")
load("data/district_year_wide.RData")
alabama_1 <- district_year_wide[State == "AL" & PreDist == 1]
alabama_1_test <- turnout_by_subdistrict(district_year_data = alabama_1,
  irregular_voter_percent = 0.05)

# need to develop statistical tests for gerrymandering
redistricting_simulation <- function(DT, n_redistricted, reps) {
  turnout_1 <- DT$turnout_1
  redistrict_distribution_1 <- replicate(reps, mean(sample(turnout_1, size = n_redistricted)))
  confint_1 <- quantile(redistrict_distribution_1, probs = c(0.005, 0.995))
  cat("* election 1 *", "\n")
  cat("* conf. int. *", "\n")
  return(confint_1)
  # turnout_2 <- DT$turnout_2
  # redistrict_distribution_2 <- replicate(reps, mean(sample(turnout_2, size = n_redistricted)))
  # confint_2 <- quantile(redistrict_distribution_2, probs = c(0.025, 0.975))
  # cat("* election 2 *", "\n")
  # cat("* conf. int. *", "\n")
  # return(confint_2)
  # turnout_3 <- DT$turnout_3
  # redistrict_distribution_3 <- replicate(reps, mean(sample(turnout_3, size = n_redistricted)))
  # confint_3 <- quantile(redistrict_distribution_3, probs = c(0.025, 0.975))
  # cat("* election 3 *", "\n")
  # cat("* conf. int. *", "\n")
  # return(confint_3)
}

# testing this shows with 99% confidence that the people moved were a group who
# turned out more in non-presidential election years more than others in AL 1
# AL 7 is the only district represented by a Democrat
redistricting_simulation(alabama_1_test, n_redistricted = 948, reps = 10000)
mean(alabama_1_test$turnout_1[alabama_1_test$post_district == 7])
