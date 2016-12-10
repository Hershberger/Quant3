district_turnout_sample_1_year <- function(state, pre_district, post_district,
  n_registrants, turnout_probability, group, coethnic_incumbent, coethnic_candidate,
  coethnic_district, perc_fem, perc_55plus, perc_under30, perc_30to55,
  seed = sample.int(.Machine$integer.max, 1))
{
  library(data.table)
  set.seed(seed)
 DT <- data.table(registrant_id = c(paste0(pre_district, post_district,
     sprintf("%06.f",1:n_registrants))),
   pre_district = pre_district,
   post_district = post_district,
   state = state,
   group = group,
   coethnic_incumbent = coethnic_incumbent,
   coethnic_candidate = coethnic_candidate,
   coethnic_district = coethnic_district,
   perc_fem = perc_fem,
   perc_55plus = perc_55plus,
   perc_under30 = perc_under30,
   perc_30to55 = perc_30to55)
 turnout_count <- floor(n_registrants * turnout_probability)
 DT[, turnout_1 := 0]
 DT[sample(nrow(DT), turnout_count), turnout_1 := 1]
 out <- DT
 return(out)
}


# alabama_test <- district_turnout_sample_1_year(state = "AL", pre_district = 1,
#   post_district = 1, n_registrants = 81200, turnout_probability = 0.3772660)

# generate turnout assuming most of the same people turnout with some random error
# turnout_data takes output of distirct_year_sample_1_year()
district_turnout_next_election <- function(turnout_data, turnout_probability,
  irregular_voter_percent,
  seed = sample.int(.Machine$integer.max, 1))
{
  DF <- data.frame(turnout_data)
  last_election <- gsub("turnout_", "", names(DF)[ncol(DF)])
  this_election <- as.numeric(gsub("turnout_", "", names(DF)[ncol(DF)])) + 1
  DF$turnout <- DF[, c(length(DF))]
  turnout_percent <- sum(DF$turnout) / nrow(DF)
  turnout_change_percent <- turnout_probability - turnout_percent
  turnout_change <- floor(abs(turnout_change_percent) * nrow(DF))
  DT <- setDT(DF)
  previous_voters <- DT[turnout == 1]
  previous_nonvoters <- DT[turnout == 0]
  irregular_voters <- floor(nrow(DT) * (irregular_voter_percent)/2)
  if(irregular_voters + turnout_change > nrow(previous_nonvoters) &
      turnout_change - irregular_voters < -1 * (nrow(previous_voters))) {
    stop("more irregular voters than possible")
  } else if (turnout_change_percent > 0) {
    previous_nonvoters[sample(nrow(previous_nonvoters), turnout_change),
      turnout := 1]
    current_nonvoters <- previous_nonvoters[turnout == 0]
    current_voters <- previous_nonvoters[turnout == 1]
    current_nonvoters[sample(nrow(current_nonvoters), irregular_voters),
      turnout := 1]
    previous_voters[sample(nrow(previous_voters), irregular_voters),
      turnout := 0]
    DT <- rbind(current_nonvoters, current_voters, previous_voters)
  } else if (turnout_change_percent < 0) {
    previous_voters[sample(nrow(previous_voters), abs(turnout_change)),
      turnout := 0]
    current_voters <- previous_voters[turnout == 1]
    current_nonvoters <- previous_voters[turnout == 0]
    current_voters[sample(nrow(current_voters), irregular_voters),
      turnout := 0]
    previous_nonvoters[sample(nrow(previous_nonvoters), irregular_voters),
      turnout := 1]
    DT <- rbind(current_voters, current_nonvoters, previous_nonvoters)
  } else {
    previous_voters[sample(nrow(previous_voters), irregular_voters),
      turnout := 0]
    previous_nonvoters[sample(nrow(previous_nonvoters), irregular_voters),
      turnout := 1]
    DT <- rbind(previous_voters, previous_nonvoters)
  }
  setnames(DT, "turnout", paste0("turnout_", this_election))
  out <- DT
  return(out)
}

# alabama_test <- district_turnout_next_election(turnout_data = alabama_test,
#   turnout_probability = 0.7065271, irregular_voter_percent = 0.01)

# for getting all elections data for a single subdistrict
subdistrict_turnout_full <- function(state, pre_district, post_district,
  n_registrants, turnout_probability_vector, irregular_voter_percent,
  n_elections, coethnic_incumbent, coethnic_candidate, coethnic_district,
  group, perc_fem, perc_55plus, perc_under30, perc_30to55,
  seed = sample.int(.Machine$integer.max, 1)) {
  counter <- 1
  set.seed(seed)
  if (length(turnout_probability_vector) != n_elections) {
    stop("there must be turnout probabilities equal to the number of elections")
  }
  if (counter == 1) {
    cat("*** ", state, pre_district, "to", state, post_district, "\n")
    turnout_data <- district_turnout_sample_1_year(state = state,
      pre_district = pre_district, post_district = post_district,
      n_registrants = n_registrants, group = group,
      coethnic_candidate = coethnic_candidate, coethnic_district = coethnic_district,
      coethnic_incumbent = coethnic_incumbent,
      perc_fem = perc_fem,
      perc_55plus = perc_55plus,
      perc_under30 = perc_under30,
      perc_30to55 = perc_30to55,
      turnout_probability = turnout_probability_vector[1])
    counter <- counter + 1
  }
    while (counter >= 2 & counter <= n_elections) {
     turnout_data <- district_turnout_next_election(turnout_data = turnout_data,
        turnout_probability = turnout_probability_vector[counter],
        irregular_voter_percent = irregular_voter_percent,)
     counter <- counter + 1
  }
  out <- turnout_data
  return(out)
}

turnout_by_subdistrict <- function(district_year_data, irregular_voter_percent) {
  dist_df <- as.data.frame(district_year_data)
  counter <- 1
  dist_df <- as.data.frame(district_year_data)
  if (counter == 1) {
    dist_DT <- subdistrict_turnout_full(state = dist_df[1, 1],
      pre_district = dist_df[1, 2],
      post_district = dist_df[1, 3],
      n_registrants = dist_df[1, 6],
      turnout_probability_vector = c(dist_df[1, 7], dist_df[1, 8],
        dist_df[1, 9], dist_df[1, 10]),
      irregular_voter_percent = irregular_voter_percent,
      group = dist_df[1, 4],
      coethnic_incumbent = dist_df[1, 31],
      coethnic_candidate = dist_df[1, 32],
      coethnic_district = dist_df[1, 33],
      perc_fem = dist_df[1, 11],
      perc_55plus = dist_df[1, 14],
      perc_under30 = dist_df[1, 12],
      perc_30to55 = dist_df[1, 13],
      n_elections = 4)
    counter <- counter +1
  }
  while (counter > 1 & counter <= nrow(dist_df)){
    temp_DT <- subdistrict_turnout_full(state = dist_df[counter, 1],
      pre_district = dist_df[counter, 2],
      post_district = dist_df[counter, 3],
      n_registrants = dist_df[counter, 6],
      turnout_probability_vector = c(dist_df[counter, 7], dist_df[counter, 8],
        dist_df[counter, 9], dist_df[counter, 10]),
      irregular_voter_percent = irregular_voter_percent,
      group = dist_df[counter, 4],
      coethnic_incumbent = dist_df[counter, 31],
      coethnic_candidate = dist_df[counter, 32],
      coethnic_district = dist_df[counter, 33],
      perc_fem = dist_df[counter, 11],
      perc_55plus = dist_df[counter, 14],
      perc_under30 = dist_df[counter, 12],
      perc_30to55 = dist_df[counter, 13],
      n_elections = 4)
    dist_DT <- rbind(dist_DT, temp_DT)
    counter <- counter + 1
  }
  return(dist_DT)
}

make_ip_weights <- function(tr, vars, dat, stabilized = FALSE)
{
  if(stabilized == FALSE)
  {
    fmla <- as.formula(paste(tr, " ~ ", paste(vars, collapse= "+")))
    mod <- glm(formula = fmla, data = dat, family = "binomial")
    dat$prop_score <- predict(mod, type = "response")
    weights <- ifelse(dat[, c(tr)] == 1, 1/dat$prop_score,
      1/(1 - dat$prop_score))
    return(weights)
  }
  if(stabilized == TRUE)
  {
    prob1 <- (sum(dat[, c(treatment)] == 1)) / (nrow(dat))
    prob0 <- (sum(dat[, c(treatment)] == 0)) / (nrow(dat))
    fmla <- as.formula(paste(tr, " ~ ", paste(vars, collapse= "+")))
    mod <- glm(formula = fmla, data = dat, family = "binomial")
    dat$prop_score <- predict(mod, type = "response")
    weights <- ifelse(dat[, c(tr)] == 1, prob1/dat$prop_score,
      prob0/(1 - dat$prop_score))
    weights
  }
}
