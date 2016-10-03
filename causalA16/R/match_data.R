library(causalA16)
library(Matching)
library(data.table)

simulate_data_for_matching <- function(n,
  seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)
  DATA <- simulate_potential_outcomes(n)
  DATA[, prob_d_equals_1 := plogis(-2 + 4 * (y1 - y0))]
  DATA[, d := 1 * (runif(n) < prob_d_equals_1)]
  DATA[, y := d * y1 + (1 - d) * y0]
  DATA[, x := rnorm(n)]
  DATA[, `:=`(y1 = NULL, y0 = NULL, prob_d_equals_1 = NULL)]
  DATA[, z := rbinom(n = n, size = 1, prob = (runif(1, min = .1, max = .9)))]
  attr(DATA, "seed") <- seed
  DATA
}

simulate_falsely_matched_regression <- function(n) {
  seed = sample.int(.Machine$integer.max, 1)
  set.seed(seed)
  DATA <- simulate_potential_outcomes(n)
  DATA[, prob_d_equals_1 := plogis(-2 + 4 * (y1 - y0))]
  DATA[, d := 1 * (runif(n) < prob_d_equals_1)]
  DATA[, y := d * y1 + (1 - d) * y0]
  DATA[, x := rnorm(n)]
  DATA[, `:=`(y1 = NULL, y0 = NULL, prob_d_equals_1 = NULL)]
  DATA[, z := rbinom(n = n, size = 1, prob = (runif(1, min = .1, max = .9)))]
  attr(DATA, "seed") <- seed
  DATA
  set.seed(seed)
  simulate_data_for_matching(n = (rbinom(1, 2970, prob = runif(1)) + 30),
    seed = seed)
  glm_match <- glm(z ~ x, family = binomial, data = DATA)
  mismatch <- Match(Y = y, Tr = z, X = glm_match$fitted, estimand = "ATE")
  mean_diff <- calc_naive_difference_in_means(DATA)
  mismatch$est
  mean_diff
}

abcd<-function(){
  y<-rnorm(205, mean = 10, sd = 1)
  x<-runif(205, min = 2, max = 7)
  z<-rbinom(205, 1, .2)
  abc<-data.frame(x, y, z)
  glm111 <- glm(z ~ x, family = binomial, data = abc)
  rr1 <- Match(Y = y, Tr = z, X = glm111$fitted, estimand = "ATE")
  rr1$est
}
abcd()

replications <- replicate(1000, abcd())
replications
hist(unlist(replications))
