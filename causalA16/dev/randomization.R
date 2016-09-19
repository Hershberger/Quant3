library(causalA16)

set.seed(1)

D <- simulate_potential_outcomes(1000)

#' @param DATA A data.table containing variables y0 and y1
randomize <- function(DATA)
{
  treated <- sample.int(1000, 500)
  control <- (1:1000)[-treated]
  # control <- setdiff(1:1000, treated) # another way to do it
  DATA[treated, y := y1]
  DATA[control, y := y0]
  DATA[treated, mean(y)] - DATA[control, mean(y)]
}

permutation_distribution <- replicate(1000, randomize(D))
mean(permutation_distribution)
sd(permutation_distribution)
hist(permutation_distribution)

# needs documentation
simulate_paired_experiment <- function(n, prob_treatment = .5,
  seed = sample.int(.Machine$integer.max, 1))
{
  stopifnot(n %% 2 == 0)
  set.seed(seed)
  DATA <- simulate_potential_outcomes(n)
  treated <- sample.int(n, n / 2)
  control <- (1:n)[-treated]
  DATA[treated, d := 1]
  DATA[control, d := 0]
  DATA[, y := d * y1 + (1 - d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL)]
  attr(DATA, "seed") <- seed
  DATA
}

experiment <- simulate_paired_experiment(1000)
experiment[, y1 := y]
experiment[, y0 := y]

randomize(experiment)
