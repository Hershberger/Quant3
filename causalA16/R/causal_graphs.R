make_data_for_figure_4_10 <- function(n,
  seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)

  u <- rbinom(n, 1, prob = .5)
  s <- rbinom(n, 1, prob = plogis(-1 + 2 * u))
  x <- rbinom(n, 1, prob = plogis(1 - 2 * u))
  d <- rbinom(n, 1, prob = plogis(-3 + 6 * s))
  y1 <- rbinom(n, 1, prob = plogis(-1 + 2 * 1 - 2 * x))
  y0 <- rbinom(n, 1, prob = plogis(-1 + 2 * 0 - 2 * x))

  y <- d * y1 + (1 - d) * y0
  DATA <- data.table(y1, y0, y, u, s, x, d)
  attr(DATA, "seed") <- seed
  DATA
}

calc_average_causal_effect_estimand <- function(DATA)
{
  DATA[, mean(y1 - y0)]
}

calc_conditional_difference_in_means <- function(DATA, conditioning_variable)
{
  Z <- DATA[, conditioning_variable, with = FALSE]
  p_Z_1 <- DATA[, mean(Z == 1)]
  p_Z_0 <- DATA[, mean(Z == 0)]
  mean_y_D_1_Z_1 <- DATA[, mean(y[d == 1 & Z == 1])]
  mean_y_D_1_Z_0 <- DATA[, mean(y[d == 1 & Z == 0])]
  mean_y_D_0_Z_1 <- DATA[, mean(y[d == 0 & Z == 1])]
  mean_y_D_0_Z_0 <- DATA[, mean(y[d == 0 & Z == 0])]
  (p_Z_1 * mean_y_D_1_Z_1 + p_Z_0 * mean_y_D_1_Z_0) -
    (p_Z_1 * mean_y_D_0_Z_1 + p_Z_0 * mean_y_D_0_Z_0)
}
