bootstrap_confint_glm <- function(formula, DATA, reps = 1000, family) {
  library(magrittr)
  n <- nrow(DATA)
  reps %>% replicate(DATA[sample(n, replace = TRUE)] %>% glm(formula, family, .)
    %>% coef) %>% quantile(c(.025, .975))
}

x <- rnorm(1000)
y <- rbinom(1000, 1, 0.5)
data_test <- data.table(cbind(y, x))

formula_test <- y ~ x

bootstrap_confint_glm(formula = formula_test, DATA = data_test, reps = 5,
  family = binomial)
