#' Bootstrap Confidence Intervals
#' Simulates standard errors by sampling from data
#' @param formula linear model to be estimated
#' @param DATA dataset for estimation of linear model to be sampled from
#' @param reps repetitions of sampling from data
#' @import magrittr
#' @export
bootstrap_confint_lm <- function(formula, DATA, reps = 1000) {
  library(magrittr)
  set.seed(sample.int(.Machine$integer.max, 1))
  n <- nrow(DATA)
  reps %>% replicate(DATA[sample(n, replace = TRUE)] %>% lm(formula, .) %>% coef) %>%
    quantile(c(.025, .975))
}


bootstrap_confint_glm <- function(formula, DATA, reps = 1000, family, link) {
  library(magrittr)
  set.seed(sample.int(.Machine$integer.max, 1))
  n <- nrow(DATA)
  setDT(DATA)
  reps %>% replicate(DATA[n, replace = TRUE] %>% glm(formula,
    family(link)) %>% coef) %>% quantile(c(.025, .975))
}
