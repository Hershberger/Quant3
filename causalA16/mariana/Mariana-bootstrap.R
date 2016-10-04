bootstrap_confint_glm <- function(formula, DATA, reps = 1000, family) {
  library(magrittr)
  set.seed(sample.int(.Machine$integer.max, 1))
  n <- nrow(DATA)
  reps %>% replicate(DATA[sample(n, replace = TRUE)] %>% glm(formula, family, .)
    %>% coef) %>% quantile(c(.025, .975))
}

formula1 <- prio ~ aidshock11 #  + aidshock11pos + lPTSave_filled +
#   lassassinbanks + lriotsbanks + lstrikesbanks + ldemonstrationsbanks +
#   linfantmort + lnciv + lpartautocracy + lpartdemocracy + lfactionaldemoc +
#   lfulldemocracy + lln_rgdpc + lln_population + loil + linstab + ethfrac +
#   relfrac + ncontig + logmtn + ColdWar + spline1 + spline2 + spline3 + year

short_data <- newdata
setDT(short_data)
short_data[!is.na(prio) & !is.na(aidshock11), .(prio, aidshock11)]


bootstrap_confint_glm(formula = formula1,
  DATA = short_data,
  reps = 5, family = binomial)


# for rare events bootstrapping need to sample equal number of 0s and 1s
# TODO: figure out how to do that thing
bootstrap_confint_rare_events <- function(formula, DATA, reps = 1000, family) {
  library(magrittr)
  set.seed(sample.int(.Machine$integer.max, 1))
  n <- nrow(DATA)
  reps %>% replicate(DATA[sample(n, replace = TRUE)] %>% glm(formula, family, .)
                     %>% coef) %>% quantile(c(.025, .975))
}
