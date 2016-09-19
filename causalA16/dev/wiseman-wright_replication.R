library(causalA16)
library(foreign)
library(data.table)
library(texreg)

# load data
house_majority_roll_rate <- read.dta("data/house_majority_roll_rate.dta")
house_minority_roll_rate <- read.dta("data/house_minority_roll_rate.dta")


# define formulas for table 2 and 3
formula_1 <- rrate ~ dwdist
formula_2 <- rrate ~ dwdist + numfpvs
formula_3 <- rrate ~ dwdist + numfpvs + lagrrate
formula_4 <- rrate ~ dwdist + lagrrate + lag2rr


# run and view tables 2 and 3
texreg::screenreg(list(
  lm(formula_1, house_majority_roll_rate),
  lm(formula_2, house_majority_roll_rate),
  lm(formula_3, house_majority_roll_rate),
  lm(formula_4, house_majority_roll_rate)),
  digits = 4
)

texreg::screenreg(list(
  lm(formula_1, house_minority_roll_rate),
  lm(formula_2, house_minority_roll_rate),
  lm(formula_3, house_minority_roll_rate),
  lm(formula_4, house_minority_roll_rate)),
  digits = 4
)


# get LaTeX code for tables 2 and 3
texreg::texreg(list(
  lm(formula_1, house_majority_roll_rate),
  lm(formula_2, house_majority_roll_rate),
  lm(formula_3, house_majority_roll_rate),
  lm(formula_4, house_majority_roll_rate)),
  digits = 4
)

texreg::texreg(list(
  lm(formula_1, house_minority_roll_rate),
  lm(formula_2, house_minority_roll_rate),
  lm(formula_3, house_minority_roll_rate),
  lm(formula_4, house_minority_roll_rate)),
  digits = 4
)
