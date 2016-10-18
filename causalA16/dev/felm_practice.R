library(causalA16)
library(lfe)
library(data.table)

fake_felm_DT <- CJ(i = 1:4, t = 1:3)
fake_felm_DT$x <- rbinom(12, 1, .5)
fake_felm_DT$x2 <- rbinom(12, 1, .7)
fake_felm_DT$y <- fake_felm_DT$x2 * rbinom(12, 1, .6)

summary(felm(y ~ x + x2 | i | 0 | i, data = fake_felm_DT))
