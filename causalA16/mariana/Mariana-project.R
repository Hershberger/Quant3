#####################################################
################# Final Project  ####################
#####################################################

library(foreign)
cwdata <- read.dta("/Users/mariana/Desktop/Latex/cwdata.dta")


#to create new variables a partir de las de _spline pero sin el guion bajo
library(plyr)
rename(cwdata, c("_spline1"="spline1", "_spline2"="spline2", "_spline3"="spline3"))
cwdata$spline1 <- cwdata$`_spline1`
cwdata$spline2 <- cwdata$`_spline2`
cwdata$spline3 <- cwdata$`_spline3`

# To create a subset just with variables used in the model and in descpriptive stats:
#
myvars <- c("prio", "aidshock11", "aidshock11pos", "lPTSave_filled", "lassassinbanks", "lriotsbanks",
            "lstrikesbanks", "ldemonstrationsbanks", "linfantmort", "lnciv", "lpartautocracy",
            "lpartdemocracy", "lfactionaldemoc", "lfulldemocracy", "lln_rgdpc", "lln_population",
            "loil", "linstab", "ethfrac", "relfrac", "ncontig", "logmtn", "ColdWar", "spline1",
            "spline2", "spline3", "year")
newdata<- cwdata[myvars]


library(Zelig)
library(texreg)
library(stargazer)
library(memisc)
mod1 <- zelig(prio ~ aidshock11 + aidshock11pos + lPTSave_filled + lassassinbanks + lriotsbanks +
                lstrikesbanks + ldemonstrationsbanks + linfantmort + lnciv + lpartautocracy +
                lpartdemocracy + lfactionaldemoc + lfulldemocracy + lln_rgdpc + lln_population +
                loil + linstab + ethfrac + relfrac + ncontig + logmtn + ColdWar + spline1 +
                spline2 + spline3 + year, data = newdata, model = "relogit")
texreg(mod1)
toLatex(mod1)
mod2 <- glm(prio ~ aidshock11 + aidshock11pos + lPTSave_filled + lassassinbanks + lriotsbanks +
              lstrikesbanks + ldemonstrationsbanks + linfantmort + lnciv + lpartautocracy +
              lpartdemocracy + lfactionaldemoc + lfulldemocracy + lln_rgdpc + lln_population +
              loil + linstab + ethfrac + relfrac + ncontig + logmtn + ColdWar + spline1 +
              spline2 + spline3 + year, data = newdata, family = binomial(link = "logit"))

library(stargazer)
stargazer(mod2, no.space = TRUE, single.row = TRUE)


# Matching:

library(Matching)
library(MASS)

datamatch <-na.omit(newdata[, c("aidshock11", "ethfrac", "prio")])

Y <- datamatch$prio
Tr <- datamatch$aidshock11

glm1 <- glm(Tr ~ ethfrac, family = binomial, data = datamatch)
summary(glm1)
stargazer(glm1)

rr1 <- Match(Y = Y, Tr = Tr, X = glm1$fitted, estimand = "ATE")
summary(rr1)

MatchBalance(Tr ~ ethfrac, match.out = rr1, nboots = 1000, data = datamatch)