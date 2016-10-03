library(causalA16)
library(Matching)
library(data.table)

Fraga_District_Stats <-
  setDT(read.csv("inst/extdata/Fraga2016_DistrictStats.csv"))

Fraga_District_Treatment <-
  setDT(read.csv("inst/extdata/Fraga2016_DistrictTreatmentResults.csv"))

Fraga_District_Stats[,
  Treat := as.numeric(X.TreatDist.) - as.numeric(X.ControlDist.)]
Fraga_District_Stats$Treat[Fraga_District_Stats$Treat > 0 |
  Fraga_District_Stats$Treat < 0] <- 1

match_glm <- glm(Treat ~ )

prop_score <- predict(match_glm, Fraga_District_Treatment)

?Matching::Match

summary(match_glm)

