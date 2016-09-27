library(causalA16)
library(Matching)

load("inst/extdata/Fraga2016_DistrictStats.Rdata")
Fraga_District_Stats <- setDT(x)

load("inst/extdata/Fraga2016_DistrictTreatmentResults.Rdata")
Fraga_District_Treatment <- setDT(x)

