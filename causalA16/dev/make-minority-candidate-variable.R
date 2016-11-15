options(stringsAsFactors = FALSE)
library(causalA16)
library(data.table)
fraga_district_stats <-
  fread("inst/extdata/Fraga2016_DistrictStats.csv")
fraga_district_treatment <-
  fread("inst/extdata/Fraga2016_DistrictTreatmentResults.csv")

