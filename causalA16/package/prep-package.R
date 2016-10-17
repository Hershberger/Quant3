# build package with functions only
devtools::document()
devtools::build()
devtools::install()

# load package with external data
options(stringsAsFactors = FALSE)
library(causalA16)
library(data.table)
fraga_district_stats <-
  fread("inst/extdata/Fraga2016_DistrictStats.csv")
fraga_district_treatment <-
  fread("inst/extdata/Fraga2016_DistrictTreatmentResults.csv")


# add redistricting stats
state_redistrict <- data.table(State = c(unique(fraga_district_stats$State)))

partisan_redistrict <- c("AL", "FL", "IL", "MS", "NC", "NY", "SC", "TX")
legis_republican_2010 <- c("AL", "AZ", "FL", "NC", "SC", "TX")
legis_republican_2012 <- c("AL", "AZ", "FL", "MS", "NC", "SC", "TX")

state_redistrict[, partisan_redistrict := 1 * (State %in% partisan_redistrict)]
state_redistrict[, legis_republican_2010 := 1 * (State %in% legis_republican_2010)]
state_redistrict[, legis_republican_2012 := 1 * (State %in% legis_republican_2012)]

# TODO: merge into other data
# TODO: merge in legislator party by district 2010 & 2012 from Volden & Wiseman
# TODO: consider structure our datasets should take
# TODO: create indicators for senator ethnicity
