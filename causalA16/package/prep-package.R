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
setDT(fraga_district_stats)

# add redistricting stats
state_redistrict <- data.table(State = c(unique(fraga_district_stats$State)))

partisan_redistrict <- c("AL", "FL", "IL", "MS", "NC", "NY", "SC", "TX")
legis_republican_2010 <- c("AL", "AZ", "FL", "NC", "SC", "TX")
legis_republican_2012 <- c("AL", "AZ", "FL", "MS", "NC", "SC", "TX")
south <- c("AL", "FL", "MS", "NC", "SC", "TX")

state_redistrict[, partisan_redistrict := 1 * (State %in% partisan_redistrict)]
state_redistrict[, legis_republican_2010 := 1 * (State %in% legis_republican_2010)]
state_redistrict[, legis_republican_2012 := 1 * (State %in% legis_republican_2012)]
state_redistrict[, south := 1 * (State %in% south)]

# create tall data.table with all variables for analysis

district_year06 <- district_year08 <- district_year10 <- district_year12 <-
  fraga_district_stats
district_year06$year <- 2006
district_year08$year <- 2008
district_year10$year <- 2010
district_year12$year <- 2012

district_year <- rbind(district_year06, district_year08, district_year10,
  district_year12)

district_year$turnout_unmatched <- NA
district_year$turnout_matched <- NA

district_year$turnout_unmatched[district_year$year == 2006] <-
  district_year$Turnout06_U
district_year$turnout_matched[district_year$year == 2006] <-
  district_year$Turnout06_M
district_year$turnout_unmatched[district_year$year == 2008] <-
  district_year$Turnout08_U
district_year$turnout_matched[district_year$year == 2008] <-
  district_year$Turnout08_M
district_year$turnout_unmatched[district_year$year == 2010] <-
  district_year$Turnout10_U
district_year$turnout_matched[district_year$year == 2010] <-
  district_year$Turnout10_M
district_year$turnout_unmatched[district_year$year == 2012] <-
  district_year$Turnout12_U
district_year$turnout_matched[district_year$year == 2012] <-
  district_year$Turnout12_M

# add partisan redistricting variable
district_year <- merge(district_year, state_redistrict, by = "State")

# getting treatment variables
district_year$coethnic_incumbent <- 0
district_year$coethnic_incumbent[district_year$Treatment == "Cand" |
  district_year$Treatment == "Maj"] <- NA
district_year$coethnic_incumbent[district_year$Treatment == "Inc" &
  district_year$PostDist == district_year$TreatDist] <- 1

district_year$coethnic_candidate <- 0
district_year$coethnic_candidate[district_year$Treatment == "Inc" |
    district_year$Treatment == "Maj"] <- NA
district_year$coethnic_candidate[district_year$Treatment == "Cand" &
    district_year$PostDist == district_year$TreatDist] <- 1

district_year$coethnic_district <- 0
district_year$coethnic_district[district_year$Treatment == "Inc" |
    district_year$Treatment == "Cand"] <- NA
district_year$coethnic_district[district_year$Treatment == "Maj" &
    district_year$PostDist == district_year$TreatDist] <- 1

# district_year$partisan_redistrict[district_year$year != 2012] <- 0
# district_year$legis_republican[district_year$year != 2012] <- 0
district_year[, repub_redistrict := partisan_redistrict * legis_republican_2010]

district_year[, c("TreatDist", "ControlDist") := NULL]

district_year$partisan_redistrict_2 <- 0
district_year$partisan_redistrict_2[district_year$year == 2012 &
    district_year$partisan_redistrict == 1] <- 1
district_year$legis_republican_2010_2 <- 0
district_year$legis_republican_2010_2[district_year$year == 2012 &
    district_year$legis_republican_2010 == 1] <- 1
district_year$repub_redistrict_2 <- 0
district_year$repub_redistrict_2[district_year$year == 2012 &
    district_year$repub_redistrict == 1] <- 1

district_year$pres_election <- 0
district_year$pres_election[district_year$year == 2008 |
  district_year$year == 2012] <- 1

save(district_year, file = "data/district_year.RData")

rm(list = ls())

load("data/district_year.RData")

# build package with functions and data
devtools::document()
devtools::build()
devtools::install()
