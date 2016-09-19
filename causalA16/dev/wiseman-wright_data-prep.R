library(causalA16)
library(foreign)
library(data.table)

############################
# # # DATA PREP SCRIPT # # #
############################

# write Cox McCubbins data as .dta file
# house_roll_rate <- load("inst/extdata/rollrates45-105.RData")
# write.dta(x, "inst/extdata/house_roll_rate.dta")


# load .dta file of Cox McCubbins and make it a datatable to do shit
house_roll_rate <- read.dta("inst/extdata/house_roll_rate.dta")
setDT(house_roll_rate)


# create separate data tables for majority and minority rolls
# recode party to have Reps 1 and Dems 0 to match Wiseman Wright paper
house_majority_roll_rate <- subset(house_roll_rate, minority == 0)
house_majority_roll_rate$party <- abs(house_majority_roll_rate$party - 1)
write.dta(house_majority_roll_rate, "data/house_majority_roll_rate.dta")

house_minority_roll_rate <- subset(house_roll_rate, minority == 1)
house_minority_roll_rate$party <- abs(house_roll_rate$party - 1)
write.dta(house_minority_roll_rate, "data/house_minority_roll_rate.dta")


# starting to merge DW-NOMINATE and Cox McCubbins data
# not finished yet
house_nominate <- read.dta("inst/extdata/hl01113d21_BSSE_12.dta")
setDT(house_nominate)
house_year_data <- house_nominate[, list(party_median = median(dwnom1),
  party_median2 = median(dwnom2)), by = list(cong, party)]
house_median <- house_nominate[, list(floor_median = median(dwnom1),
  floor_median2 = median(dwnom2)), by = cong]
