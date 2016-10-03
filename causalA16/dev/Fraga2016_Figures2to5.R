#########################################################################################
## Replication Code for "Redistricting and the Causal Impact of Race on Voter Turnout" ##
## by Bernard L. Fraga ##
## as published in the Journal of Politics ##
#############################################

########################################################
## Replication Code File 2 - Data used in Figures 2-5 ##
## Tested with R v3.2.0, Platform: x86_64-apple-darwin13.4.0 (64-bit) ##
## Last Updated: Aug 11, 2015 ##
################################

###################################################################
## Load "DistrictTreatmentResults.csv," New1+New2+Old District "Triple" level dataset ##

# had to add in inst/extdata/
fraga2 <- read.csv("inst/extdata/Fraga2016_DistrictTreatmentResults.csv", stringsAsFactors=FALSE)
dim(fraga2) # Should be 1229 Rows by 12 Columns

#######################################################################################
## FIGURE 2 to 5 DATA FOR PLOTTING (Overall Average Treatment Effect on the Treated) ##

# Set treatments and groups of interest
# NOTE: Conditional is intersection of Cand and Maj treatment/control designation. See article for details.
treatments <- c("Inc","Cand", "Maj","Conditional")
groups <- c("White","Black","Latino","Asian")

# Dataframe to hold results
frame1 <- as.data.frame(1:(length(groups)*length(treatments)))
colnames(frame1) <- "Group"
frame1$Group <- NA
frame1$Treatment <- NA
frame1$Trials <- NA # Trials ("Triples") used to calculate Overall ATT
frame1$OverallATT_U <- NA # "Baseline" (Unmatched) Data Overall ATT
frame1$OverallATT_CIlo_U <- NA # "Baseline" (Unmatched) Data 95% CI, Lower Bound
frame1$OverallATT_CIhi_U <- NA # "Baseline" (Unmatched) Data 95% CI, Upper Bound
frame1$OverallATT_M <- NA # Matched Data Overall ATT
frame1$OverallATT_CIlo_M <- NA # Matched Data 95% CI, Lower Bound
frame1$OverallATT_CIhi_M <- NA # Matched Data 95% CI, Lower Bound

# Helper function for producing inverse-variance weighted standard errors
weightedse <- function(x){
	se <- sqrt(1/(sum(x)))
	return(se)
}

# Loop to combine results
for(j in 1:length(treatments)){
	treatment <- treatments[j]
	if(treatment == "Inc"){
		data <- subset(fraga2, Treatment == "Inc")
	}
	if(treatment == "Cand"){
		data <- subset(fraga2, Treatment == "Cand")
	}
	if(treatment == "Maj"){
		data <- subset(fraga2, Treatment == "Maj")
	}
	if(treatment == "Conditional"){
		data <- subset(fraga2, Treatment == "Maj" & ConditionalTreat == 1)
	}
	for(i in 1:length(groups)){
	data_grp <- subset(data, Group == groups[i])
	frame1$Group[min(which(is.na(frame1$Group)))] <- groups[i]
	frame1$Treatment[min(which(is.na(frame1$Treatment)))] <- treatment
	frame1$Trials[min(which(is.na(frame1$Trials)))] <- nrow(data_grp)
	# Unmatched
		data_grp$InvVar_U <- 1/((data_grp$TurnDiffSE_U)^2)
		oatt <- weighted.mean(data_grp$TurnDiff_U, w=data_grp$InvVar_U)
		oatt.lo <- oatt-(weightedse(data_grp$InvVar_U)*1.96)
		oatt.hi <- oatt+(weightedse(data_grp$InvVar_U)*1.96)
			if(is.na(oatt)){oatt <- 0}
			if(is.na(oatt.lo)){oatt.lo <- 0}
			if(is.na(oatt.hi)){oatt.hi <- 0}
		frame1$OverallATT_U[min(which(is.na(frame1$OverallATT_U)))] <- oatt
		frame1$OverallATT_CIlo_U[min(which(is.na(frame1$OverallATT_CIlo_U)))] <- oatt.lo
		frame1$OverallATT_CIhi_U[min(which(is.na(frame1$OverallATT_CIhi_U)))] <- oatt.hi
	# Matched
		data_grp$InvVar_M <- 1/((data_grp$TurnDiffSE_M)^2)
		oatt <- weighted.mean(data_grp$TurnDiff_M, w=data_grp$InvVar_M)
		oatt.lo <- oatt-(weightedse(data_grp$InvVar_M)*1.96)
		oatt.hi <- oatt+(weightedse(data_grp$InvVar_M)*1.96)
			if(is.na(oatt)){oatt <- 0}
			if(is.na(oatt.lo)){oatt.lo <- 0}
			if(is.na(oatt.hi)){oatt.hi <- 0}
		frame1$OverallATT_M[min(which(is.na(frame1$OverallATT_M)))] <- oatt
		frame1$OverallATT_CIlo_M[min(which(is.na(frame1$OverallATT_CIlo_M)))] <- oatt.lo
		frame1$OverallATT_CIhi_M[min(which(is.na(frame1$OverallATT_CIhi_M)))] <- oatt.hi
	}
}
frame1[,4:9] <- frame1[,4:9]*100 # Scales to Percentage Points

########
frame1 # Displays results, by Group and Treatment
########

# need to manually make the charts and such
