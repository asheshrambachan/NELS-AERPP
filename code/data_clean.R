# ################################################################
# Author: Ashesh Rambachan
# Last updated: 12/5/2017
# Description: Loads in raw NELS data and runs data_preparation 
# script to subset NELS variables to variables of interest.
# The data are then cleaned using the scripts in clean_script
# directory and saved to NELS_BYF4_cleaned.rdata
# ################################################################

# !ATTENTION!: Set working directory
setwd("~/Dropbox/Harvard/Algorithmic-Bias/NELS-AERPP");

# Runs data_preparation.R script to return BY/F4 subset of NELS
source("code/clean_scripts/data_preparation.R");

# !ATTENTION!: Indicate outcome of interest
# 1 = At least bachelor's degree received
# 2 = At least mostly B's received
# 3 = At least A's and B's received
# 4 = At least C's and B's received
outcome = 1

# Removes larger dataset
rm(BYF4STU);
rm(keepvars);

# Rename BYF4STU_subset
NELS <- BYF4STU_subset;
rm(BYF4STU_subset);

# Restrict the observations to those that ever attended a 4-year institution
NELS <- NELS[NELS$F4ATT4YR == 1, ];

# Cleans demographic variables
source("code/clean_scripts/demog_clean.R");

# Cleans BY academic variables
source("code/clean_scripts/BYacad_clean.R");

# Cleans FU1 academic variables
source("code/clean_scripts/FU1acad_clean.R");

# Cleans FU2 academic variables
source("code/clean_scripts/FU2acad_clean.R");

# Cleans FU3 academic variables
source("code/clean_scripts/FU3acad_clean.R");

# Cleans FU1 activities variables
source("code/clean_scripts/FU1act_clean.R");

# Cleans FU2 activities variables
source("code/clean_scripts/FU2act_clean.R");

# cleans outcome variables
# Bachelors degree
NELS <- NELS[NELS$F4HHDG != -9, ] # drop if missing
NELS <- NELS[NELS$F4HHDG != -3, ] # Drop if legit skip
# undergrad grades
NELS <- NELS[NELS$F4EGRD != -3, ] # drop if legit skip
NELS <- NELS[NELS$F4EGRD != -2, ] # refused
NELS <- NELS[NELS$F4EGRD != -1, ] # don't know
NELS <- NELS[NELS$F4EGRD != -7, ] # not reached

# Selects outcome variables
if (outcome == 1) {
  # Receives at least Bachelors degree
  y = ifelse(NELS$F4HHDG >= 4, 1, 0); # Did they receive at least a Bachelor's degree or not?
  NELS <- cbind(NELS, y);
}
if (outcome == 2) { 
  # Receives at least mostly B's in undergrad
  y = ifelse(NELS$F4EGRD <=  3, 1, 0) # at least mostly B's in undergrad
  NELS <- cbind(NELS, y);
}
if (outcome == 3) {
  # Receives at least A's and B's in undergrad
  y = ifelse(NELS$F4EGRD <=  2, 1, 0) # at least A's and B's in undergrad
  NELS <- cbind(NELS, y);
}
if (outcome == 4) {
  # Receives at least B's and C's in undergrad
  y = ifelse(NELS$F4EGRD <=  4, 1, 0) # at least C's and B's in undergrad
  NELS <- cbind(NELS, y);
}
NELS <- subset(NELS, select = -c(F4HHDG, F4EGRD, F4ATTPSE, F4ATT4YR, F4TYPEDG));
rm(outcome)
rm(BYF4STU_subset_overview)

# Constructs the design matrix
source("code/covmtx_build.R")

# remove all school-level and district-level information
covar = subset(covar, select = -c(urb_dist, rur_dist, m_dist, 
                                  reg_dist2, reg_dist3, reg_dist4, reg_distm, 
                                  dist_type2, G10size1, G10size2, G10size3, G10size4,
                                  G10size5, G10size6, G10size7, G10sizem,
                                  ssize1, ssize2, ssize3, ssize4, ssize5, ssize6, ssize7,
                                  ssize8, ssizem))

# restrict sample to only blacks and whites
black = NELS$black
white = ifelse(NELS$black == 0 & NELS$asian == 0 & NELS$hispanic == 0 & 
                 NELS$nativeam == 0 & NELS$m_race == 0, 1, 0);

covar <- cbind(black, white, y, covar)
covar <- covar[covar$black != 0 | covar$white != 0, ]
rm(black, white)
y = covar$y
y = factor(y)







