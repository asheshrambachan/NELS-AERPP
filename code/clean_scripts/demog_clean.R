# ###################################################
# Author: Ashesh Rambachan
# Last Updated: 12/5/2017
# This script is a helper script for data_clean.R. 
# It cleans the demographic covariates.
# ###################################################

# Race: Base = white, non-hispanic
asian = ifelse(NELS$RACE == 1, 1, 0);
hispanic = ifelse(NELS$RACE == 2, 1, 0);
black = ifelse(NELS$RACE == 3, 1, 0);
nativeam = ifelse(NELS$RACE == 5, 1, 0);
m_race = ifelse(NELS$RACE == 8, 1, 0); # missing race indicator
NELS <- subset(NELS, select = -c(RACE));
NELS <- cbind(NELS, asian, hispanic, black, nativeam, m_race);
rm(asian, black, hispanic, hispanic, nativeam, m_race);

# Gender: Base = male
female = ifelse(NELS$SEX == 2, 1, 0);
NELS <- subset(NELS, select = -c(SEX));
NELS <- cbind(NELS, female);
rm(female);

# Birthdates
NELS <- subset(NELS, select = -c(BIRTHMO)); # drop birth month
NELS <- NELS[NELS$BIRTHYR != 98, ]; # drop if missing birth year

# Drop Parent education, family composition, family size
# parent marital status, family income
# Unlikely to influence admissions decision
NELS <- subset(NELS, select = -c(BYPARED, BYFCOMP, BYFAMSIZ, BYPARMAR, BYFAMINC, BYSES)); 

# Drop demographic details of middle school.
# College admissions unlikely to use this info.
NELS <- subset(NELS, select = -c(G8URBAN, G8REGON, G8CTRL1, G8MINOR, G8LUNCH, BYRATIO));

# School district info
# Type of district: Base case = suburban
urb_distt = ifelse(NELS$G10URBAN == 1, 1, 0);
rur_distt = ifelse(NELS$G10URBAN == 3, 1, 0);
m_distt = ifelse(NELS$G10URBAN == 8, 1, 0); # missing indicator
NELS <- cbind(NELS, urb_distt, rur_distt, m_distt);
rm(urb_distt, rur_distt, m_distt);

# District region: Base case = northeast
reg_dist2 = ifelse(NELS$G10REGON == 2, 1, 0);
reg_dist3 = ifelse(NELS$G10REGON == 3, 1, 0);
reg_dist4 = ifelse(NELS$G10REGON == 4, 1, 0);
reg_distm = ifelse(NELS$G10REGON == 98, 1, 0); # missing indicator
NELS <- cbind(NELS, reg_dist2, reg_dist3, reg_dist4, reg_distm);
rm(reg_dist2, reg_dist3, reg_dist4, reg_distm);

# District type: base case = public
dist_type2 = ifelse(NELS$G10CTRL1 == 2, 1, 0);
dist_type3 = ifelse(NELS$G10CTRL1 == 3, 1, 0);
dist_type4 = ifelse(NELS$G10CTRL1 == 4, 1, 0);
dist_type5 = ifelse(NELS$G10CTRL1 == 5, 1, 0);
dist_typem = ifelse(NELS$G10CTRL1 == 98, 1, 0); # missing indicator
NELS <- cbind(NELS, dist_type2, dist_type3, dist_type4, dist_type5, dist_typem);
rm(dist_type2, dist_type3, dist_type4, dist_type5, dist_typem);

# 10th grade size: base case = 700+
G10size1 = ifelse(NELS$G10ENROL == 1, 1, 0);
G10size2 = ifelse(NELS$G10ENROL == 2, 1, 0);
G10size3 = ifelse(NELS$G10ENROL == 3, 1, 0);
G10size4 = ifelse(NELS$G10ENROL == 4, 1, 0);
G10size5 = ifelse(NELS$G10ENROL == 5, 1, 0);
G10size6 = ifelse(NELS$G10ENROL == 6, 1, 0);
G10size7 = ifelse(NELS$G10ENROL == 7, 1, 0);
G10sizem = ifelse(NELS$G10ENROL == 98, 1, 0); # missing indicator
NELS <- cbind(NELS, G10size1, G10size2, G10size3, G10size4, G10size5, G10size6, G10size7, G10sizem);
rm(G10size1, G10size2, G10size3, G10size4, G10size5, G10size6, G10size7, G10sizem);

# School size: base case = 2500+
ssize1 = ifelse(NELS$F1SCENRL == 1, 1, 0);
ssize2 = ifelse(NELS$F1SCENRL == 2, 1, 0);
ssize3 = ifelse(NELS$F1SCENRL == 3, 1, 0);
ssize4 = ifelse(NELS$F1SCENRL == 4, 1, 0);
ssize5 = ifelse(NELS$F1SCENRL == 5, 1, 0);
ssize6 = ifelse(NELS$F1SCENRL == 6, 1, 0);
ssize7 = ifelse(NELS$F1SCENRL == 7, 1, 0);
ssize8 = ifelse(NELS$F1SCENRL == 8, 1, 0);
ssizem = ifelse(NELS$F1SCENRL == 98, 1, 0); # missing indicator
NELS <- cbind(NELS, ssize1, ssize2, ssize3, ssize4, ssize5, ssize6, ssize7, ssize8, ssizem);
rm(ssize1, ssize2, ssize3, ssize4, ssize5, ssize6, ssize7, ssize8, ssizem);

NELS <- subset(NELS, select = -c(G10URBAN, G10REGON, G10CTRL1, G10ENROL, F1SCENRL));