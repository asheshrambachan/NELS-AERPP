# ###################################################
# Author: Ashesh Rambachan
# Last Updated: 12/5/2017
# This script is a helper script for data_clean.R. 
# It cleans the FU3 academic covariates
# ###################################################

# Took the SAT indicator: Base case = legit skip, don't know, refusal, missing
SAT_yes = ifelse(NELS$PPOSTEX1 == 1, 1, 0);
SAT_no = ifelse(NELS$PPOSTEX1 == 2, 1, 0);
NELS <- cbind(NELS, SAT_yes, SAT_no);
rm(SAT_yes, SAT_no);

# Took the ACT indicator: Base case = legit skip, don't know, refusal, missing
ACT_yes = ifelse(NELS$PPOSTEX2 == 1, 1, 0);
ACT_no = ifelse(NELS$PPOSTEX2 == 2, 1, 0);
NELS <- cbind(NELS, ACT_yes, ACT_no);
rm(ACT_yes, ACT_no);

NELS <- subset(NELS, select = -c(PPOSTEX1, PPOSTEX2));

