# ###################################################
# Author: Ashesh Rambachan
# Last Updated: 12/5/2017
# This script is a helper script for data_clean.R. 
# It cleans the base year academic covariates.
# ###################################################

# Drops BY grades variables
# Note: Correspond to middle-school grades, not available to college admission
NELS <- subset(NELS, select = -c(BYS81A, BYS81B, BYS81C, BYS81D, BYGRADS, 
                                 BY2XRSTD, BY2XMSTD, BY2XSSTD, BY2XHSTD));
