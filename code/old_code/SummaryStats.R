# ################################################################
# Author: Ashesh Rambachan
# Last updated: 12/6/2017
# Description: Computes unweighted and weighted
# summary statistics for the NELS subsample of interest.
# ################################################################

# !ATTENTION!: Set working directory
setwd("~/Dropbox/Harvard/Algorithmic-Bias/NELS-AERPP");

# Loads the cleaned NELS dataset
source("code/data_clean.R");

# Constructs summary statistics for unweighted NELS
NELS_overview = var_overview(NELS);
write.csv(NELS_overview, "documentation/NELS_unweighted_overview.csv", 
          row.names=FALSE);

# Constructs summary statistics for weighted NELS
# Note: I round the survey weights to the nearest integer and
# expand the dataset so the weights can easily be handled in a 
# ML algorithm. 
library(splitstackshape)
NELS.w <- NELS;
NELS.w$F4BYPNWT <- round(NELS.w$F4BYPNWT);
NELS.w <- expandRows(NELS.w, "F4BYPNWT");

tmp <- do.call(NELS.w, 
               list(mean = apply(NELS.w, 2, mean),
                    sd = apply(NELS.w, 2, sd),
                    median = apply(NELS.w, 2, median),
                    min = apply(NELS.w, 2, min),
                    max = apply(NELS.w, 2, max)))
NELSw_summ <- data.frame(t(tmp))
write.csv(NELSw_summ, "documentation/NELS_weighted_overview.csv", 
          row.names=FALSE);

