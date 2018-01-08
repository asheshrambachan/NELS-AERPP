# ################################################################
# Author: Ashesh Rambachan
# Last updated: 1/1/2018
# Description: Computes summary statistics of outcomes
# for the human decisions. Human decisions are simply given by 
# the summary statistics computed over the test set.
# ################################################################

# Loads the cleaned NELS dataset
source("code/data_clean.R");

# Construct training and test sets
set.seed(2000)
train = sample.int(n = nrow(covar), size = floor(0.8*nrow(covar)), replace = F) # index of training set
trg_sz = length(train)

# Computes the summary statistics of the outcome var.
# over the test set
mean(covar$y[-train])
mean(covar$black[-train])
