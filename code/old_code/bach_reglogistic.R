# ####################################################################
# Author: Ashesh Rambachan
# Last updated: December 6, 2017
# Description: This script estimates the random forest for to predict
# completion of a Bachelor's degree using the full set of covariates.
# I restrict the sample to only blacks and whites. The 
# data are split 80/20 into a training and test set. I estimate
# a regularized logistic reg that does not have access to race and a
# a regularized logistic reg that does have access to race on the training
# sample. The test set is used to evaluate the predictive performance
# of each random forest.
# ####################################################################

library(glmnet)

# !ATTENTION!: Set working directory
setwd("~/Dropbox/Harvard/Algorithmic-Bias/NELS-AERPP");

# Loads the cleaned NELS dataset
# Make sure outcome variable in data_clean.R set to 1.
source("code/data_clean.R");

# Race variables
black = NELS$black;
white = ifelse(NELS$black == 0 & NELS$asian == 0 & NELS$hispanic == 0 & 
                 NELS$nativeam == 0 & NELS$m_race == 0, 1, 0);

# Outcome of interset
y = NELS$bach_rec

# Constructs the design matrix
source("code/covmtx_build.R")

# Restrict sample to only blacks and whites
covar = cbind(black, white, y, covar)
covar = covar[covar$black != 0 | covar$white != 0, ]
y = covar$y
y = factor(y)

blind_covar = subset(covar, select = -c(black, white, y))
race_covar = subset(covar, select = -c(y))

# Construct training and test sets
set.seed(2000)
train = sample.int(n = nrow(covar), size = floor(0.8*nrow(covar)), replace = F) # index of training set

###################################
# Construct Race-Blind Predictor #
##################################

blind_covar = data.matrix(blind_covar)
raceblind_log = cv.glmnet(blind_covar[train, ], y[train], family = "binomial", 
                       alpha = 0.6, standardize = TRUE, intercept = TRUE)
raceblind_lambda = raceblind_log$lambda.1se
raceblind_trg_pred = predict(raceblind_log, blind_covar[train, ], type = "response", 
                             s = raceblind_lambda)

###################################
# Construct Race-Aware Predictor #
##################################
race_covar = data.matrix(race_covar)
raceaware_log = cv.glmnet(race_covar[train, ], y[train], family = "binomial",
                       alpha = 0.6, standardize = TRUE, intercept = TRUE)
raceaware_lambda = raceaware_log$lambda.1se
raceaware_trg_pred = predict(raceaware_log, race_covar[train, ], type = "response",
                             s = raceaware_lambda)

########################
# DO NOT RUN THIS PART #
########################
#############################
# Out-of-sample Performance #
#############################
# raceblind_tst_pred = predict(raceblind_log, blind_covar[-train, ], type = "response")
# raceaware_tst_pred = predict(raceaware_log, race_covar[-train, ], type = "response")

