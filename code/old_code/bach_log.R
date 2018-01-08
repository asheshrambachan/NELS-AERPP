# ####################################################################
# Author: Ashesh Rambachan
# Last updated: December 9, 2017
# Description: This script estimates the random forest for to predict
# completion of a Bachelor's degree using the full set of covariates.
# I restrict the sample to only blacks and whites. The 
# data are split 80/20 into a training and test set. I estimate
# a logistic regression
# ####################################################################

library(randomForest)

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
constant = rep(1, nrow(covar))

blind_covar = subset(covar, select = -c(black, white, y))
race_covar = subset(covar, select = -c(y, white))
blind_covar = cbind(blind_covar, constant)
race_covar = cbind(race_covar, constant)

# Construct training and test sets
set.seed(2000)
train = sample.int(n = nrow(covar), size = floor(0.8*nrow(covar)), replace = F) # index of training set
trg_sz = length(train)
tst_sz = nrow(covar) - trg_sz

##################################
# Construct race-blind predictor #
##################################
blind_covar <- blind_covar[ , unique(names(blind_covar)), with = FALSE]
lr_blind = glm(y ~., data = blind_covar, subset = train, family = binomial(link='logit'))
blind_pred.trg = ifelse(lr_blind$fitted.values >= 0.5, 1, 0)

# error rate on training set
blind_err.trg = 1 - sum(blind_pred.trg == y[train])/trg_sz

##################################
# Construct race-aware predictor #
##################################
race_covar <- race_covar[ , unique(names(race_covar)), with = FALSE]
lr_aware = glm(y~., data = race_covar, subset = train, family = binomial(link = 'logit'))
race_pred.trg = ifelse(lr_aware$fitted.values >= 0.5, 1, 0)

# Error rate on training set
aware_err.trg = 1 - sum(race_pred.trg == y[train])/trg_sz

######################################
# Estimates out-of-sample error rate #
######################################
blind_pred.tst = ifelse(predict(lr_blind, blind_covar[-train], type = "response") >= 0.5, 1, 0)
aware_pred.tst = ifelse(predict(lr_aware, race_covar[-train], type = "response") >= 0.5, 1, 0)

blind_err.tst = 1 - (sum(blind_pred.tst == y[-train]))/tst_sz
aware_err.tst = 1 - (sum(aware_pred.tst == y[-train]))/tst_sz

