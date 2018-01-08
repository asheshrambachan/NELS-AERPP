# ####################################################################
# Author: Ashesh Rambachan
# Last updated: December 6, 2017
# Description: This computes a simple linear predictor of Bachelor's 
# degree receipt on average of z-scores of standardized test scores in
# math, reading, science and history in F1 and F2.
# It compares the linear predictor for Blacks and Whites pooled together, 
# blacks alone and whites alone
# ####################################################################

library(dplyr)

# !ATTENTION!: Set working directory
setwd("~/Dropbox/Harvard/Algorithmic-Bias/NELS-AERPP");

# Loads the cleaned NELS dataset
# Make sure outcome variable in data_clean.R set to 1.
source("code/data_clean.R");

# Drop observations with missing/not-completed standardized test scores
NELS <- NELS[NELS$F12XRSTD > 0, ];
NELS <- NELS[NELS$F12XMSTD > 0, ];
NELS <- NELS[NELS$F12XSSTD > 0, ];
NELS <- NELS[NELS$F12XHSTD > 0, ];

NELS <- NELS[NELS$F22XRSTD > 0, ];
NELS <- NELS[NELS$F22XMSTD > 0, ];
NELS <- NELS[NELS$F22XSSTD > 0, ];
NELS <- NELS[NELS$F22XHSTD > 0, ];

# Computes the z-score of F1 standardized test scores
F1rstd = (NELS$F12XRSTD - mean(NELS$F12XRSTD))/sd(NELS$F12XRSTD);
F1mstd = (NELS$F12XMSTD - mean(NELS$F12XMSTD))/sd(NELS$F12XMSTD);
F1sstd = (NELS$F12XSSTD - mean(NELS$F12XSSTD))/sd(NELS$F12XSSTD);
F1hstd = (NELS$F12XHSTD - mean(NELS$F12XHSTD))/sd(NELS$F12XHSTD);
F1comp = (F1rstd + F1mstd + F1sstd + F1hstd)/4;

rm(F1rstd, F1mstd, F1sstd, F1hstd);

# Computes the z-score of F1 standardized test scores
F2rstd <- (NELS$F22XRSTD - mean(NELS$F22XRSTD))/sd(NELS$F22XRSTD);
F2mstd <- (NELS$F22XMSTD - mean(NELS$F22XMSTD))/sd(NELS$F22XMSTD);
F2sstd <- (NELS$F22XSSTD - mean(NELS$F22XSSTD))/sd(NELS$F22XSSTD);
F2hstd <- (NELS$F22XHSTD - mean(NELS$F22XHSTD))/sd(NELS$F22XHSTD);
F2comp = (F2rstd + F2mstd + F2sstd + F2hstd)/4;

rm(F2rstd, F2mstd, F2sstd, F2hstd);

# Computes composite standardized test score
compstd <- (F1comp + F2comp)/2;

# race indicators
black = NELS$black;
white = ifelse(NELS$black == 0 & NELS$asian == 0 & NELS$hispanic == 0 & 
               NELS$nativeam == 0 & NELS$m_race == 0, 1, 0);

# Constructs dataset 
tmp_data = data.frame(NELS$bach_rec, compstd, black, white)

# Race blind predictor
raceblind_lm = lm(NELS.bach_rec ~ 1 + compstd, data = tmp_data, subset = (black == 1 | white == 1))
blind_pred = raceblind_lm$coefficients[1] + raceblind_lm$coefficients[2]*tmp_data$compstd

# Constructs calibration curve for race-blind predictor
blind_data = data.frame(NELS$bach_rec, blind_pred, black, white)
blind_data$pctile = ntile(blind_data$blind_pred, 10) # constructs quintiles of fitted values

blind_bin_avg = aggregate(blind_data[, 1:2], list(blind_data$pctile), mean) # constructs averages over quinitles
black_bin_avg = aggregate(blind_data[black == 1, 1:2], 
                          list(blind_data$pctile[black == 1]), mean) # constructs averages for blacks
white_bin_avg = aggregate(blind_data[white == 1, 1:2], 
                          list(blind_data$pctile[white == 1]), .drop = FALSE, mean) # constructs averages for whites

# Race blind calibration curve
plot(blind_bin_avg$Group.1, blind_bin_avg$NELS.bach_rec, type = 'p', col = 'red', 
     xlab = "Deciles of predicted prob. of rec. at least bachelors", 
     ylab = "Prob. of rec. at least bachelors",
     ylim = c(0.3, 1), xaxt = "n")
lines(blind_bin_avg$Group.1, blind_bin_avg$blind_pred, type = 'l', col = 'black')
title(main = "Bachelors calib. curve: Race blind lin. pred.")
legend(7, 0.4, c("Observed", "Fitted"), 
       lty = c(NA, 1), lwd = c(NA, 2), pch = c(1, NA), col = c("red", "black"))
axis(side = 1, c(1:10))

# compare race blind predictor with observed for each race
plot(blind_bin_avg$Group.1, blind_bin_avg$blind_pred, type = "l", col = "black",
     xlab = "Quintiles of predicted prob. of rec. at least 'mostly Bs",
     ylab = "Prob of rec. at least 'mostly Bs'", 
     ylim = c(0.5, 1), xaxt = "n")
lines(white_bin_avg$Group.1, white_bin_avg$NELS.bach_rec, type = 'p', pch = 0, col = 'blue')
lines(black_bin_avg$Group.1, black_bin_avg$NELS.bach_rec, type = 'p', col = 'red')
title("Bachelors calib. curve: By racial group")
legend(8, 0.6, c("Predicted", "White", "Black"), 
       lty = c(1, NA, NA), lwd = c(2, NA, NA), pch = c(NA, 0, 1), 
       col = c("black", "blue", "red"))
axis(side = 1, c(1:10))

# Race aware predictors
black_lm = lm(NELS.bach_rec ~ 1 + compstd, data = tmp_data, subset = (black == 1))
white_lm = lm(NELS.bach_rec ~ 1 + compstd, data = tmp_data, subset = (white == 1))

# Plots the different curves
compstd_rng = seq(from = min(compstd), to = max(compstd), length.out = 50);
blind_pred = raceblind_lm$coefficients[1] + raceblind_lm$coefficients[2]*compstd_rng;
black_pred = black_lm$coefficients[1] + black_lm$coefficients[2]*compstd_rng;
white_pred = white_lm$coefficients[1] + white_lm$coefficients[2]*compstd_rng;

# Plots
plot(compstd_rng, blind_pred, type = "l", col = "red", xlab = "Composite Std. Test Z-Scores", 
     ylab = "Fitted prob. of at least completing bachelor's")
lines(compstd_rng, black_pred, type = "l", col = "blue")
lines(compstd_rng, white_pred, type = "l", col = "green")
title("Blacks with higher std. test scores have higher Yhats")
legend(0.5, 0.4, c("Race-Blind", "Blacks", "Whites"), 
       lty = c(1, 1, 1), lwd = c(2.5, 2.5, 2.5), col=c("red","blue", "green"))



