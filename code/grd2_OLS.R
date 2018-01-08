# ####################################################################
# Author: Ashesh Rambachan
# Last updated: Jan 6, 2018
# Description: This script estimates OLS to predict
# grades in college using the full set of covariates.
# I restrict the sample to only blacks and whites. The 
# data are split 80/20 into a training and test set. I estimate
# an OLS predictor that does not have access to race and a
# an OLS that does have access to race on the training
# sample. The test set is used to evaluate the predictive performance
# ####################################################################

library(dplyr)
library(ggplot2)
library(reshape2)

# !ATTENTION!: Set working directory
setwd("~/Dropbox/Harvard/Algorithmic-Bias/NELS-AERPP");

# Loads the cleaned NELS dataset
# Make sure outcome variable in data_clean.R set to 2.
source("code/data_clean.R");

y = as.numeric(y) - 1
blind_covar = subset(covar, select = -c(black, white, y))
black = covar$black
white = covar$white
race_covar = subset(covar, select = -c(white, y))
orthog_covar = race_covar # used to orthogonalize features with respect to race

# Construct interaction terms with black for each covariate
cov_names = colnames(race_covar)
cov_names = cov_names[2:length(cov_names)]
for (i in 1:length(cov_names)) {
  int = race_covar$black * race_covar[[cov_names[i]]]
  race_covar = cbind(race_covar, int)
  colnames(race_covar)[length(colnames(race_covar))] = paste0("black_", cov_names[i], ".Int", sep = "")
}
rm(int, cov_names)

# Construct training and test sets for blind and race aware predictors
set.seed(2000)
train = sample.int(n = nrow(covar), size = floor(0.8*nrow(covar)), replace = F) # index of training set
trg_sz = length(train)
tst_sz = nrow(covar) - trg_sz

black.tst = black[-train]
white.tst = white[-train]
y.tst = y[-train]

# Constructs orthogonal predictors
black.orth = orthog_covar$black
orthog_covar = subset(orthog_covar, select = -c(black))
covar.list = colnames(orthog_covar) 

# loop through each variable, regress on black indicator
# store residuals
for (i in 1:length(covar.list)) {
  resid = lsfit(black.orth, orthog_covar[[covar.list[i]]], intercept = TRUE)$residuals
  orthog_covar[[covar.list[i]]] = resid
}
rm(black.orth, covar.list, resid, i)


#########################
# Estimates regressions #
#########################
blind_covar.ols = blind_covar[, unique(names(blind_covar)), with = FALSE]
blind_ols = lm(y~., blind_covar.ols, subset = train)
blind_covar.tst = blind_covar.ols[-train, ]

aware_covar.ols = race_covar[, unique(names(race_covar)), with = FALSE]
aware_ols = lm(y~., aware_covar.ols, subset = train)
race_covar.tst = aware_covar.ols[-train, ]

orthog_covar.ols = orthog_covar[, unique(names(orthog_covar)), with = FALSE]
orthog_ols = lm(y~., orthog_covar.ols, subset = train)
orthog_covar.tst = orthog_covar.ols[-train, ]

blind_ols.prob = predict(blind_ols, blind_covar.ols[-train, ], type = 'response')
aware_ols.prob = predict(aware_ols, aware_covar.ols[-train, ], type = 'response')
orthog_ols.prob = predict(orthog_ols, orthog_covar.ols[-train, ], type = 'response')

#############
# Heat-maps #
#############
# full sample
raceblindprob.decile = ntile(blind_ols.prob, 10)
raceawareprob.decile = ntile(aware_ols.prob, 10)

heatmap = matrix(rep(0, 10*10), c(10, 10))
for (i in 1:tst_sz) {
  heatmap[raceblindprob.decile[i], raceawareprob.decile[i]] = heatmap[raceblindprob.decile[i], raceawareprob.decile[i]] + 1
}
heatmap = heatmap/tst_sz
dimnames(heatmap) <- list(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10'), 
                          c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10'))
heatmap.melted <- melt(heatmap)
ggplot(heatmap.melted, aes(x = Var1, y = Var2, fill = value)) + geom_tile(aes(fill=value, col=value)) + 
  ylab('Race aware, deciles of pred. prob') + 
  xlab('Race blind, deciles of pred. prob') + 
  ggtitle("Rec. at least mostly B's: Pred. prob. blind vs. aware") +   
  scale_colour_gradient(low = "white", high = "red") + 
  scale_fill_gradient(low = "white", high = "red")

# whites only
wht.tst = sum(white.tst)
raceblind.prob.w = blind_ols.prob[race_covar.tst$black == 0]
raceaware.prob.w = aware_ols.prob[race_covar.tst$black == 0]

raceblind.dec.w = ntile(raceblind.prob.w, 10)
raceaware.dec.w = ntile(raceaware.prob.w, 10)

heatmap.w = matrix(rep(0, 10*10), c(10, 10))
for (i in 1:wht.tst) {
  heatmap.w[raceblind.dec.w[i], raceaware.dec.w[i]] = heatmap.w[raceblind.dec.w[i], raceaware.dec.w[i]] + 1
}
heatmap.w = heatmap.w/wht.tst
dimnames(heatmap.w) <- list(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10'), 
                            c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10'))
heatmap.melted.w <- melt(heatmap.w)
ggplot(heatmap.melted.w, aes(x = Var1, y = Var2, fill = value)) + geom_tile(aes(fill=value, col=value)) + 
  ylab('Race aware, deciles of pred. prob') + 
  xlab('Race blind, deciles of pred. prob') + 
  ggtitle("Rec. at least mostly B's: Pred. prob. blind vs. aware, whites") + 
  scale_colour_gradient(low = "white", high = "red") + 
  scale_fill_gradient(low = "white", high = "red")

# blacks only
blk.tst = sum(race_covar.tst$black)
raceblindprob.b = blind_ols.prob[race_covar$black == 1]
raceawareprob.b = aware_ols.prob[race_covar$black == 1]

raceblind.dec.b = ntile(raceblindprob.b, 10)
raceaware.dec.b = ntile(raceawareprob.b, 10)

heatmap.b = matrix(rep(0, 10*10), c(10, 10))
for (i in 1:blk.tst) {
  heatmap.b[raceblind.dec.b[i], raceaware.dec.b[i]] = heatmap.b[raceblind.dec.b[i], raceaware.dec.b[i]] + 1
}
heatmap.b = heatmap.b/blk.tst
dimnames(heatmap.b) <- list(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10'), 
                            c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10'))
heatmap.melted.b <- melt(heatmap.b)
ggplot(heatmap.melted.b, aes(x = Var1, y = Var2, fill = value)) + geom_tile(aes(fill=value, col=value)) + 
  ylab('Race aware, deciles of pred. prob') + 
  xlab('Race blind, deciles of pred. prob') + 
  scale_colour_gradient(low = "white", high = "red") + 
  scale_fill_gradient(low = "white", high = "red") + 
  ggtitle("Rec. at least mostly B's: Pred. prob. blind vs. aware, blacks") 

######################
# Calibration Curves #
######################
# Race-blind predictor
blind_data = data.frame(y.tst, blind_ols.prob, race_covar.tst$black, race_covar.tst$white)
blind_data$decile = ntile(blind_data$blind_ols.prob, 10)

blind_bin_avg = aggregate(blind_data[ , 1:2], list(blind_data$decile), mean)
blk_bin_avg = aggregate(blind_data[blind_data$race_covar.tst.black == 1, 1:2], 
                        list(blind_data$decile[blind_data$race_covar.tst.black == 1]), mean)
wht_bin_avg = aggregate(blind_data[blind_data$race_covar.tst.white == 1, 1:2], 
                        list(blind_data$decile[blind_data$race_covar.tst.white == 1]), mean)

# Plots race-blind calibration curve
plot(blind_bin_avg$Group.1, blind_bin_avg$y.tst, type = 'p', col = 'red', 
     xlab = "Deciles of pred. prob. of rec. at least mostly B's", 
     ylab = "Prob. of rec. at least mostly B's", ylim = c(0.5, 1), xaxt = 'n')
lines(blind_bin_avg$Group.1, blind_bin_avg$blind_ols.prob, type = 'l', col = 'black')
title(main = "Rec. at least mostly B's calib. curve: Blind predictor")
legend(8, 0.6, c("observed", "fitted"), 
       lty = c(NA, 1), lwd = c(NA, 2), pch = c(1, NA), col = c("red", "black"))
axis(side = 1, c(1:10))

# compare race-blind predictor with each group
plot(wht_bin_avg$Group.1, wht_bin_avg$blind_ols.prob, type = 'l', lty = 1, col = 'black',
     xlab = "Deciles of pred. prob. of rec. at least mostly B's",
     ylab = "Prob. of rec. at least mostly B's", ylim = c(0.5, 1), xaxt = 'n')
lines(blk_bin_avg$Group.1, blk_bin_avg$blind_ols.prob, type = 'l', lty = 2, col = 'black')
lines(wht_bin_avg$Group.1, wht_bin_avg$y.tst, type = 'p', pch = 0, col = 'blue')
lines(blk_bin_avg$Group.1, blk_bin_avg$y.tst, type = 'p', pch = 1, col = 'red')
title(main = "Rec. at least mostly B's calib. curve: Blind, by group")
legend(7, 0.6, c('fit. whites', 'fit. blacks', 'obs. whites', 'obs. blacks'), 
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA), pch = c(NA, NA, 0, 1), 
       col = c('black', 'black', 'blue', 'red'))
axis(side = 1, c(1:10))

# Race-aware predictor
aware_data = data.frame(y.tst, aware_ols.prob, race_covar.tst$black, race_covar.tst$white)
aware_data$decile = ntile(aware_data$raceaware_prob, 10)

aware_bin_avg = aggregate(aware_data[, 1:2], list(aware_data$decile), mean)
blk_bin_avg = aggregate(aware_data[aware_data$race_covar.tst.black == 1, 1:2], 
                        list(aware_data$decile[aware_data$race_covar.tst.black == 1]), mean)
wht_bin_avg = aggregate(aware_data[aware_data$race_covar.tst.white == 1, 1:2], 
                        list(aware_data$decile[aware_data$race_covar.tst.white == 1]), mean)

# plots race-aware calibration curve
plot(aware_bin_avg$Group.1, aware_bin_avg$y.tst, type = 'p', col = 'red', 
     xlab = "Deciles of pred. prob. of rec. at least mostly B's", 
     ylab = "Prob. of rec. at least mostly B's", ylim = c(0.5, 1), xaxt = 'n')
lines(aware_bin_avg$Group.1, aware_bin_avg$aware_ols.prob, type = 'l', col = 'black')
title(main = "Rec. at least mostly B's calib. curve: Aware predictor")
legend(8, 0.6, c("observed", "fitted"), 
       lty = c(NA, 1), lwd = c(NA, 2), pch = c(1, NA), col = c("red", "black"))
axis(side = 1, c(1:10))

# compare race-aware predictor with each group
plot(wht_bin_avg$Group.1, wht_bin_avg$aware_ols.prob, type = 'l', lty = 1, col = 'black', 
     xlab = "Deciles of pred. prob. of rec. at least mostly B's",
     ylab = "Prob. of rec. at least mostly B's", ylim = c(0.5, 1))
lines(blk_bin_avg$Group.1, blk_bin_avg$aware_ols.prob, type = 'l', lty = 2, col = 'black')
lines(wht_bin_avg$Group.1, wht_bin_avg$y.tst, type = 'p', pch = 0, col = 'blue')
lines(blk_bin_avg$Group.1, blk_bin_avg$y.tst, type = 'p', col = 'red')
title(main = "Rec. at least mostly B's calib. curve: Aware, by group")
legend(7, 0.6, c('fit. whites', 'fit. blacks', 'obs. whites', 'obs. blacks'), 
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA), pch = c(NA, NA, 0, 1), 
       col = c('black', 'black', 'blue', 'red'))
axis(side = 1, c(1:10))

#############
# Densities #
############# 
# full sample
prob_change = blind_ols.prob - aware_ols.prob
fs = density(prob_change)
plot(fs, xlim = c(-0.05, 0.10), 
     xlab = "Change in pred. prob. of rec. at least mostly B's", 
     ylab = "Density",
     main = "Blind vs. aware predictors, full test sample")

# Blacks and Whites
whites = density(prob_change[race_covar.tst$white == 1])
blacks = density(prob_change[race_covar.tst$black == 1])
plot(whites$x, whites$y, type = 'l', lty = 1, col = 'red', 
     xlab = "Change in pred. prob. of rec. at least mostly B's", 
     ylab = "Density", xlim = c(-0.05, 0.05))
title(main = "Difference of Yhat of blind vs. aware predictors")
lines(blacks$x, blacks$y, type = 'l', lty = 2, col = 'blue')

####################
# Marginal effects #
####################
# Plots using a Lollipop graphs
# Standardized test scores
covariates = c('F1 english std.', 'F1 english std.', 
               'F1 math std.', 'F1 math std.', 
               'F1 science std.', 'F1 science std.',
               'F1 history std.', 'F1 history std.', 
               'F2 english std.', 'F2 english std.', 
               'F2 math std.', 'F2 math std.', 
               'F2 science std.', 'F2 science std.',
               'F2 history std.', 'F2 history std.')
race = c('white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black')
coefficient = c(aware_ols$coefficients[['F12XRSTD']], aware_ols$coefficients[['F12XRSTD']] + aware_ols$coefficients[['black_F12XRSTD.Int']],
                 aware_ols$coefficients[['F12XMSTD']], aware_ols$coefficients[['F12XMSTD']] + aware_ols$coefficients[['black_F12XMSTD.Int']],
                 aware_ols$coefficients[['F12XSSTD']], aware_ols$coefficients[['F12XSSTD']] + aware_ols$coefficients[['black_F12XSSTD.Int']],
                 aware_ols$coefficients[['F12XHSTD']], aware_ols$coefficients[['F12XHSTD']] + aware_ols$coefficients[['black_F12XHSTD.Int']], 
                 aware_ols$coefficients[['F22XRSTD']], aware_ols$coefficients[['F22XRSTD']] + aware_ols$coefficients[['black_F22XRSTD.Int']],
                 aware_ols$coefficients[['F22XMSTD']], aware_ols$coefficients[['F22XMSTD']] + aware_ols$coefficients[['black_F22XMSTD.Int']],
                 aware_ols$coefficients[['F22XSSTD']], aware_ols$coefficients[['F22XSSTD']] + aware_ols$coefficients[['black_F22XSSTD.Int']],
                 aware_ols$coefficients[['F22XHSTD']], aware_ols$coefficients[['F22XHSTD']] + aware_ols$coefficients[['black_F22XHSTD.Int']])
data = data.frame(covariates, race, coefficient)
ggplot(data, aes(coefficient, covariates)) + geom_point(aes(color = race)) + 
  geom_line(aes(group = covariates)) + ggtitle("Rec. at least mostly B's: Coef. on std. test scores")

# Average grades and credits
covariates = c('F2 english grades', 'F2 english grades', 
               'F2 english credits', 'F2 english credits', 
               'F2 math grades', 'F2 math grades',
               'F2 math credits', 'F2 math credits', 
               'F2 science grades', 'F2 science grades', 
               'F2 science credits', 'F2 science credits')
race = c('white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black',
         'white', 'black')
coefficient = c(aware_ols$coefficients[['F2RHENG2']], aware_ols$coefficients[['F2RHENG2']] + aware_ols$coefficients[['black_F2RHENG2.Int']],
                 aware_ols$coefficients[['F2RENG_C']], aware_ols$coefficients[['F2RENG_C']] + aware_ols$coefficients[['black_F2RENG_C.Int']],
                 aware_ols$coefficients[['F2RHMAG2']], aware_ols$coefficients[['F2RHMAG2']] + aware_ols$coefficients[['black_F2RHMAG2.Int']],
                 aware_ols$coefficients[['F2RMAT_C']], aware_ols$coefficients[['F2RMAT_C']] + aware_ols$coefficients[['black_F2RMAT_C.Int']],
                 aware_ols$coefficients[['F2RHSCG2']], aware_ols$coefficients[['F2RHSCG2']] + aware_ols$coefficients[['black_F2RHSCG2.Int']],
                 aware_ols$coefficients[['F2RSCI_C']], aware_ols$coefficients[['F2RSCI_C']] + aware_ols$coefficients[['black_F2RSCI_C.Int']])
data = data.frame(covariates, race, coefficient)
ggplot(data, aes(coefficient, covariates)) + geom_point(aes(color = race)) +
  geom_line(aes(group = covariates)) + ggtitle("Rec. at least mostly B's: Coef. on grades and credits")

# Plots the change in Yhat: Standardized test scores
xrg = seq(from = 0, to = 68, length.out = 20)

F12XRSTD_coef.whites = aware_ols$coefficients['F12XRSTD']
F12XRSTD_coef.blacks = aware_ols$coefficients['F12XRSTD'] + aware_ols$coefficients['black_F12XRSTD.Int']
rstd.blk = xrg * F12XRSTD_coef.blacks
rstd.wht = xrg * F12XRSTD_coef.whites

F12XMSTD_coef.whites = aware_ols$coefficients['F12XMSTD']
F12XMSTD_coef.blacks = aware_ols$coefficients['F12XMSTD'] + aware_ols$coefficients['black_F12XMSTD.Int']
mstd.blk = xrg * F12XMSTD_coef.blacks
mstd.wht = xrg * F12XMSTD_coef.whites

F12XSSTD_coef.whites = aware_ols$coefficients['F12XSSTD']
F12XSSTD_coef.blacks = aware_ols$coefficients['F12XSSTD'] + aware_ols$coefficients['black_F12XSSTD.Int']
sstd.blk = xrg * F12XSSTD_coef.blacks
sstd.wht = xrg * F12XSSTD_coef.whites

F12XHSTD_coef.whites = aware_ols$coefficients['F12XHSTD']
F12XHSTD_coef.blacks = aware_ols$coefficients['F12XHSTD'] + aware_ols$coefficients['black_F12XHSTD.Int']
hstd.blk = xrg * F12XHSTD_coef.blacks
hstd.wht = xrg * F12XHSTD_coef.whites

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1), oma = c(4, 4, 2, 2))
plot(xrg, rstd.blk, type = 'l', col = 'blue', ylim = c(-1, 0.2), 
     ylab = 'Change in pred. prob', xlab = "", main = 'English')
lines(xrg, rstd.wht, type = 'l', col = 'red')

plot(xrg, mstd.blk, type = 'l', col = 'blue', ylim = c(0, 1), 
     xlab = "", ylab = "", main = 'Math')
lines(xrg, mstd.wht, type = 'l', col = 'red')

plot(xrg, sstd.blk, type = 'l', col = 'blue', ylim = c(-1, 0), 
     ylab = 'Change in pred. prob', xlab = 'Std. scores', main = 'Science')
lines(xrg, sstd.wht, type = 'l', col = 'red')

plot(xrg, hstd.blk, type = 'l', col = 'blue', ylim = c(0, 1), 
     ylab = "", xlab = 'Std. scores', main = 'History')
lines(xrg, hstd.wht, type = 'l', col = 'red')
title(main = 'Change in pred. prob: F1 std. scores', outer = TRUE, cex = 1.5)
legend(-50, -0.75, xpd = NA, legend = c('blacks', 'whites'), horiz = TRUE,
       lty = c(1, 1), lwd = c(2, 2), col = c('red', 'blue'), cex = 1, bty = 'n')

#####################
# Policy Simulation #
#####################
###################
# Decision Rule #1: Admit top 50%
###################
rule = 50 # cutoff of race-blind rule
print(paste0("Blind Rule: Admit if Yhat > ", rule, " percentile"))

# race blind admit rule
blind_ols.pctile = ntile(blind_ols.prob, 100)
blind_admit = ifelse(blind_ols.pctile >= rule, 1, 0)
blind_admit.wht = sum(white.tst[blind_admit == 1])
blind_admit.blk = sum(black.tst[blind_admit == 1])

print(paste0("Blind admit rule admits ", sum(blind_admit)))
print(paste0("Blind admit rule admits ", blind_admit.wht, " whites"))
print(paste0("Blind admit rule admits ", blind_admit.blk, " blacks"))

# race orthog admit rule
orthog_ols.pctile = ntile(orthog_ols.prob, 100)
orthog_admit = ifelse(orthog_ols.pctile >= rule, 1, 0)
orthog_admit.wht = sum(white.tst[orthog_admit == 1])
orthog_admit.blk = sum(black.tst[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
aware_ols.pctile = ntile(aware_ols.prob, 100)
aware_admit = ifelse(aware_ols.pctile >= rule, 1, 0)
aware_admit.wht = sum(white.tst[aware_admit == 1])
aware_admit.blk = sum(black.tst[aware_admit == 1])

print(paste0("Aware admit rule admits ", sum(aware_admit)))
print(paste0("Aware admit rule admits ", aware_admit.wht, " whites"))
print(paste0("Aware admit rule admits ", aware_admit.blk, " blacks"))

# results
blind_admit.acc = mean(y.tst[blind_admit == 1])
orthog_admit.acc = mean(y.tst[orthog_admit == 1])
aware_admit.acc = mean(y.tst[aware_admit == 1])
print(paste0("Blind admit accuracy: ", blind_admit.acc))
print(paste0("Orthog admit accuracy: ", orthog_admit.acc))
print(paste0("Aware admit accuracy: ", aware_admit.acc))

blind_admit.blkp = mean(black.tst[blind_admit == 1])
orthog_admit.blkp = mean(black.tst[orthog_admit == 1])
aware_admit.blkp = mean(black.tst[aware_admit == 1])
print(paste0("Blind black %: ", blind_admit.blkp))
print(paste0("Orthog black %: ", orthog_admit.blkp))
print(paste0("Aware black %: ", aware_admit.blkp))

##################
# DECISION RULE 2: Admit same composition as race blind admit top 25%
################## 
# race orthog admit rule
wht_orthogprob.srt = sort(orthog_ols.prob[white.tst == 1], decreasing = TRUE)
blk_orthogprob.srt = sort(orthog_ols.prob[black.tst == 1], decreasing = TRUE)

whiteorthog_cutoff = wht_orthogprob.srt[blind_admit.wht]
blackorthog_cutoff = blk_orthogprob.srt[blind_admit.blk]

orthog_admit = ifelse(white.tst == 1 & orthog_ols.prob >= whiteorthog_cutoff |
                        black.tst == 1 & orthog_ols.prob >= blackorthog_cutoff, 1, 0)
orthog_admit.wht = sum(white.tst[orthog_admit == 1])
orthog_admit.blk = sum(black.tst[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
wht_awareprob.srt = sort(aware_ols.prob[white.tst == 1], decreasing = TRUE)
blk_awareprob.srt = sort(aware_ols.prob[black.tst == 1], decreasing = TRUE)

whiteaware_cutoff = wht_awareprob.srt[blind_admit.wht]
blackaware_cutoff = blk_awareprob.srt[blind_admit.blk]

aware_admit = ifelse(white.tst == 1 & aware_ols.prob >= whiteaware_cutoff |
                       black.tst == 1 & aware_ols.prob >= blackaware_cutoff, 1, 0)
aware_admit.wht = sum(white.tst[aware_admit == 1])
aware_admit.blk = sum(black.tst[aware_admit == 1])

print(paste0("Aware admit rule admits ", sum(aware_admit)))
print(paste0("Aware admit rule admits ", aware_admit.wht, " whites"))
print(paste0("Aware admit rule admits ", aware_admit.blk, " blacks"))

# results
orthog_admit.acc = mean(y.tst[orthog_admit == 1])
aware_admit.acc = mean(y.tst[aware_admit == 1])
print(paste0("Orthog admit accuracy: ", orthog_admit.acc))
print(paste0("Aware admit accuracy: ", aware_admit.acc))

orthog_admit.blkp = mean(black.tst[orthog_admit == 1])
aware_admit.blkp = mean(black.tst[aware_admit == 1])
print(paste0("Orthog black %: ", orthog_admit.blkp))
print(paste0("Aware black %: ", aware_admit.blkp))

##################
# DECISION RULE 3: 10% of admits black
##################
total.n = 483
blk.n = 49
wht.n = 434

# race blind admit rule
wht_blindprob.srt = sort(blind_ols.prob[white.tst == 1], decreasing = TRUE)
blk_blindprob.srt = sort(blind_ols.prob[black.tst == 1], decreasing = TRUE)

whiteblind_cutoff = wht_blindprob.srt[wht.n]
blackblind_cutoff = blk_blindprob.srt[blk.n]

blind_admit = ifelse(white.tst == 1 & blind_ols.prob >= whiteblind_cutoff |
                       black.tst == 1 & blind_ols.prob >= blackblind_cutoff, 1, 0)
blind_admit.wht = sum(white.tst[blind_admit == 1])
blind_admit.blk = sum(black.tst[blind_admit == 1])

print(paste0("Blind admit rule admits ", sum(blind_admit)))
print(paste0("Blind admit rule admits ", blind_admit.wht, " whites"))
print(paste0("Blind admit rule admits ", blind_admit.blk, " blacks"))

# race orthog admit rule
whiteorthog_cutoff = wht_orthogprob.srt[wht.n]
blackorthog_cutoff = blk_orthogprob.srt[blk.n]

orthog_admit = ifelse(white.tst == 1 & orthog_ols.prob >= whiteorthog_cutoff |
                        black.tst == 1 & orthog_ols.prob >= blackorthog_cutoff, 1, 0)
orthog_admit.wht = sum(white.tst[orthog_admit == 1])
orthog_admit.blk = sum(black.tst[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
whiteaware_cutoff = wht_awareprob.srt[wht.n]
blackaware_cutoff = blk_awareprob.srt[blk.n]

aware_admit = ifelse(white.tst == 1 & aware_ols.prob >= whiteaware_cutoff |
                       black.tst == 1 & aware_ols.prob >= blackaware_cutoff, 1, 0)
aware_admit.wht = sum(white.tst[aware_admit == 1])
aware_admit.blk = sum(black.tst[aware_admit == 1])

print(paste0("Aware admit rule admits ", sum(aware_admit)))
print(paste0("Aware admit rule admits ", aware_admit.wht, " whites"))
print(paste0("Aware admit rule admits ", aware_admit.blk, " blacks"))

# results
blind_admit.acc = mean(y.tst[blind_admit == 1])
orthog_admit.acc = mean(y.tst[orthog_admit == 1])
aware_admit.acc = mean(y.tst[aware_admit == 1])
print(paste0("Blind admit accuracy: ", blind_admit.acc))
print(paste0("Orthog admit accuracy: ", orthog_admit.acc))
print(paste0("Aware admit accuracy: ", aware_admit.acc))

blind_admit.blkp = mean(black.tst[blind_admit == 1])
orthog_admit.blkp = mean(black.tst[orthog_admit == 1])
aware_admit.blkp = mean(black.tst[aware_admit == 1])
print(paste0("Blind black %: ", blind_admit.blkp))
print(paste0("Orthog black %: ", orthog_admit.blkp))
print(paste0("Aware black %: ", aware_admit.blkp))

##################
# DECISION RULE 4: 5% of admits black
##################
total.n = 483
blk.n = 25
wht.n = 458

# race blind admit rule
whiteblind_cutoff = wht_blindprob.srt[wht.n]
blackblind_cutoff = blk_blindprob.srt[blk.n]

blind_admit = ifelse(white.tst == 1 & blind_ols.prob >= whiteblind_cutoff |
                       black.tst == 1 & blind_ols.prob >= blackblind_cutoff, 1, 0)
blind_admit.wht = sum(white.tst[blind_admit == 1])
blind_admit.blk = sum(black.tst[blind_admit == 1])

print(paste0("Blind admit rule admits ", sum(blind_admit)))
print(paste0("Blind admit rule admits ", blind_admit.wht, " whites"))
print(paste0("Blind admit rule admits ", blind_admit.blk, " blacks"))

# race orthog admit rule
whiteorthog_cutoff = wht_orthogprob.srt[wht.n]
blackorthog_cutoff = blk_orthogprob.srt[blk.n]

orthog_admit = ifelse(white.tst == 1 & orthog_ols.prob >= whiteorthog_cutoff |
                        black.tst == 1 & orthog_ols.prob >= blackorthog_cutoff, 1, 0)
orthog_admit.wht = sum(white.tst[orthog_admit == 1])
orthog_admit.blk = sum(black.tst[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
whiteaware_cutoff = wht_awareprob.srt[wht.n]
blackaware_cutoff = blk_awareprob.srt[blk.n]

aware_admit = ifelse(white.tst == 1 & aware_ols.prob >= whiteaware_cutoff |
                       black.tst == 1 & aware_ols.prob >= blackaware_cutoff, 1, 0)
aware_admit.wht = sum(white.tst[aware_admit == 1])
aware_admit.blk = sum(black.tst[aware_admit == 1])

print(paste0("Aware admit rule admits ", sum(aware_admit)))
print(paste0("Aware admit rule admits ", aware_admit.wht, " whites"))
print(paste0("Aware admit rule admits ", aware_admit.blk, " blacks"))

# results
blind_admit.acc = mean(y.tst[blind_admit == 1])
orthog_admit.acc = mean(y.tst[orthog_admit == 1])
aware_admit.acc = mean(y.tst[aware_admit == 1])
print(paste0("Blind admit accuracy: ", blind_admit.acc))
print(paste0("Orthog admit accuracy: ", orthog_admit.acc))
print(paste0("Aware admit accuracy: ", aware_admit.acc))

blind_admit.blkp = mean(black.tst[blind_admit == 1])
orthog_admit.blkp = mean(black.tst[orthog_admit == 1])
aware_admit.blkp = mean(black.tst[aware_admit == 1])
print(paste0("Blind black %: ", blind_admit.blkp))
print(paste0("Orthog black %: ", orthog_admit.blkp))
print(paste0("Aware black %: ", aware_admit.blkp))


