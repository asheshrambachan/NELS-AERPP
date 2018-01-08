# ####################################################################
# Author: Ashesh Rambachan
# Last updated: Jan 1, 2018
# Description: This script estimates the random forest for to predict
# grades in college using the full set of covariates.
# I restrict the sample to only blacks and whites. The 
# data are split 80/20 into a training and test set. I estimate
# a random forest that does not have access to race and a
# a random forest that does have access to race on the training
# sample. The test set is used to evaluate the predictive performance
# of each random forest.
# ####################################################################

library(randomForest)
library(dplyr)
library(ggplot2)
library(reshape2)

# !ATTENTION!: Set working directory
setwd("~/Dropbox/Harvard/Algorithmic-Bias/NELS-AERPP");

# Loads the cleaned NELS dataset
# Make sure outcome variable in data_clean.R set to 2.
source("code/data_clean.R");

blind_covar = subset(covar, select = -c(black, white, y))
race_covar = subset(covar, select = -c(y))
orthog_covar = race_covar # used to orthogonalize features with respect to race

# Construct training and test sets
set.seed(2000)
train = sample.int(n = nrow(covar), size = floor(0.8*nrow(covar)), replace = F) # index of training set
trg_sz = length(train)
tst_sz = nrow(covar) - trg_sz

######################################
# Construct Race Blind Random Forest #
######################################
# Tunes sample size
set.seed(3000)
raceblind_tune = randomForest(blind_covar[train, ], y[train], ntree = 2000)
oob_tune_blind = c(raceblind_tune$err.rate[,1])
for (i in 1:4) { 
  raceblind_tune = randomForest(blind_covar[train, ], y[train ], sampsize = (i/5)*trg_sz, ntree = 2000)
  oob_tune_blind = cbind(oob_tune_blind, raceblind_tune$err.rate[,1])
  print(i)
}

# Plots out-of-bag errors for different sample sizes
plot(oob_tune_blind[, 1], type = 'l', col = "blue", xlab = "Number of Trees", ylab = "OOB Error",
     ylim = c(0.185, 0.21))
clrs = c("red", "green", "orange", "cyan")
for (i in 1:4) { 
  lines(oob_tune_blind[,i+1], type = 'l', col = clrs[i])
}
title("Rec. at least mostly B's sampsize tuning: Race blind")
legend("topright", c("Def.", "20%", "40%", "60%", "80%"), 
       lty = c(1, 1, 1, 1, 1), lwd = c(1, 1, 1, 1, 1), 
       col = c("blue", "red", "green", "orange", "cyan"))

# Selects sample size of 20% of training size with 600 trees
# Tunes number of variables randomly sampled at each split
# Best forest is returned and used from this
set.seed(4000)
raceblind_rf = tuneRF(blind_covar[train, ], y[train], ntreeTry = 2000, sampsize = 0.2*trg_sz, stepfactor = 1.5,
                      improve = 1e-5, trace = TRUE, plot = TRUE, doBest = FALSE)

# Plots MTRY tuning
plot(raceblind_rf[, 1], raceblind_rf[, 2], type = "o", col = "black",
     xlab = "Variables rand. sampled for split", ylab = "OOB Error")
title("Rec. at least mostly B's mtry tuning: Race blind, 20% bootstrap")

# Estimates the race blind random forest. Based on sampsize tuning, looks like 1500 trees will suffice.
# Optimal mtry = 5
set.seed(5000)
RF_blind = randomForest(blind_covar[train, ], y[train], ntree = 1500, sampsize = 0.2*trg_sz, mtry = 5)

######################################
# Construct Race-Aware Random Forest #
######################################
# Tunes sample size
set.seed(3000)
raceaware_tune = randomForest(race_covar[train, ], y[train], ntree = 2000, importance = TRUE)
oob_tune_aware = c(raceaware_tune$err.rate[,1])
for (i in 1:4) {
  raceaware_tune = randomForest(race_covar[train, ], y[train ], sampsize = (i/5)*trg_sz, ntree = 2000)
  oob_tune_aware = cbind(oob_tune_aware, raceaware_tune$err.rate[,1])
  print(i)
}

# Plots out-of-bag errors for different sample sizes
plot(oob_tune_aware[,1], type = 'l', col = "blue", xlab = "Number of Trees", ylab = "OOB Error",
     ylim = c(0.185, 0.21))
clrs = c("red", "green", "orange", "cyan")
for (i in 1:4) { 
  lines(oob_tune_aware[,i+1], type = 'l', col = clrs[i])
}
title("Grades sampsize tuning: Race aware")
legend("topright", c("def.", "20%", "40%", "60%", "80%"), 
       lty = c(1, 1, 1, 1, 1), lwd = c(1, 1, 1, 1, 1), 
       col = c("blue", "red", "green", "orange", "cyan"))

# Select sample size to 60% set to search over 2000. 
# tunes number of variables randomly sampled at each split
set.seed(4000)
raceaware_rf = tuneRF(race_covar[train, ], y[train], ntreeTry = 2000, sampsize = 0.6*trg_sz, stepfactor = 1.5, 
                      improve = 1e-5, trace = TRUE, plot = TRUE, doBest = FALSE)

# Plots MTRY tuning
plot(raceaware_rf[, 1], raceaware_rf[, 2], type = "o", col = "black",
     xlab = "Variables rand. sampled for split", ylab = "OOB Error")
title("Rec. at least mostly B's mtry tuning: Race aware, 60% bootstrap")

# Estimates the race aware random forest
# Based on sampsize tuning, looks like 1500 trees will suffice.
# Set mtry to 10
set.seed(5000)
RF_aware = randomForest(race_covar[train, ], y[train], sampsize = 0.6*trg_sz, ntree = 1500, mtry = 10)

#############################
# Out-of-sample Performance #
#############################
raceblind_pred = predict(RF_blind, blind_covar[-train, ], type = "response")
raceaware_pred = predict(RF_aware, race_covar[-train, ], type = "response")
 
table(observed = y[-train], predicted = raceblind_pred)
table(observed = y[-train], predicted = raceaware_pred)

blind_covar.tst = blind_covar[-train, ]
race_covar.tst = race_covar[-train, ]
y.tst = as.numeric(y[-train]) - 1

raceblind_prob = predict(RF_blind, blind_covar[-train, ], type = "prob")[, 2]
raceaware_prob = predict(RF_aware, race_covar[-train, ], type = "prob")[, 2]

#####################################
# Heatmap Deciles of race blind and # 
# race aware predictors             #
#####################################
# full sample
raceblindprob.decile = ntile(raceblind_prob, 10)
raceawareprob.decile = ntile(raceaware_prob, 10)

heatmap = matrix(rep(0, 10*10), c(10, 10))
for (i in 1:tst_sz) {
  heatmap[raceblindprob.decile[i], raceawareprob.decile[i]] = heatmap[raceblindprob.decile[i], raceawareprob.decile[i]] + 1
}
heatmap = heatmap/tst_sz
dimnames(heatmap) <- list(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10'), 
                          c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10'))
heatmap.melted <- melt(heatmap)
ggplot(heatmap.melted, aes(x = Var1, y = Var2, fill = value)) + geom_tile(aes(fill=value, col=value)) + 
  ylab('Race aware pred. prob') + 
  xlab('Race blind pred. prob') + 
  ggtitle("Rec. at least mostly B's: Deciles of pred. prob. blind vs. aware") +   
  scale_colour_gradient(low = "white", high = "red") + 
  scale_fill_gradient(low = "white", high = "red")

# whites only
wht.tst = sum(race_covar.tst$white)
raceblind.prob.w = raceblind_prob[race_covar.tst$white == 1]
raceaware.prob.w = raceaware_prob[race_covar.tst$white == 1]

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
  ylab('Race aware pred. prob') + 
  xlab('Race blind pred. prob') + 
  ggtitle("Rec. at least mostly B's: Deciles of pred. prob. blind vs. aware, whites") + 
  scale_colour_gradient(low = "white", high = "red") + 
  scale_fill_gradient(low = "white", high = "red")

# blacks only
blk.tst = sum(race_covar.tst$black)
raceblindprob.b = raceblind_prob[race_covar$black == 1]
raceawareprob.b = raceaware_prob[race_covar$black == 1]

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
  ylab('Race aware pred. prob') + 
  xlab('Race blind pred. prob') + 
  scale_colour_gradient(low = "white", high = "red") + 
  scale_fill_gradient(low = "white", high = "red") + 
  ggtitle("Rec. at least mostly B's: Deciles of pred. prob. blind vs. aware, blacks") 

######################
# Calibration Curves #
######################
# Race-blind predictor
blind_data = data.frame(y.tst, raceblind_prob, race_covar.tst$black, race_covar.tst$white)
blind_data$decile = ntile(blind_data$raceblind_prob, 10)

blind_bin_avg = aggregate(blind_data[ , 1:2], list(blind_data$decile), mean)
blk_bin_avg = aggregate(blind_data[blind_data$race_covar.tst.black == 1, 1:2], 
                        list(blind_data$decile[blind_data$race_covar.tst.black == 1]), mean)
wht_bin_avg = aggregate(blind_data[blind_data$race_covar.tst.white == 1, 1:2], 
                        list(blind_data$decile[blind_data$race_covar.tst.white == 1]), mean)

# Plots race-blind calibration curve
plot(blind_bin_avg$Group.1, blind_bin_avg$y.tst, type = 'p', col = 'red', 
     xlab = "Deciles of pred. prob. of rec. at least mostly B's", 
     ylab = "Prob. of rec. at least mostly B's", ylim = c(0.5, 1), xaxt = 'n')
lines(blind_bin_avg$Group.1, blind_bin_avg$raceblind_prob, type = 'l', col = 'black')
title(main = "Rec. at least mostly B's calib. curve: Blind predictor")
legend(8, 0.6, c("observed", "fitted"), 
       lty = c(NA, 1), lwd = c(NA, 2), pch = c(1, NA), col = c("red", "black"))
axis(side = 1, c(1:10))

# compare race-blind predictor with each group
plot(wht_bin_avg$Group.1, wht_bin_avg$raceblind_prob, type = 'l', lty = 1, col = 'black',
     xlab = "Deciles of pred. prob. of rec. at least mostly B's",
     ylab = "Prob. of rec. at least mostly B's", ylim = c(0.5, 1), xaxt = 'n')
lines(blk_bin_avg$Group.1, blk_bin_avg$raceblind_prob, type = 'l', lty = 2, col = 'black')
lines(wht_bin_avg$Group.1, wht_bin_avg$y.tst, type = 'p', pch = 0, col = 'blue')
lines(blk_bin_avg$Group.1, blk_bin_avg$y.tst, type = 'p', pch = 1, col = 'red')
title(main = "Rec. at least mostly B's calib. curve: Blind, by group")
legend(7, 0.6, c('fit. whites', 'fit. blacks', 'obs. whites', 'obs. blacks'), 
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA), pch = c(NA, NA, 0, 1), 
       col = c('black', 'black', 'blue', 'red'))
axis(side = 1, c(1:10))

# Race-aware predictor
aware_data = data.frame(y.tst, raceaware_prob, race_covar.tst$black, race_covar.tst$white)
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
lines(aware_bin_avg$Group.1, aware_bin_avg$raceaware_prob, type = 'l', col = 'black')
title(main = "Rec. at least mostly B's calib. curve: Aware predictor")
legend(8, 0.6, c("observed", "fitted"), 
       lty = c(NA, 1), lwd = c(NA, 2), pch = c(1, NA), col = c("red", "black"))
axis(side = 1, c(1:10))

# compare race-aware predictor with each group
plot(wht_bin_avg$Group.1, wht_bin_avg$raceaware_prob, type = 'l', lty = 1, col = 'black', 
     xlab = "Deciles of pred. prob. of rec. at least mostly B's",
     ylab = "Prob. of rec. at least mostly B's", ylim = c(0.5, 1))
lines(blk_bin_avg$Group.1, blk_bin_avg$raceaware_prob, type = 'l', lty = 2, col = 'black')
lines(wht_bin_avg$Group.1, wht_bin_avg$y.tst, type = 'p', pch = 0, col = 'blue')
lines(blk_bin_avg$Group.1, blk_bin_avg$y.tst, type = 'p', col = 'red')
title(main = "Rec. at least mostly B's calib. curve: Aware, by group")
legend(7, 0.6, c('fit. whites', 'fit. blacks', 'obs. whites', 'obs. blacks'), 
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA), pch = c(NA, NA, 0, 1), 
       col = c('black', 'black', 'blue', 'red'))
axis(side = 1, c(1:10))

#########################################
# Densities: Plots distribution         #
# of changes in predicted probabilities #
#########################################
# full sample
prob_change = raceblind_prob - raceaware_prob
fs = density(prob_change)
plot(fs, xlim = c(-0.05, 0.15), 
     xlab = "Change in pred. prob. of rec. at least mostly B's", 
     ylab = "Density",
     main = "Race blind vs. race aware predictors, full test sample")

whites = density(prob_change[race_covar.tst$white == 1])
blacks = density(prob_change[race_covar.tst$black == 1])
plot(whites$x, whites$y, type = 'l', lty = 1, col = 'red', 
     xlab = "Change in pred. prob. of rec. at least mostly B's", 
     ylab = "Density")
title(main = "Difference of Yhat of blind vs. aware predictors")
lines(blacks$x, blacks$y, type = 'l', lty = 2, col = 'blue')

############################
# partial dependence plots #
############################
# Follow-Up 1 standardized test scores
op = par(mfrow = c(2, 2), oma = c(4, 2, 2, 1), 
         mar = c(2, 2, 1, 0))
partialPlot(RF_aware, race_covar[race_covar$black == 1], F12XRSTD,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 68),
            ylab = "Pred. prob.",
            main = "English") # english scores
partialPlot(RF_aware, race_covar[race_covar$white == 1], F12XRSTD, 
            which.class = 1, add = TRUE, col = 'blue') # english scores

partialPlot(RF_aware, race_covar[race_covar$black == 1], F12XMSTD,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 68),
            main = "Math") # math scores
partialPlot(RF_aware, race_covar[race_covar$white == 1], F12XMSTD, 
            which.class = 1, add = TRUE, col = 'blue') # math scores

partialPlot(RF_aware, race_covar[race_covar$black == 1], F12XSSTD, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 68),
            ylab = "Pred. prob.", xlab = "Std. scores", 
            main = "Science") # science scores
partialPlot(RF_aware, race_covar[race_covar$white == 1], F12XSSTD, 
            which.class = 1, col = "blue", add = TRUE) # science scores

partialPlot(RF_aware, race_covar[race_covar$black == 1], F12XHSTD, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 68),
            xlab = "Std. scores",
            main = "History") # history scores
partialPlot(RF_aware, race_covar[race_covar$white == 1], F12XHSTD, 
            which.class = 1, col = "blue", add = TRUE) # History scores

title(main = "Partial dep. of F1 std. scores: Race Aware", outer = TRUE, cex = 1.5)
legend(-30, -0.5, inset = c(0, 0), xpd = NA, legend =  c("blacks", "whites"), horiz = TRUE, 
       lty = c(1, 1), lwd = c(2, 2), col = c('red', 'blue'), cex = 1, bty = 'n')

# Follow-up 2 standardized test scores
par(mfrow = c(2, 2), oma = c(4, 2, 2, 1), 
    mar = c(2, 2, 1, 0))

p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F22XRSTD,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 66),
            ylab = "Pred. prob.",
            main = "English") # english scores
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F22XRSTD, 
            which.class = 1, add = TRUE, col = 'blue') # english scores
reading = cbind(p1$x, p2$y - p2$x)

p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F22XMSTD,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 66),
            main = "Math") # math scores
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F22XMSTD, 
            which.class = 1, add = TRUE, col = 'blue') # math scores
math = cbind(p1$x, p2$y - p2$x)

p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F22XSSTD, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 66),
            ylab = "Pred. prob.", xlab = "Std. scores", 
            main = "Science") # science scores
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F22XSSTD, 
            which.class = 1, col = "blue", add = TRUE) # science scores
science = cbind(p1$x, p2$y - p2$x)

p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F22XHSTD, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(0, 66),
            xlab = "Std. scores",
            main = "History") # history scores
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F22XHSTD, 
            which.class = 1, col = "blue", add = TRUE) # History scores
history = cbind(p1$x, p2$y - p2$x)

title(main = "Partial dep. of F2 std. scores: Race Aware", outer = TRUE, cex = 1.5)
legend(-30, -0.5, inset = c(0, 0), xpd = NA, legend =  c("blacks", "whites"), horiz = TRUE, 
       lty = c(1, 1), lwd = c(2, 2), col = c('red', 'blue'), cex = 1, bty = 'n')

# Follow-up 2 average grades
p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RHENG2,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 13),
            ylab = "Pred. prob.",
            main = "English") # english grades
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RHENG2, 
            which.class = 1, add = TRUE, col = 'blue') # english grades
eng = cbind(p1$x, p2$y - p1$y)

p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RHMAG2,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 13),
            main = "Math") # math grades
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RHMAG2, 
            which.class = 1, add = TRUE, col = 'blue') # math grades
math = cbind(p1$x, p2$y - p1$y)

p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RHSCG2, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 13),
            ylab = "Pred. prob.", xlab = "Grades", 
            main = "Science") # science grades
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RHSCG2, 
            which.class = 1, col = "blue", add = TRUE) # science grades
science = cbind(p1$x, p2$y - p1$y)

p1 = partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RHSOG2, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 13),
            xlab = "Grades",
            main = "Social Studies") # social studies grades
p2 = partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RHSOG2, 
            which.class = 1, col = "blue", add = TRUE) # social studies grades
social = cbind(p1$x, p2$y - p1$y)

par(mfrow = c(2, 2), oma = c(4, 2, 2, 1), mar = c(2, 2, 1, 0))
plot(eng[, 1], eng[, 2], type = 'l', col = 'blue', main = "English", xlim = c(0, 12), ylim = c(0, 0.4))
plot(math[, 1], math[, 2], type = 'l', col = 'blue', main = "Math", xlim = c(0, 12), ylim = c(0, 0.4))
plot(science[, 1], science[, 2], type = 'l', col = 'blue', main = "Science", xlim = c(0, 12), ylim = c(0, 0.4))
plot(social[, 1], social[, 2], type = 'l', col = 'blue', main = "Social Studies", xlim = c(0, 12), ylim = c(0, 0.4))
title(main = "Diff. of partial dep. of F2 grades: Race Aware", outer = TRUE, cex = 1.5)

# Follow-up 2 number of credits
par(mfrow = c(2, 2), oma = c(4, 2, 2, 1), 
    mar = c(2, 2, 1, 0))

partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RENG_C,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 14),
            main = "English") # english grades
partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RENG_C, 
            which.class = 1, add = TRUE, col = 'blue') # english grades

partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RMAT_C,
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 7),
            main = "Math") # math grades
partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RMAT_C, 
            which.class = 1, add = TRUE, col = 'blue') # math grades

partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RSCI_C, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 10),
            ylab = "Pred. prob.", xlab = "Grades", 
            main = "Science") # science grades
partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RSCI_C, 
            which.class = 1, col = "blue", add = TRUE) # science grades

partialPlot(RF_aware, race_covar[race_covar$black == 1], F2RSOC_C, 
            which.class = 1, col = 'red', ylim = c(0, 1), xlim = c(2, 14),
            xlab = "Grades",
            main = "Social Studies") # social studies grades
partialPlot(RF_aware, race_covar[race_covar$white == 1], F2RSOC_C, 
            which.class = 1, col = "blue", add = TRUE) # social studies grades
title(main = "Partial dep. of F2 credits: Race Aware", outer = TRUE, cex = 1.5)
legend(-5, -0.5, inset = c(0, 0), xpd = NA, legend =  c("blacks", "whites"), horiz = TRUE, 
       lty = c(1, 1), lwd = c(2, 2), col = c('red', 'blue'), cex = 1, bty = 'n')

######################################
# Orthogonalized prediction function #
######################################
black.orth = orthog_covar$black
orthog_covar = subset(orthog_covar, select = -c(black, white))
covar.list = colnames(orthog_covar) 

# loop through each variable, regress on black indicator
# store residuals
for (i in 1:length(covar.list)) {
  resid = lsfit(black.orth, orthog_covar[[covar.list[i]]], intercept = TRUE)$residuals
  orthog_covar[[covar.list[i]]] = resid
}
rm(black.orth)

# Constructs orthogonal random forest
# Tunes sample size
set.seed(3000)
raceorthog_tune = randomForest(orthog_covar[train, ], y[train], ntree = 2000)
oob_tune_orthog = c(raceorthog_tune$err.rate[,1])
for (i in 1:4) { 
  raceorthog_tune = randomForest(orthog_covar[train, ], y[train ], sampsize = (i/5)*trg_sz, ntree = 2000)
  oob_tune_orthog = cbind(oob_tune_orthog, raceorthog_tune$err.rate[,1])
  print(i)
}

# Plots out-of-bag errors for different sample sizes
plot(oob_tune_orthog[, 1], type = 'l', col = "blue", xlab = "Number of Trees", ylab = "OOB Error",
     ylim = c(0.185, 0.21))
clrs = c("red", "green", "orange", "cyan")
for (i in 1:4) { 
  lines(oob_tune_orthog[,i+1], type = 'l', col = clrs[i])
}
title("Rec. at least mostly B's sampsize tuning: Orthog")
legend("topright", c("Def.", "20%", "40%", "60%", "80%"), 
       lty = c(1, 1, 1, 1, 1), lwd = c(1, 1, 1, 1, 1), 
       col = c("blue", "red", "green", "orange", "cyan"))

# Selects sample size of 40% of training size
# Tunes number of variables randomly sampled at each split
# Best forest is returned and used from this
set.seed(4000)
raceorthog_rf = tuneRF(orthog_covar[train, ], y[train], ntreeTry = 2000, sampsize = 0.4*trg_sz, stepfactor = 1.5,
                      improve = 1e-5, trace = TRUE, plot = TRUE, doBest = FALSE)

# Plots MTRY tuning
plot(raceorthog_rf[, 1], raceorthog_rf[, 2], type = "o", col = "black",
     xlab = "Variables rand. sampled for split", ylab = "OOB Error")
title("At least mostly B's mtry tuning: Orthog, 40%")

# Estimates the race blind random forest. Based on sampsize tuning, looks like 1500 trees will suffice.
# Optimal mtry = 5
set.seed(5000)
RF_orthog = randomForest(orthog_covar[train, ], y[train], ntree = 1500, sampsize = 0.4*trg_sz, mtry = 5)

# Out-of-sample estimates
raceorthog_pred = predict(RF_orthog, orthog_covar[-train, ], type = "response")
table(observed = y[-train], predicted = raceorthog_pred)

raceorthog_prob = predict(RF_orthog, orthog_covar[-train, ], type = "prob")[, 2]
orthog_covar.tst = orthog_covar[-train, ]

#############################
# policy rule decision rule #
#############################
##################
# DECISION RULE 1: Admit top 25%
##################
rule = 50 # cutoff of race-blind rule
print(paste0("Blind Rule: Admit if Yhat > ", rule, " percentile"))

# race blind admit rule
blind_pctile = ntile(raceblind_prob, 100)
blind_admit = ifelse(blind_pctile >= rule, 1, 0)
blind_admit.wht = sum(race_covar.tst$white[blind_admit == 1])
blind_admit.blk = sum(race_covar.tst$black[blind_admit == 1])

print(paste0("Blind admit rule admits ", sum(blind_admit)))
print(paste0("Blind admit rule admits ", blind_admit.wht, " whites"))
print(paste0("Blind admit rule admits ", blind_admit.blk, " blacks"))

# race orthog admit rule
orthog_pctile = ntile(raceorthog_prob, 100)
orthog_admit = ifelse(orthog_pctile >= rule, 1, 0)
orthog_admit.wht = sum(race_covar.tst$white[orthog_admit == 1])
orthog_admit.blk = sum(race_covar.tst$black[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
aware_pctile = ntile(raceaware_prob, 100)
aware_admit = ifelse(aware_pctile >= rule, 1, 0)
aware_admit.wht = sum(race_covar.tst$white[aware_admit == 1])
aware_admit.blk = sum(race_covar.tst$black[aware_admit == 1])

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

blind_admit.blkp = mean(race_covar.tst$black[blind_admit == 1])
orthog_admit.blkp = mean(race_covar.tst$black[orthog_admit == 1])
aware_admit.blkp = mean(race_covar.tst$black[aware_admit == 1])
print(paste0("Blind black %: ", blind_admit.blkp))
print(paste0("Orthog black %: ", orthog_admit.blkp))
print(paste0("Aware black %: ", aware_admit.blkp))

##################
# DECISION RULE 2: Admit same composition as race blind admit top 25%
##################
# race orthog admit rule
wht_orthogprob.srt = sort(raceorthog_prob[race_covar.tst$white == 1], decreasing = TRUE)
blk_orthogprob.srt = sort(raceorthog_prob[race_covar.tst$black == 1], decreasing = TRUE)

whiteorthog_cutoff = wht_orthogprob.srt[blind_admit.wht]
blackorthog_cutoff = blk_orthogprob.srt[blind_admit.blk]

orthog_admit = ifelse(race_covar.tst$white == 1 & raceorthog_prob >= whiteorthog_cutoff |
                       race_covar.tst$black == 1 & raceorthog_prob >= blackorthog_cutoff, 1, 0)
orthog_admit[race_covar.tst$white == 1 & raceorthog_prob == whiteorthog_cutoff] = c(1, 0)
orthog_admit.wht = sum(race_covar.tst$white[orthog_admit == 1])
orthog_admit.blk = sum(race_covar.tst$black[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
wht_awareprob.srt = sort(raceaware_prob[race_covar.tst$white == 1], decreasing = TRUE)
blk_awareprob.srt = sort(raceaware_prob[race_covar.tst$black == 1], decreasing = TRUE)

whiteaware_cutoff = wht_awareprob.srt[blind_admit.wht]
blackaware_cutoff = blk_awareprob.srt[blind_admit.blk]

aware_admit = ifelse(race_covar.tst$white == 1 & raceaware_prob >= whiteaware_cutoff |
                       race_covar.tst$black == 1 & raceaware_prob >= blackaware_cutoff, 1, 0)
aware_admit.wht = sum(race_covar.tst$white[aware_admit == 1])
aware_admit.blk = sum(race_covar.tst$black[aware_admit == 1])

print(paste0("Aware admit rule admits ", sum(aware_admit)))
print(paste0("Aware admit rule admits ", aware_admit.wht, " whites"))
print(paste0("Aware admit rule admits ", aware_admit.blk, " blacks"))

# results
orthog_admit.acc = mean(y.tst[orthog_admit == 1])
aware_admit.acc = mean(y.tst[aware_admit == 1])
print(paste0("Orthog admit accuracy: ", orthog_admit.acc))
print(paste0("Aware admit accuracy: ", aware_admit.acc))

orthog_admit.blkp = mean(race_covar.tst$black[orthog_admit == 1])
aware_admit.blkp = mean(race_covar.tst$black[aware_admit == 1])
print(paste0("Orthog black %: ", orthog_admit.blkp))
print(paste0("Aware black %: ", aware_admit.blkp))

##################
# DECISION RULE 3: 10% of admits black
##################
total.n = 483
blk.n = 49
wht.n = 434

# race blind admit rule
wht_blindprob.srt = sort(raceblind_prob[race_covar.tst$white == 1], decreasing = TRUE)
blk_blindprob.srt = sort(raceblind_prob[race_covar.tst$black == 1], decreasing = TRUE)

whiteblind_cutoff = wht_blindprob.srt[wht.n]
blackblind_cutoff = blk_blindprob.srt[blk.n]

blind_admit = ifelse(race_covar.tst$white == 1 & raceblind_prob >= whiteblind_cutoff |
                       race_covar.tst$black == 1 & raceblind_prob >= blackblind_cutoff, 1, 0)
# exists tie for last-in whites. Take first index
blind_admit[race_covar.tst$white == 1 & raceblind_prob == whiteblind_cutoff] = c(1, 1, 1, 0, 0)
blind_admit[race_covar.tst$black == 1 & raceblind_prob == blackblind_cutoff] = c(1, 0)
blind_admit.wht = sum(race_covar.tst$white[blind_admit == 1])
blind_admit.blk = sum(race_covar.tst$black[blind_admit == 1])

print(paste0("Blind admit rule admits ", sum(blind_admit)))
print(paste0("Blind admit rule admits ", blind_admit.wht, " whites"))
print(paste0("Blind admit rule admits ", blind_admit.blk, " blacks"))

# race orthog admit rule
whiteorthog_cutoff = wht_orthogprob.srt[wht.n]
blackorthog_cutoff = blk_orthogprob.srt[blk.n]

orthog_admit = ifelse(race_covar.tst$white == 1 & raceorthog_prob >= whiteorthog_cutoff |
                        race_covar.tst$black == 1 & raceorthog_prob >= blackorthog_cutoff, 1, 0)
orthog_admit[race_covar.tst$white == 1 & raceorthog_prob == whiteorthog_cutoff] = c(1, 1, 0, 0, 0)
orthog_admit.wht = sum(race_covar.tst$white[orthog_admit == 1])
orthog_admit.blk = sum(race_covar.tst$black[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
whiteaware_cutoff = wht_awareprob.srt[wht.n]
blackaware_cutoff = blk_awareprob.srt[blk.n]

aware_admit = ifelse(race_covar.tst$white == 1 & raceaware_prob >= whiteaware_cutoff |
                       race_covar.tst$black == 1 & raceaware_prob >= blackaware_cutoff, 1, 0)
aware_admit.wht = sum(race_covar.tst$white[aware_admit == 1])
aware_admit.blk = sum(race_covar.tst$black[aware_admit == 1])

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
      
blind_admit.blkp = mean(race_covar.tst$black[blind_admit == 1])
orthog_admit.blkp = mean(race_covar.tst$black[orthog_admit == 1])
aware_admit.blkp = mean(race_covar.tst$black[aware_admit == 1])
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
wht_blindprob.srt = sort(raceblind_prob[race_covar.tst$white == 1], decreasing = TRUE)
blk_blindprob.srt = sort(raceblind_prob[race_covar.tst$black == 1], decreasing = TRUE)

whiteblind_cutoff = wht_blindprob.srt[wht.n]
blackblind_cutoff = blk_blindprob.srt[blk.n]

blind_admit = ifelse(race_covar.tst$white == 1 & raceblind_prob >= whiteblind_cutoff |
                       race_covar.tst$black == 1 & raceblind_prob >= blackblind_cutoff, 1, 0)
# exists tie for last-in whites. Take first index
blind_admit[race_covar.tst$white == 1 & raceblind_prob == whiteblind_cutoff] = c(1, 1, 0)
blind_admit.wht = sum(race_covar.tst$white[blind_admit == 1])
blind_admit.blk = sum(race_covar.tst$black[blind_admit == 1])

print(paste0("Blind admit rule admits ", sum(blind_admit)))
print(paste0("Blind admit rule admits ", blind_admit.wht, " whites"))
print(paste0("Blind admit rule admits ", blind_admit.blk, " blacks"))

# race orthog admit rule
whiteorthog_cutoff = wht_orthogprob.srt[wht.n]
blackorthog_cutoff = blk_orthogprob.srt[blk.n]

orthog_admit = ifelse(race_covar.tst$white == 1 & raceorthog_prob >= whiteorthog_cutoff |
                        race_covar.tst$black == 1 & raceorthog_prob >= blackorthog_cutoff, 1, 0)
# exists tie for last-in whites. Take first index
orthog_admit[race_covar.tst$white == 1 & raceorthog_prob == whiteorthog_cutoff] = c(1, 1, 1, 0)
orthog_admit.wht = sum(race_covar.tst$white[orthog_admit == 1])
orthog_admit.blk = sum(race_covar.tst$black[orthog_admit == 1])

print(paste0("Orthog admit rule admits ", sum(orthog_admit)))
print(paste0("Orthog admit rule admits ", orthog_admit.wht, " whites"))
print(paste0("Orthog admit rule admits ", orthog_admit.blk, " blacks"))

# race aware admit rule
whiteaware_cutoff = wht_awareprob.srt[wht.n]
blackaware_cutoff = blk_awareprob.srt[blk.n]

aware_admit = ifelse(race_covar.tst$white == 1 & raceaware_prob >= whiteaware_cutoff |
                       race_covar.tst$black == 1 & raceaware_prob >= blackaware_cutoff, 1, 0)
# exists tie for last-in whites. Take first index
aware_admit[race_covar.tst$white == 1 & raceaware_prob == whiteaware_cutoff] = c(1, 1, 0) 
aware_admit.wht = sum(race_covar.tst$white[aware_admit == 1])
aware_admit.blk = sum(race_covar.tst$black[aware_admit == 1])

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

blind_admit.blkp = mean(race_covar.tst$black[blind_admit == 1])
orthog_admit.blkp = mean(race_covar.tst$black[orthog_admit == 1])
aware_admit.blkp = mean(race_covar.tst$black[aware_admit == 1])
print(paste0("Blind black %: ", blind_admit.blkp))
print(paste0("Orthog black %: ", orthog_admit.blkp))
print(paste0("Aware black %: ", aware_admit.blkp))
