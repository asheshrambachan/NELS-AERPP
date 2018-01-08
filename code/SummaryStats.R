# ################################################################
# Author: Ashesh Rambachan
# Last updated: 1/1/2018
# Description: Computes summary statistics for outcomes and 
# key covariates for NELS
# ################################################################

# !ATTENTION!: Set working directory
setwd("~/Dropbox/Harvard/Algorithmic-Bias/NELS-AERPP");

# Loads the cleaned NELS dataset
# Set outcome variable in data_clean.R script!
source("code/data_clean.R");

# Outcome
mean(covar$y)
mean(covar$y[covar$black == 1])
mean(covar$y[covar$white == 1])

# Fraction female
mean(covar$female)
mean(covar$female[covar$black == 1]) 
mean(covar$female[covar$white == 1])

# F1 standardized test scores
mean(covar$F12XRSTD[covar$F12XRSTDm == 0 & covar$F12XRSTDnc == 0])
sd(covar$F12XRSTD[covar$F12XRSTDm == 0 & covar$F12XRSTDnc == 0])
mean(covar$F12XRSTD[covar$F12XRSTDm == 0 & covar$F12XRSTDnc == 0 & covar$black == 1])
mean(covar$F12XRSTD[covar$F12XRSTDm == 0 & covar$F12XRSTDnc == 0 & covar$white == 1])

mean(covar$F12XMSTD[covar$F12XMSTDm == 0 & covar$F12XMSTDnc == 0])
sd(covar$F12XMSTD[covar$F12XMSTDm == 0 & covar$F12XMSTDnc == 0])
mean(covar$F12XMSTD[covar$F12XMSTDm == 0 & covar$F12XMSTDnc == 0 & covar$black == 1])
mean(covar$F12XMSTD[covar$F12XMSTDm == 0 & covar$F12XMSTDnc == 0 & covar$white == 1])

mean(covar$F12XSSTD[covar$F12XSSTDm == 0 & covar$F12XSSTDnc == 0])
sd(covar$F12XSSTD[covar$F12XSSTDm == 0 & covar$F12XSSTDnc == 0])
mean(covar$F12XSSTD[covar$F12XSSTDm == 0 & covar$F12XSSTDnc == 0 & covar$black == 1])
mean(covar$F12XSSTD[covar$F12XSSTDm == 0 & covar$F12XSSTDnc == 0 & covar$white == 1])

mean(covar$F12XHSTD[covar$F12XHSTDm == 0 & covar$F12XHSTDnc == 0])
sd(covar$F12XHSTD[covar$F12XHSTDm == 0 & covar$F12XHSTDnc == 0])
mean(covar$F12XHSTD[covar$F12XHSTDm == 0 & covar$F12XHSTDnc == 0 & covar$black == 1])
mean(covar$F12XHSTD[covar$F12XHSTDm == 0 & covar$F12XHSTDnc == 0 & covar$white == 1])

# F1 grades
mean(covar$G9math2)
mean(covar$G9math2[covar$black == 1])
mean(covar$G9math2[covar$white == 1])

mean(covar$G9eng2)
mean(covar$G9eng2[covar$black == 1])
mean(covar$G9eng2[covar$white == 1])

mean(covar$G9hist2)
mean(covar$G9hist2[covar$black == 1])
mean(covar$G9hist2[covar$white == 1])

mean(covar$G9sci2)
mean(covar$G9sci2[covar$black == 1])
mean(covar$G9sci2[covar$white == 1])

# F2 standardized test scores
mean(covar$F22XRSTD[covar$F22XRSTDm == 0 & covar$F22XRSTDnc == 0])
sd(covar$F22XRSTD[covar$F22XRSTDm == 0 & covar$F22XRSTDnc == 0])
mean(covar$F22XRSTD[covar$F22XRSTDm == 0 & covar$F22XRSTDnc == 0 & covar$black == 1])
mean(covar$F22XRSTD[covar$F22XRSTDm == 0 & covar$F22XRSTDnc == 0 & covar$white == 1])

mean(covar$F22XMSTD[covar$F22XMSTDm == 0 & covar$F22XMSTDnc == 0])
sd(covar$F22XMSTD[covar$F22XMSTDm == 0 & covar$F22XMSTDnc == 0])
mean(covar$F22XMSTD[covar$F22XMSTDm == 0 & covar$F22XMSTDnc == 0 & covar$black == 1])
mean(covar$F22XMSTD[covar$F22XMSTDm == 0 & covar$F22XMSTDnc == 0 & covar$white == 1])

mean(covar$F22XSSTD[covar$F22XSSTDm == 0 & covar$F22XSSTDnc == 0])
sd(covar$F22XSSTD[covar$F22XSSTDm == 0 & covar$F22XSSTDnc == 0])
mean(covar$F22XSSTD[covar$F22XSSTDm == 0 & covar$F22XSSTDnc == 0 & covar$black == 1])
mean(covar$F22XSSTD[covar$F22XSSTDm == 0 & covar$F22XSSTDnc == 0 & covar$white == 1])

mean(covar$F22XHSTD[covar$F22XHSTDm == 0 & covar$F22XHSTDnc == 0])
sd(covar$F22XHSTD[covar$F22XHSTDm == 0 & covar$F22XHSTDnc == 0])
mean(covar$F22XHSTD[covar$F22XHSTDm == 0 & covar$F22XHSTDnc == 0 & covar$black == 1])
mean(covar$F22XHSTD[covar$F22XHSTDm == 0 & covar$F22XHSTDnc == 0 & covar$white == 1])

# F2 average grades in english, math, science and social studies
mean(covar$F2RHENG2[covar$F2RHENG2m == 0 & covar$F2RHENG2nc == 0])
mean(covar$F2RHENG2[covar$F2RHENG2m == 0 & covar$F2RHENG2nc == 0 & covar$black == 1])
mean(covar$F2RHENG2[covar$F2RHENG2m == 0 & covar$F2RHENG2nc == 0 & covar$white == 1])

mean(covar$F2RHMAG2[covar$F2RHMAG2m == 0 & covar$F2RHMAG2nc == 0])
mean(covar$F2RHMAG2[covar$F2RHMAG2m == 0 & covar$F2RHMAG2nc == 0 & covar$black == 1])
mean(covar$F2RHMAG2[covar$F2RHMAG2m == 0 & covar$F2RHMAG2nc == 0 & covar$white == 1])

mean(covar$F2RHSCG2[covar$F2RHSCG2m == 0 & covar$F2RHSCG2nc == 0])
mean(covar$F2RHSCG2[covar$F2RHSCG2m == 0 & covar$F2RHSCG2nc == 0 & covar$black == 1])
mean(covar$F2RHSCG2[covar$F2RHSCG2m == 0 & covar$F2RHSCG2nc == 0 & covar$white == 1])

mean(covar$F2RHSOG2[covar$F2RHSOG2m == 0 & covar$F2RHSOG2nc == 0])
mean(covar$F2RHSOG2[covar$F2RHSOG2m == 0 & covar$F2RHSOG2nc == 0 & covar$black == 1])
mean(covar$F2RHSOG2[covar$F2RHSOG2m == 0 & covar$F2RHSOG2nc == 0 & covar$white == 1])

# Follow-up 2: Credits in English, Math, Science and social studies
mean(covar$F2RENG_C[covar$F2RENG_Cnc == 0])
mean(covar$F2RENG_C[covar$F2RENG_Cnc == 0 & covar$black == 1])
mean(covar$F2RENG_C[covar$F2RENG_Cnc == 0 & covar$white == 1])

mean(covar$F2RMAT_C[covar$F2RMAT_Cnc == 0])
mean(covar$F2RMAT_C[covar$F2RMAT_Cnc == 0 & covar$black == 1])
mean(covar$F2RMAT_C[covar$F2RMAT_Cnc == 0 & covar$white == 1])

mean(covar$F2RSCI_C[covar$F2RSCI_Cnc == 0])
mean(covar$F2RSCI_C[covar$F2RSCI_Cnc == 0 & covar$black == 1])
mean(covar$F2RSCI_C[covar$F2RSCI_Cnc == 0 & covar$white == 1])

mean(covar$F2RSOC_C[covar$F2RSOC_Cnc == 0])
mean(covar$F2RSOC_C[covar$F2RSOC_Cnc == 0 & covar$black == 1])
mean(covar$F2RSOC_C[covar$F2RSOC_Cnc == 0 & covar$white == 1])

# Follow-up 1: Time spent on extra-curriculars
mean(covar$F1exhr0)
mean(covar$F1exhr0[covar$black == 1])
mean(covar$F1exhr0[covar$white == 1])

mean(covar$F1exhr1)
mean(covar$F1exhr1[covar$black == 1])
mean(covar$F1exhr1[covar$white == 1])

mean(covar$F1exhr2)
mean(covar$F1exhr2[covar$black == 1])
mean(covar$F1exhr2[covar$white == 1])

mean(covar$F1exhr3)
mean(covar$F1exhr3[covar$black == 1])
mean(covar$F1exhr3[covar$white == 1])

mean(covar$F1exhr4)
mean(covar$F1exhr4[covar$black == 1])
mean(covar$F1exhr4[covar$white == 1])

mean(covar$F1exhr5)
mean(covar$F1exhr5[covar$black == 1])
mean(covar$F1exhr5[covar$white == 1])


# Follow-up 2: Time spent on extra-curriculars
mean(covar$F2exhr0)
mean(covar$F2exhr0[covar$black == 1])
mean(covar$F2exhr0[covar$white == 1])

mean(covar$F2exhr1)
mean(covar$F2exhr1[covar$black == 1])
mean(covar$F2exhr1[covar$white == 1])

mean(covar$F2exhr2)
mean(covar$F2exhr2[covar$black == 1])
mean(covar$F2exhr2[covar$white == 1])

mean(covar$F2exhr3)
mean(covar$F2exhr3[covar$black == 1])
mean(covar$F2exhr3[covar$white == 1])

mean(covar$F2exhr4)
mean(covar$F2exhr4[covar$black == 1])
mean(covar$F2exhr4[covar$white == 1])

mean(covar$F2exhr5)
mean(covar$F2exhr5[covar$black == 1])
mean(covar$F2exhr5[covar$white == 1])

mean(covar$F2exhr6)
mean(covar$F2exhr6[covar$black == 1])
mean(covar$F2exhr6[covar$white == 1])

mean(covar$F2exhr7)
mean(covar$F2exhr7[covar$black == 1])
mean(covar$F2exhr7[covar$white == 1])

# Fraction took SAT and ACT
mean(covar$SAT_yes)
mean(covar$SAT_yes[covar$black == 1])
mean(covar$SAT_yes[covar$white == 1])

mean(covar$ACT_yes)
mean(covar$ACT_yes[covar$black == 1])
mean(covar$ACT_yes[covar$white == 1])

# sample sizes
sum(covar$black)
sum(covar$white)

# full set of summary statistcs
fs_mean = colMeans(covar)
black_mean = colMeans(covar[covar$black == 1])
white_mean = colMeans(covar[covar$white == 1])
sumstats = t(cbind(fs_mean, white_mean, black_mean))
write.table(sumstats, file = "sumstats.csv", sep = ",")
