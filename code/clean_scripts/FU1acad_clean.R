# ###################################################
# Author: Ashesh Rambachan
# Last Updated: 12/5/2017
# This script is a helper script for data_clean.R. 
# It cleans the FY1 academic covariates
# ###################################################

# Grades in math, english, history, science
# Procedure: Base case = multiple responses, refusal, legit skip or class not graded
# code as zero.

# Math
G9math1 = ifelse(NELS$F1S39A == 1, 1, 0);
G9math2 = ifelse(NELS$F1S39A == 2, 1, 0);
G9math3 = ifelse(NELS$F1S39A == 3, 1, 0);
G9math4 = ifelse(NELS$F1S39A == 4, 1, 0);
G9math5 = ifelse(NELS$F1S39A == 5, 1, 0);
G9math6 = ifelse(NELS$F1S39A == 6, 1, 0);
G9math7 = ifelse(NELS$F1S39A == 7, 1, 0);
G9math8 = ifelse(NELS$F1S39A == 8, 1, 0);
G9math9 = ifelse(NELS$F1S39A == 9, 1, 0);
G9mathm = ifelse(NELS$F1S39A == 98, 1, 0); # missing indicator
NELS <- cbind(NELS, G9math1, G9math2, G9math3, G9math4, G9math5, G9math6, G9math7, 
              G9math8, G9math9, G9mathm);
rm(G9math1, G9math2, G9math3, G9math4, G9math5, G9math6, G9math7, G9math8, G9math9, G9mathm);

# English
G9eng1 = ifelse(NELS$F1S39B == 1, 1, 0);
G9eng2 = ifelse(NELS$F1S39B == 2, 1, 0);
G9eng3 = ifelse(NELS$F1S39B == 3, 1, 0);
G9eng4 = ifelse(NELS$F1S39B == 4, 1, 0);
G9eng5 = ifelse(NELS$F1S39B == 5, 1, 0);
G9eng6 = ifelse(NELS$F1S39B == 6, 1, 0);
G9eng7 = ifelse(NELS$F1S39B == 7, 1, 0);
G9eng8 = ifelse(NELS$F1S39B == 8, 1, 0);
G9eng9 = ifelse(NELS$F1S39B == 9, 1, 0);
G9engm = ifelse(NELS$F1S39B == 98, 1, 0);
NELS <- cbind(NELS, G9eng1, G9eng2, G9eng3, G9eng4, G9eng5, G9eng6, 
              G9eng7, G9eng8, G9eng9, G9engm);
rm(G9eng1, G9eng2, G9eng3, G9eng4, G9eng5, G9eng6, G9eng7, G9eng8, G9eng9, G9engm);

# History
G9hist1 = ifelse(NELS$F1S39C == 1, 1, 0);
G9hist2 = ifelse(NELS$F1S39C == 2, 1, 0);
G9hist3 = ifelse(NELS$F1S39C == 3, 1, 0);
G9hist4 = ifelse(NELS$F1S39C == 4, 1, 0);
G9hist5 = ifelse(NELS$F1S39C == 5, 1, 0);
G9hist6 = ifelse(NELS$F1S39C == 6, 1, 0);
G9hist7 = ifelse(NELS$F1S39C == 7, 1, 0);
G9hist8 = ifelse(NELS$F1S39C == 8, 1, 0);
G9hist9 = ifelse(NELS$F1S39C == 9, 1, 0);
G9histm = ifelse(NELS$F1S39C == 98, 1, 0);
NELS <- cbind(NELS, G9hist1, G9hist2, G9hist3, G9hist4, G9hist5, G9hist6, 
              G9hist7, G9hist8, G9hist9, G9histm);
rm(G9hist1, G9hist2, G9hist3, G9hist4, G9hist5, G9hist6, G9hist7, G9hist8, G9hist9, G9histm);

# Science
G9sci1 = ifelse(NELS$F1S39D == 1, 1, 0);
G9sci2 = ifelse(NELS$F1S39D == 2, 1, 0);
G9sci3 = ifelse(NELS$F1S39D == 3, 1, 0);
G9sci4 = ifelse(NELS$F1S39D == 4, 1, 0);
G9sci5 = ifelse(NELS$F1S39D == 5, 1, 0);
G9sci6 = ifelse(NELS$F1S39D == 6, 1, 0);
G9sci7 = ifelse(NELS$F1S39D == 7, 1, 0);
G9sci8 = ifelse(NELS$F1S39D == 8, 1, 0);
G9sci9 = ifelse(NELS$F1S39D == 9, 1, 0);
G9scim = ifelse(NELS$F1S39D == 98, 1, 0);
NELS <- cbind(NELS, G9sci1, G9sci2, G9sci3, G9sci4, G9sci5, G9sci6, 
              G9sci7, G9sci8, G9sci9, G9scim);
rm(G9sci1, G9sci2, G9sci3, G9sci4, G9sci5, G9sci6, G9sci7, G9sci8, G9sci9, G9scim);

NELS <- subset(NELS, select = -c(F1S39A, F1S39B, F1S39C, F1S39D));

# Reading standardized test scores
F12XRSTDm = ifelse(NELS$F12XRSTD == 99.98, 1, 0); # missing indicator
F12XRSTDnc = ifelse(NELS$F12XRSTD == 99.99, 1, 0); # not completed indicator
NELS$F12XRSTD[NELS$F12XRSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F12XRSTD[NELS$F12XRSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F12XRSTDm, F12XRSTDnc);
rm(F12XRSTDm, F12XRSTDnc);

# Math standardized test scores
F12XMSTDm = ifelse(NELS$F12XMSTD == 99.98, 1, 0); # missing indicator
F12XMSTDnc = ifelse(NELS$F12XMSTD == 99.99, 1, 0); # not completed indicator
NELS$F12XMSTD[NELS$F12XMSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F12XMSTD[NELS$F12XMSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F12XMSTDm, F12XMSTDnc);
rm(F12XMSTDm, F12XMSTDnc);

# Science standardized test scores
F12XSSTDm = ifelse(NELS$F12XSSTD == 99.98, 1, 0); # missing indicator
F12XSSTDnc = ifelse(NELS$F12XSSTD == 99.99, 1, 0); # not completed indicator
NELS$F12XSSTD[NELS$F12XSSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F12XSSTD[NELS$F12XSSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F12XSSTDm, F12XSSTDnc);
rm(F12XSSTDm, F12XSSTDnc);

# History standardized test scores
F12XHSTDm = ifelse(NELS$F12XHSTD == 99.98, 1, 0); # missing indicator
F12XHSTDnc = ifelse(NELS$F12XHSTD == 99.99, 1, 0); # not completed indicator
NELS$F12XHSTD[NELS$F12XHSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F12XHSTD[NELS$F12XHSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F12XHSTDm, F12XHSTDnc);
rm(F12XHSTDm, F12XHSTDnc);

# Reading proficiency: Base case = below level 1
F1RPROm = ifelse(NELS$F12XRPRO == 8, 1, 0); # missing indicator
F1RPROnc = ifelse(NELS$F12XRPRO == 9, 1, 0); # not completed indicator
F1RPRO1 = ifelse(NELS$F12XRPRO == 1, 1, 0);
F1RPRO2 = ifelse(NELS$F12XRPRO == 2, 1, 0);
NELS <- cbind(NELS, F1RPROm, F1RPROnc, F1RPRO1, F1RPRO2);
rm(F1RPROm, F1RPROnc, F1RPRO1, F1RPRO2);

# Math proficiency: Base case = below level 1
F1MPROm = ifelse(NELS$F12XMPRO == 8, 1, 0); # missing indicator
F1MPROnc = ifelse(NELS$F12XMPRO == 9, 1, 0); # not completed indicator
F1MPRO1 = ifelse(NELS$F12XMPRO == 1, 1, 0);
F1MPRO2 = ifelse(NELS$F12XMPRO == 2, 1, 0);
F1MPRO3 = ifelse(NELS$F12XMPRO == 3, 1, 0);
F1MPRO4 = ifelse(NELS$F12XMPRO == 4, 1, 0);
NELS <- cbind(NELS, F1MPROm, F1MPROnc, F1MPRO1, F1MPRO2, F1MPRO3, F1MPRO4);
rm(F1MPROm, F1MPROnc, F1MPRO1, F1MPRO2, F1MPRO3, F1MPRO4);

# Science proficiency: Base case = below level 1
F1SPROm = ifelse(NELS$F12XSPRO == 8, 1, 0); # missing indicator
F1SPROnc = ifelse(NELS$F12XSPRO == 9, 1, 0); # not completed indicator
F1SPRO1 = ifelse(NELS$F12XSPRO == 1, 1, 0);
F1SPRO2 = ifelse(NELS$F12XSPRO == 2, 1, 0);
F1SPRO3 = ifelse(NELS$F12XSPRO == 3, 1, 0);
NELS <- cbind(NELS, F1SPROm, F1SPROnc, F1SPRO1, F1SPRO2, F1SPRO3);
rm(F1SPROm, F1SPROnc, F1SPRO1, F1SPRO2, F1SPRO3);

NELS <- subset(NELS, select = -c(F12XSPRO, F12XMPRO, F12XRPRO));