# ###################################################
# Author: Ashesh Rambachan
# Last Updated: 12/5/2017
# This script is a helper script for data_clean.R. 
# It cleans the FU2 academic covariates
# ###################################################

# Reading standardized test scores
F22XRSTDm = ifelse(NELS$F22XRSTD == 99.98, 1, 0); # missing indicator
F22XRSTDnc = ifelse(NELS$F22XRSTD == 99.99, 1, 0); # not completed indicator
NELS$F22XRSTD[NELS$F22XRSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F22XRSTD[NELS$F22XRSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F22XRSTDm, F22XRSTDnc);
rm(F22XRSTDm, F22XRSTDnc);

# Math standardized test scores
F22XMSTDm = ifelse(NELS$F22XMSTD == 99.98, 1, 0); # missing indicator
F22XMSTDnc = ifelse(NELS$F22XMSTD == 99.99, 1, 0); # not completed indicator
NELS$F22XMSTD[NELS$F22XMSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F22XMSTD[NELS$F22XMSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F22XMSTDm, F22XMSTDnc);
rm(F22XMSTDm, F22XMSTDnc);

# Science standardized test scores
F22XSSTDm = ifelse(NELS$F22XSSTD == 99.98, 1, 0); # missing indicator
F22XSSTDnc = ifelse(NELS$F22XSSTD == 99.99, 1, 0); # not completed indicator
NELS$F22XSSTD[NELS$F22XSSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F22XSSTD[NELS$F22XSSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F22XSSTDm, F22XSSTDnc);
rm(F22XSSTDm, F22XSSTDnc);

# History standardized test scores
F22XHSTDm = ifelse(NELS$F22XHSTD == 99.98, 1, 0); # missing indicator
F22XHSTDnc = ifelse(NELS$F22XHSTD == 99.99, 1, 0); # not completed indicator
NELS$F22XHSTD[NELS$F22XHSTD == 99.98] <- -2; # set score to -2 if missing
NELS$F22XHSTD[NELS$F22XHSTD == 99.99] <- -1; # set score to -1 if not completed
NELS <- cbind(NELS, F22XHSTDm, F22XHSTDnc);
rm(F22XHSTDm, F22XHSTDnc);

# Reading proficiency: Base case = below level 1
F2RPROm = ifelse(NELS$F22XRPRO == 8, 1, 0); # missing indicator
F2RPROnc = ifelse(NELS$F22XRPRO == 9, 1, 0); # not completed indicator
F2RPRO1 = ifelse(NELS$F22XRPRO == 1, 1, 0);
F2RPRO2 = ifelse(NELS$F22XRPRO == 2, 1, 0);
F2RPRO3 = ifelse(NELS$F22XRPRO == 3, 1, 0);
NELS <- cbind(NELS, F2RPROm, F2RPROnc, F2RPRO1, F2RPRO2, F2RPRO3);
rm(F2RPROm, F2RPROnc, F2RPRO1, F2RPRO2, F2RPRO3);

# Math proficiency: Base case = below level 1
F2MPROm = ifelse(NELS$F22XMPRO == 8, 1, 0); # missing indicator
F2MPROnc = ifelse(NELS$F22XMPRO == 9, 1, 0); # not completed indicator
F2MPRO1 = ifelse(NELS$F22XMPRO == 1, 1, 0);
F2MPRO2 = ifelse(NELS$F22XMPRO == 2, 1, 0);
F2MPRO3 = ifelse(NELS$F22XMPRO == 3, 1, 0);
F2MPRO4 = ifelse(NELS$F22XMPRO == 4, 1, 0);
F2MPRO5 = ifelse(NELS$F22XMPRO == 5, 1, 0);
NELS <- cbind(NELS, F2MPROm, F2MPROnc, F2MPRO1, F2MPRO2, F2MPRO3, F2MPRO4, F2MPRO5);
rm(F2MPROm, F2MPROnc, F2MPRO1, F2MPRO2, F2MPRO3, F2MPRO4, F2MPRO5);

# Science proficiency: Base case = below level 1
F2SPROm = ifelse(NELS$F22XSPRO == 8, 1, 0); # missing indicator
F2SPROnc = ifelse(NELS$F22XSPRO == 9, 1, 0); # not completed indicator
F2SPRO1 = ifelse(NELS$F22XSPRO == 1, 1, 0);
F2SPRO2 = ifelse(NELS$F22XSPRO == 2, 1, 0);
F2SPRO3 = ifelse(NELS$F22XSPRO == 3, 1, 0);
NELS <- cbind(NELS, F2SPROm, F2SPROnc, F2SPRO1, F2SPRO2, F2SPRO3);
rm(F2SPROm, F2SPROnc, F2SPRO1, F2SPRO2, F2SPRO3);

NELS <- subset(NELS, select = -c(F22XSPRO, F22XMPRO, F22XRPRO));

# Transcript indic. High School Program: Base case = Legit skip
F2RTR1 = ifelse(NELS$F2RTRPRG == 1, 1, 0);
F2RTR2 = ifelse(NELS$F2RTRPRG == 2, 1, 0);
F2RTR3 = ifelse(NELS$F2RTRPRG == 3, 1, 0);
F2RTR4 = ifelse(NELS$F2RTRPRG == 4, 1, 0);
F2RTR5 = ifelse(NELS$F2RTRPRG == 5, 1, 0);
F2RTR6 = ifelse(NELS$F2RTRPRG == 6, 1, 0);
NELS <- cbind(NELS, F2RTR1, F2RTR2, F2RTR3, F2RTR4, F2RTR5, F2RTR6);
rm(F2RTR1, F2RTR2, F2RTR3, F2RTR4, F2RTR5, F2RTR6);
NELS <- subset(NELS, select = -c(F2RTRPRG))

# Average grades: No credit to 0, legit skip set to -1
# English
NELS$F2RHENG2[NELS$F2RHENG2 == 99.98] = 0; # No credit set to 0
NELS$F2RHENG2[NELS$F2RHENG2 == 99.99] = -1; # legit skip set to -1
F2RHENG2m = ifelse(NELS$F2RHENG2 == 99.98, 1, 0) # missing indicator
F2RHENG2nc = ifelse(NELS$F2RHENG2 == 99.99, 1, 0) # legit skip indicator
NELS <- cbind(NELS, F2RHENG2m, F2RHENG2nc);
rm(F2RHENG2m, F2RHENG2nc);

# Math
NELS$F2RHMAG2[NELS$F2RHMAG2 == 99.98] = 0; # No credit set to 0
NELS$F2RHMAG2[NELS$F2RHMAG2 == 99.99] = -1; # legit skip set to -1
F2RHMAG2m = ifelse(NELS$F2RHMAG2 == 99.98, 1, 0) # missing indicator
F2RHMAG2nc = ifelse(NELS$F2RHMAG2 == 99.99, 1, 0) # legit skip indicator
NELS <- cbind(NELS, F2RHMAG2m, F2RHMAG2nc);
rm(F2RHMAG2m, F2RHMAG2nc);

# Science
NELS$F2RHSCG2[NELS$F2RHSCG2 == 99.98] = 0; # No credit set to 0
NELS$F2RHSCG2[NELS$F2RHSCG2 == 99.99] = -1; # legit skip set to -1
F2RHSCG2m = ifelse(NELS$F2RHSCG2 == 99.98, 1, 0) # missing indicator
F2RHSCG2nc = ifelse(NELS$F2RHSCG2 == 99.99, 1, 0) # legit skip indicator
NELS <- cbind(NELS, F2RHSCG2m, F2RHSCG2nc);
rm(F2RHSCG2m, F2RHSCG2nc);

# Social Studies
NELS$F2RHSOG2[NELS$F2RHSOG2 == 99.98] = 0; # No credit set to 0
NELS$F2RHSOG2[NELS$F2RHSOG2 == 99.99] = -1; # legit skip set to -1
F2RHSOG2m = ifelse(NELS$F2RHSOG2 == 99.98, 1, 0) # missing indicator
F2RHSOG2nc = ifelse(NELS$F2RHSOG2 == 99.99, 1, 0) # legit skip indicator
NELS <- cbind(NELS, F2RHSOG2m, F2RHSOG2nc);
rm(F2RHSOG2m, F2RHSOG2nc);

# Computer Science
NELS$F2RHCOG2[NELS$F2RHCOG2 == 99.98] = 0; # No credit set to 0
NELS$F2RHCOG2[NELS$F2RHCOG2 == 99.99] = -1; # legit skip set to -1
F2RHCOG2m = ifelse(NELS$F2RHCOG2 == 99.98, 1, 0) # missing indicator
F2RHCOG2nc = ifelse(NELS$F2RHCOG2 == 99.99, 1, 0) # legit skip indicator
NELS <- cbind(NELS, F2RHCOG2m, F2RHCOG2nc);
rm(F2RHCOG2m, F2RHCOG2nc);

# Foreign language
NELS$F2RHFOG2[NELS$F2RHFOG2 == 99.98] = 0; # No credit set to 0
NELS$F2RHFOG2[NELS$F2RHFOG2 == 99.99] = -1; # legit skip set to -1
F2RHFOG2m = ifelse(NELS$F2RHFOG2 == 99.98, 1, 0) # missing indicator
F2RHFOG2nc = ifelse(NELS$F2RHFOG2 == 99.99, 1, 0) # legit skip indicator
NELS <- cbind(NELS, F2RHFOG2m, F2RHFOG2nc);
rm(F2RHFOG2m, F2RHFOG2nc);

# Credit units: Legit skip set to -1
# English
NELS$F2RENG_C[NELS$F2RENG_C == 99.99] = -1; # legit skip set to -1
F2RENG_Cnc = ifelse(NELS$F2RENG_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RENG_Cnc);
rm(F2RENG_Cnc);

# Foreign Language
NELS$F2RFOR_C[NELS$F2RFOR_C == 99.99] = -1; # legit skip set to -1
F2RFOR_Cnc = ifelse(NELS$F2RFOR_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RFOR_Cnc);
rm(F2RFOR_Cnc);

# Math
NELS$F2RMAT_C[NELS$F2RMAT_C == 9.99] = 0; # legit skip set to -1
F2RMAT_Cnc = ifelse(NELS$F2RMAT_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RMAT_Cnc);
rm(F2RMAT_Cnc);

# Algebra 1
NELS$F2RAL1_C[NELS$F2RAL1_C == 9.99] = -1; # legit skip set to -1
F2RAL1_Cnc = ifelse(NELS$F2RAL1_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RAL1_Cnc);
rm(F2RAL1_Cnc);

# Algebra 2
NELS$F2RAL2_C[NELS$F2RAL2_C == 9.99] = -1; # legit skip set to -1
F2RAL2_Cnc = ifelse(NELS$F2RAL2_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RAL2_Cnc);
rm(F2RAL2_Cnc);

# Geometry
NELS$F2RGEO_C[NELS$F2RGEO_C == 9.99] = -1; # legit skip set to -1
F2RGEO_Cnc = ifelse(NELS$F2RGEO_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RGEO_Cnc);
rm(F2RGEO_Cnc);

# Trigonometry
NELS$F2RTRI_C[NELS$F2RTRI_C == 9.99] = -1; # legit skip set to -1
F2RTRI_Cnc = ifelse(NELS$F2RTRI_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RTRI_Cnc);
rm(F2RTRI_Cnc);

# Pre-calculus
NELS$F2RPRE_C[NELS$F2RPRE_C == 9.99] = -1; # legit skip set to -1
F2RPRE_Cnc = ifelse(NELS$F2RPRE_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RPRE_Cnc);
rm(F2RPRE_Cnc);

# Calculus
NELS$F2RCAL_C[NELS$F2RCAL_C == 9.99] = -1; # legit skip set to -1
F2RCAL_Cnc = ifelse(NELS$F2RCAL_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RCAL_Cnc);
rm(F2RCAL_Cnc);

# Other math courses
NELS$F2ROMA_C[NELS$F2ROMA_C == 9.99] = -1; # legit skip set to -1
F2ROMA_Cnc = ifelse(NELS$F2ROMA_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2ROMA_Cnc);
rm(F2ROMA_Cnc);

# Science
NELS$F2RSCI_C[NELS$F2RSCI_C == 99.99] = -1; # legit skip set to -1
F2RSCI_Cnc = ifelse(NELS$F2RSCI_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RSCI_Cnc);
rm(F2RSCI_Cnc);

# Earth Science
NELS$F2REAR_C[NELS$F2REAR_C == 9.99] = -1; # legit skip set to -1
F2REAR_Cnc = ifelse(NELS$F2REAR_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2REAR_Cnc);
rm(F2REAR_Cnc);

# Biology
NELS$F2RBIO_C[NELS$F2RBIO_C == 9.99] = -1; # legit skip set to -1
F2RBIO_Cnc = ifelse(NELS$F2RBIO_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RBIO_Cnc);
rm(F2RBIO_Cnc);

# Chemistry
NELS$F2RCHE_C[NELS$F2RCHE_C == 9.99] = -1; # legit skip set to -1
F2RCHE_Cnc = ifelse(NELS$F2RCHE_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RCHE_Cnc);
rm(F2RCHE_Cnc);

# Physics
NELS$F2RPHY_C[NELS$F2RPHY_C == 9.99] = -1; # legit skip set to -1
F2RPHY_Cnc = ifelse(NELS$F2RPHY_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RPHY_Cnc);
rm(F2RPHY_Cnc);

# Other science courses
NELS$F2ROSC_C[NELS$F2ROSC_C == 9.99] = -1; # legit skip set to -1
F2ROSC_Cnc = ifelse(NELS$F2ROSC_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2ROSC_Cnc);
rm(F2ROSC_Cnc);

# Social Studies
NELS$F2RSOC_C[NELS$F2RSOC_C == 99.99] = -1; # legit skip set to -1
F2RSOC_Cnc = ifelse(NELS$F2RSOC_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RSOC_Cnc);
rm(F2RSOC_Cnc);

# History
NELS$F2RHIS_C[NELS$F2RHIS_C == 9.99] = -1; # legit skip set to -1
F2RHIS_Cnc = ifelse(NELS$F2RHIS_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RHIS_Cnc);
rm(F2RHIS_Cnc);

# Other social studies courses
NELS$F2ROSO_C[NELS$F2ROSO_C == 9.99] = -1; # legit skip set to -1
F2ROSO_Cnc = ifelse(NELS$F2ROSO_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2ROSO_Cnc);
rm(F2ROSO_Cnc);

# Computer science
NELS$F2RCOM_C[NELS$F2RCOM_C == 9.99] = -1; # legit skip set to -1
F2RCOM_Cnc = ifelse(NELS$F2RCOM_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RCOM_Cnc);
rm(F2RCOM_Cnc);

# Agriculture
NELS$F2RVAG_C[NELS$F2RVAG_C == 99.99] = -1; # legit skip set to -1
F2RVAG_Cnc = ifelse(NELS$F2RVAG_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVAG_Cnc);
rm(F2RVAG_Cnc);

# Business
NELS$F2RVBU_C[NELS$F2RVBU_C == 99.99] = -1; # legit skip set to -1
F2RVBU_Cnc = ifelse(NELS$F2RVBU_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVBU_Cnc);
rm(F2RVBU_Cnc);

# General introductory vocational courses
NELS$F2RVGN_C[NELS$F2RVGN_C == 9.99] = -1; # legit skip set to -1
F2RVGN_Cnc = ifelse(NELS$F2RVGN_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVGN_Cnc);
rm(F2RVGN_Cnc);

# Health/Human Resources 
NELS$F2RVHE_C[NELS$F2RVHE_C == 99.99] = -1; # legit skip set to -1
F2RVHE_Cnc = ifelse(NELS$F2RVHE_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVHE_Cnc);
rm(F2RVHE_Cnc);

# Vocational home economics
NELS$F2RVHO_C[NELS$F2RVHO_C == 99.99] = -1; # legit skip set to -1
F2RVHO_Cnc = ifelse(NELS$F2RVHO_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVHO_Cnc);
rm(F2RVHO_Cnc);

# Marketing/Distribution
NELS$F2RVMA_C[NELS$F2RVMA_C == 9.99] = -1; # legit skip set to -1
F2RVMA_Cnc = ifelse(NELS$F2RVMA_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVMA_Cnc);
rm(F2RVMA_Cnc);

# Technical
NELS$F2RVTE_C[NELS$F2RVTE_C == 9.99] = -1; # legit skip set to -1
F2RVTE_Cnc = ifelse(NELS$F2RVTE_C == 9.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVTE_Cnc);
rm(F2RVTE_Cnc);

# Trade/Industry
NELS$F2RVTR_C[NELS$F2RVTR_C == 99.99] = -1; # legit skip set to -1
F2RVTR_Cnc = ifelse(NELS$F2RVTR_C == 99.99, 1, 0); # Legit skip indicator
NELS <- cbind(NELS, F2RVTR_Cnc);
rm(F2RVTR_Cnc);