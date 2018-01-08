# ####################################################################
# Author: Ashesh Rambachan
# Last updated: December 6, 2017
# Description: This script generates the covariate matrix used
# for the estimation of the random forest and the regularized
# logit models.
# ####################################################################

##################################
# Builds dataframe of covariates #
##################################
# Demographic and School District Variables
female = NELS$female
urb_dist = NELS$urb_distt
rur_dist = NELS$rur_distt
m_dist = NELS$m_distt
covar = cbind(female, urb_dist, rur_dist, m_dist)
rm(female, urb_dist, rur_dist, m_dist)

reg_dist = subset(NELS, select = grep("reg_dist", names(NELS)))
dist_type = subset(NELS, select = grep("dist_type2", names(NELS)))
G10size = subset(NELS, select = grep("G10size", names(NELS)))
ssize = subset(NELS, select = grep("ssize", names(NELS)))

covar = cbind(covar, reg_dist, dist_type, G10size, ssize);
rm(reg_dist, dist_type, G10size, ssize);

# F1 academic variables
G9math = subset(NELS, select = grep("G9math", names(NELS)))
G9eng = subset(NELS, select = grep("G9eng", names(NELS)))
G9hist = subset(NELS, select = grep("G9hist", names(NELS)))
G9sci = subset(NELS, select = grep("G9sci", names(NELS)))
F12XRSTD = subset(NELS, select = grep("F12XRSTD", names(NELS)))
F12XMSTD = subset(NELS, select = grep("F12XMSTD", names(NELS)))
F12XSSTD = subset(NELS, select = grep("F12XSSTD", names(NELS)))
F12XHSTD = subset(NELS, select = grep("F12XHSTD", names(NELS)))
F1RPRO = subset(NELS, select = grep("F1RPRO", names(NELS)))
F1MPRO = subset(NELS, select = grep("F1MPRO", names(NELS)))
F1SPRO = subset(NELS, select = grep("F1SPRO", names(NELS)))

covar = cbind(covar, G9math, G9eng, G9hist, G9sci, 
              F12XRSTD, F12XMSTD, F12XSSTD, F12XHSTD, F1RPRO, F1MPRO, F1SPRO)
rm(G9math, G9eng, G9hist, G9sci, 
   F12XRSTD, F12XMSTD, F12XSSTD, F12XHSTD, F1RPRO, F1MPRO, F1SPRO)

# F2 academic variables
F22XRSTD = subset(NELS, select = grep("F22XRSTD", names(NELS))) 
F22XMSTD = subset(NELS, select = grep("F22XMSTD", names(NELS)))
F22XSSTD = subset(NELS, select = grep("F22XSSTD", names(NELS)))
F22XHSTD = subset(NELS, select = grep("F22XHSTD", names(NELS)))
F2RPRO = subset(NELS, select = grep("F2RPRO", names(NELS)))
F2MPRO = subset(NELS, select = grep("F2MPRO", names(NELS)))
F2SPRO = subset(NELS, select = grep("F2SPRO", names(NELS)))
F2RTR = subset(NELS, select = grep("F2RTR", names(NELS)))
F2RHENG = subset(NELS, select = grep("F2RHENG", names(NELS)))
F2RHMAG = subset(NELS, select = grep("F2RHMAG", names(NELS)))
F2RHSCG = subset(NELS, select = grep("F2RHSCG", names(NELS)))
F2RHSOG = subset(NELS, select = grep("F2RHSOG", names(NELS)))
F2RHCOG = subset(NELS, select = grep("F2RHCOG", names(NELS)))
F2RHFOG = subset(NELS, select = grep("F2RHFOG", names(NELS)))
F2RENG_C = subset(NELS, select = grep("F2RENG_C", names(NELS)))
F2RFOR_C = subset(NELS, select = grep("F2RFOR_C", names(NELS)))
F2RMAT_C = subset(NELS, select = grep("F2RMAT_C", names(NELS)))
F2RAL1_C = subset(NELS, select = grep("F2RAL1_C", names(NELS)))
F2RAL2_C = subset(NELS, select = grep("F2RAL2_C", names(NELS)))
F2RGEO_C = subset(NELS, select = grep("F2RGEO_C", names(NELS)))
F2RTRI_C = subset(NELS, select = grep("F2RTRI_C", names(NELS)))
F2RPRE_C = subset(NELS, select = grep("F2RPRE_C", names(NELS)))
F2RCAL_C = subset(NELS, select = grep("F2RCAL_C", names(NELS)))
F2ROMA_C = subset(NELS, select = grep("F2ROMA_C", names(NELS)))
F2RSCI_C = subset(NELS, select = grep("F2RSCI_C", names(NELS)))
F2REAR_C = subset(NELS, select = grep("F2REAR_C", names(NELS)))
F2RBIO_C = subset(NELS, select = grep("F2RBIO_C", names(NELS)))
F2RCHE_C = subset(NELS, select = grep("F2RCHE_C", names(NELS)))
F2RPHY_C = subset(NELS, select = grep("F2RPHY_C", names(NELS)))
F2ROSC_C = subset(NELS, select = grep("F2ROSC_C", names(NELS)))
F2RSOC_C = subset(NELS, select = grep("F2RSOC_C", names(NELS)))
F2RHIS_C = subset(NELS, select = grep("F2RHIS_C", names(NELS)))
F2ROSO_C = subset(NELS, select = grep("F2ROSO_C", names(NELS)))
F2RCOM_C = subset(NELS, select = grep("F2RCOM_C", names(NELS)))
F2RVAG_C = subset(NELS, select = grep("F2RVAG_C", names(NELS)))
F2RVBU_C = subset(NELS, select = grep("F2RVBU_C", names(NELS)))
F2RVGN_C = subset(NELS, select = grep("F2RVGN_C", names(NELS)))
F2RVHE_C = subset(NELS, select = grep("F2RVHE_C", names(NELS)))
F2RVHO_C = subset(NELS, select = grep("F2RVHO_C", names(NELS)))
F2RVMA_C = subset(NELS, select = grep("F2RVMA_C", names(NELS)))
F2RVTE_C = subset(NELS, select = grep("F2RVTE_C", names(NELS)))
F2RVTR_C = subset(NELS, select = grep("F2RENG_C", names(NELS)))

covar <- cbind(covar, F22XRSTD, F22XMSTD, F22XSSTD, 
               F22XHSTD, F2RPRO, F2MPRO, F2SPRO, F2RTR, 
               F2RHENG, F2RHMAG, F2RHSCG, F2RHSOG, F2RHCOG, 
               F2RHFOG, F2RENG_C, F2RFOR_C, F2RMAT_C, F2RAL1_C,
               F2RAL2_C, F2RGEO_C, F2RTRI_C, F2RPRE_C, F2RCAL_C, 
               F2ROMA_C, F2RSCI_C, F2REAR_C, F2RBIO_C, F2RCHE_C, 
               F2RPHY_C, F2ROSC_C, F2RSOC_C, F2RHIS_C, 
               F2ROSO_C, F2RCOM_C, F2RVAG_C, F2RVBU_C, F2RVGN_C, 
               F2RVHE_C, F2RVHO_C, F2RVMA_C, F2RVTE_C, F2RVTR_C 
)
rm(F22XRSTD, F22XMSTD, F22XSSTD, 
   F22XHSTD, F2RPRO, F2MPRO, F2SPRO, F2RTR, 
   F2RHENG, F2RHMAG, F2RHSCG, F2RHSOG, F2RHCOG, 
   F2RHFOG, F2RENG_C, F2RFOR_C, F2RMAT_C, F2RAL1_C,
   F2RAL2_C, F2RGEO_C, F2RTRI_C, F2RPRE_C, F2RCAL_C, 
   F2ROMA_C, F2RSCI_C, F2REAR_C, F2RBIO_C, F2RCHE_C, 
   F2RPHY_C, F2ROSC_C, F2RSOC_C, F2RHIS_C, 
   F2ROSO_C, F2RCOM_C, F2RVAG_C, F2RVBU_C, F2RVGN_C, 
   F2RVHE_C, F2RVHO_C, F2RVMA_C, F2RVTE_C, F2RVTR_C
)

# F3 academic variables
SAT_yes = NELS$SAT_yes
SAT_no = NELS$SAT_no
ACT_yes = NELS$ACT_yes
ACT_no = NELS$ACT_no
covar <- cbind(covar, SAT_yes, SAT_no, ACT_yes, ACT_no)

rm(SAT_yes, SAT_no, ACT_yes, ACT_no)

# F1 activities variables
F1exhr = subset(NELS, select = grep("F1exhr", names(NELS)))
F1baseball = subset(NELS, select = grep("F1baseball", names(NELS)))
F1basketball = subset(NELS, select = grep("F1basketball", names(NELS)))
F1football = subset(NELS, select = grep("F1football", names(NELS)))
F1soccer = subset(NELS, select = grep("F1soccer", names(NELS)))
F1swim = subset(NELS, select = grep("F1swim", names(NELS)))
F1other = subset(NELS, select = grep("F1other", names(NELS)))
F1ind = subset(NELS, select = grep("F1ind", names(NELS)))
F1cheer = subset(NELS, select = grep("F1cheer", names(NELS)))
F1drill = subset(NELS, select = grep("F1drill", names(NELS)))
F1band = subset(NELS, select = grep("F1band", names(NELS)))
F1play = subset(NELS, select = grep("F1play", names(NELS)))
F1govt = subset(NELS, select = grep("F1govt", names(NELS)))
F1honor = subset(NELS, select = grep("F1honor", names(NELS)))
F1news = subset(NELS, select = grep("F1news", names(NELS)))
F1serv = subset(NELS, select = grep("F1serv", names(NELS)))
F1acad = subset(NELS, select = grep("F1acad", names(NELS)))
F1hobby = subset(NELS, select = grep("F1hobby", names(NELS)))
F1FTA = subset(NELS, select = grep("F1FTA", names(NELS)))

covar <- cbind(covar, F1exhr, F1baseball, F1football, F1soccer, 
               F1swim, F1other, F1ind, F1cheer, F1drill, F1band,
               F1play, F1govt, F1honor, F1news, F1serv, F1acad, 
               F1hobby, F1FTA)
rm(F1exhr, F1baseball, F1basketball, F1football, F1soccer, 
   F1swim, F1other, F1ind, F1cheer, F1drill, F1band,
   F1play, F1govt, F1honor, F1news, F1serv, F1acad, 
   F1hobby, F1FTA)

# F2 activities
F2exhr = subset(NELS, select = grep("F2exhr", names(NELS)))
F2team = subset(NELS, select = grep("F2team", names(NELS)))
F2ind = subset(NELS, select = grep("F2ind", names(NELS)))
F2cheer = subset(NELS, select = grep("F2cheer", names(NELS)))
F2music = subset(NELS, select = grep("F2music", names(NELS)))
F2play = subset(NELS, select = grep("F2play", names(NELS)))
F2govt = subset(NELS, select = grep("F2govt", names(NELS)))
F2honor = subset(NELS, select = grep("F2honor", names(NELS)))
F2news = subset(NELS, select = grep("F2news", names(NELS)))
F2serv = subset(NELS, select = grep("F2serv", names(NELS)))
F2acad = subset(NELS, select = grep("F2acad", names(NELS)))
F2hobby = subset(NELS, select = grep("F2hobby", names(NELS)))
F2FTA = subset(NELS, select = grep("F2FTA", names(NELS)))
F2ITM = subset(NELS, select = grep("F2ITM", names(NELS)))
F2IIM = subset(NELS, select = grep("F2IIM", names(NELS)))
F2VYG = subset(NELS, select = grep("F2VYG", names(NELS)))
F2VSG = subset(NELS, select = grep("F2VSG", names(NELS)))
F2VPG = subset(NELS, select = grep("F2VPG", names(NELS)))
F2VCHG = subset(NELS, select = grep("F2VCHG", names(NELS)))
F2VCOG = subset(NELS, select = grep("F2VCOG", names(NELS)))
F2HOS = subset(NELS, select = grep("F2HOS", names(NELS)))
F2EDU = subset(NELS, select = grep("F2EDU", names(NELS)))
F2ENV = subset(NELS, select = grep("F2ENV", names(NELS)))

covar <- cbind(covar, F2exhr, F2team, F2ind, F2cheer, F2music, F2play, 
               F2govt, F2honor, F2news, F2serv, F2acad, F2hobby, 
               F2FTA, F2ITM, F2IIM, F2VYG, F2VSG, F2VPG, F2VCHG, F2VCOG, 
               F2HOS, F2EDU, F2ENV)
rm(F2exhr, F2team, F2ind, F2cheer, F2music, F2play, 
   F2govt, F2honor, F2news, F2serv, F2acad, F2hobby, 
   F2FTA, F2ITM, F2IIM, F2VYG, F2VSG, F2VPG, F2VCHG, F2VCOG, 
   F2HOS, F2EDU, F2ENV)
