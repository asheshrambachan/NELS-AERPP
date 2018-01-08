#---------------------------------------------------------------------------
# Author: Clara Marquardt
# Date: December 2017
# Language: R
# Notes: (-) Script assumes that in NELS directory                  
#---------------------------------------------------------------------------

# Setup
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# Dependencies
library(data.table)                 # install.packages("data.table", repos="https://cran.rstudio.com")
source("code/clean_scripts/var_overview.R")      

# Basic Data Prep. (Raw > Mod Data (Subset with only the selected variables))
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# Load Raw Data
load("raw_data/NELS_88_00_BYF4STU_v1_0.rdata")

# Create Vector of Selected Variables
keepvars <- c(
  "STU_ID",
  "F4UNIV1",
  "BYS81A",
  "BYS81B",
  "BYS81C",
  "BYS81D",
  "G8URBAN",
  "G8REGON",
  "G8MINOR",
  "G8LUNCH",
  "SEX",
  "RACE",
  "BIRTHMO",
  "BIRTHYR",
  "BYSES",
  "BYPARED",
  "BYFAMSIZ",
  "BYFCOMP",
  "BYPARMAR",
  "BYFAMINC",
  "BYGRADS",
  "BY2XRSTD",
  "BY2XMSTD",
  "BY2XSSTD",
  "BY2XHSTD",
  "F1S39A",
  "F1S39B",
  "F1S39C",
  "F1S39D",
  "F1S41AA",
  "F1S41AB",
  "F1S41AC",
  "F1S41AD",
  "F1S41AE",
  "F1S41AF",
  "F1S41AG",
  "F1S41AH",
  "F1S41AI",
  "F1S41BA",
  "F1S41BB",
  "F1S41BC",
  "F1S41BD",
  "F1S41BE",
  "F1S41BF",
  "F1S41BG",
  "F1S41BH",
  "F1S41BI",
  "F1S42",
  "F1DOSTAT",
  "G8CTRL1",
  "G10CTRL1",
  "G10URBAN",
  "G10REGON",
  "F1SCENRL",
  "G10ENROL",
  "F12XRSTD",
  "F12XMSTD",
  "F12XSSTD",
  "F12XHSTD",
  "F12XRPRO",
  "F12XMPRO",
  "F12XSPRO",
  "F2S30AA",
  "F2S30AB",
  "F2S30AC",
  "F2S30BA",
  "F2S30BB",
  "F2S30BC",
  "F2S30BD",
  "F2S30BE",
  "F2S30BF",
  "F2S30BG",
  "F2S30BH",
  "F2S30BI",
  "F2S30BJ",
  "F2S30BK",
  "F2S31",
  "F2S39A",
  "F2S39B",
  "F2S39C",
  "F2S39D",
  "F2S39E",
  "F2S39F",
  "F2S39G",
  "F2S39H",
  "F2DOSTAT",
  "F22XRSTD",
  "F22XMSTD",
  "F22XSSTD",
  "F22XHSTD",
  "F22XRPRO",
  "F22XMPRO",
  "F22XSPRO",
  "F2RTROUT",
  "F2RTRPRG",
  "F2RHENG2",
  "F2RHMAG2",
  "F2RHSCG2",
  "F2RHSOG2",
  "F2RHCOG2",
  "F2RHFOG2",
  "F2RENG_C",
  "F2RFOR_C",
  "F2RMAT_C",
  "F2RAL1_C",
  "F2RAL2_C",
  "F2RGEO_C",
  "F2RTRI_C",
  "F2RPRE_C",
  "F2RCAL_C",
  "F2ROMA_C",
  "F2RSCI_C",
  "F2REAR_C",
  "F2RBIO_C",
  "F2RCHE_C",
  "F2RPHY_C",
  "F2ROSC_C",
  "F2RSOC_C",
  "F2RHIS_C",
  "F2ROSO_C",
  "F2RCOM_C",
  "F2RVAG_C",
  "F2RVBU_C",
  "F2RVGN_C",
  "F2RVHE_C",
  "F2RVHO_C",
  "F2RVMA_C",
  "F2RVTE_C",
  "F2RVTR_C",
  "BYRATIO",
  "PPOSTEX1",
  "PPOSTEX2",
  "F3DIPLOM",
  "F4EGRD",
  "F4BYPNFL",
  "F4PNLFL",
  "F4F1PNFL",
  "F4F2PNFL",
  "F4BYPNWT",
  "F4PNLWT",
  "F4F1PNWT",
  "F4F2PNWT",
  "F4HSDIPL",
  "F4HHDG",
  "F4TYPEDG",
  "F4ATT4YR",
  "F4ATTPSE",
  "F1QFLG", 
  "F2QFLG"
)

# Number of variables selected
keepvars <- unique(keepvars)
cat(sprintf("number of selected variables: %d\n", length(keepvars)))

# Basic Formatting
# ---------------------------

## Subset to relevant variables
BYF4STU_subset <- BYF4STU[keepvars]

## Convert to data table
BYF4STU_subset <- as.data.table(BYF4STU_subset)

## Impose sample restrictions

sink("documentation/subsetting_process.txt", split=TRUE)

# (0) Baseline
cat(sprintf("[Raw] Rows: %d\n", nrow(BYF4STU_subset)))

# (1) Respondents which are part of the BY survey & the F4 survey (F4BYPNFL Panel Flag)
BYF4STU_subset <- BYF4STU_subset[F4BYPNFL==1]
cat(sprintf("[BY & F4] Rows: %d\n", nrow(BYF4STU_subset)))

# Observations - follow-up 1/2 survey
cat(sprintf("[BY & F4 > F1] Rows: %d\n", nrow(BYF4STU_subset[F1QFLG==1])))
cat(sprintf("[BY & F4 > F2] Rows: %d\n", nrow(BYF4STU_subset[F2QFLG==1])))

sink()

# Save Dataset As RDS File
# ---------------------------
saveRDS(BYF4STU_subset, file="mod_data/NELS_88_00_BYF4STU_v1_0_subset.RDS")

# Data Overview
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# Generate Data Overview
BYF4STU_subset_overview <- var_overview(BYF4STU_subset)

# Save Data Overview
write.csv(BYF4STU_subset_overview, "documentation/BYF4STU_subset_overview.csv", 
  row.names=FALSE)

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

