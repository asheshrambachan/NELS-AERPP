Author: Ashesh Rambachan
Last updated: December 9, 2017


CODE FOLDER
- Contains all R code to produce results. 
- Code description:
	+ clean_scripts folder = folder of scripts to clean data. There is no need to access this directly.
	  it is called directly when needed.
	+ old_code folder = folder of old code created. There for the sake of version control.
	+ bach/grd denotes which outcome variable is used. bach = at least a bachelor's degree is received.
	  grd = at least mostly B's are received as an undergrad
	+ stdlinpred.R = construct the calibration curves that are in the figures folder.
	+ randomforest.R = construct the random forest predictors for both outcomes. 
	+ log.R = construct the logistic regression predictors 
	+ covmtx_build.R = builds the matrix of covariates used throughout the analysis.
	+ data_clean.R = cleans the NELS data. Update this file when you want to use different outcome variables.

NOTES FOLDER
- Contains .tex file for the notes and the associated pdfs.

ESTIMATION FOLDER
- contains save .Rdata files for the constructed random forests. This is useful if you do not want
  to rerun the estimation.

FIGURES FOLDER
- contains saved figures produced in the analyses

RAW_DATA FOLDER
- Contains uncleaned data pulled directly from NELS website. There is no need to access this directly.

MOD_DATA FOLDER
- Contains modified data pulled from NELS website. It has been subsetted to the variables of interest
  and BY/F4 respondents. There is no need to access this directly.

 DOCUMENTATION FOLDER
 - Contains some files that describe the NELS data. 

