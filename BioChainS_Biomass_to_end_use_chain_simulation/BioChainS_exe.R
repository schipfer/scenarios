#############################################################################################
# Filename: BioChainS_exe.R
#
# Author: Fabian Schipfer (FS)
# Created: 15-December-2016
#
# Version: 1.0
#
# Changed on: 23-April-2017 by (FS)
#         preparation for submission in Biomass and Bioenergy: documentation updated; 
# Run on: RStudio Version 0.98.1049 with R version 3.1.1 (2014-07-10)
#############################################################################################
#
# All calculations and graphics presented in manuscript
#
#
# Data files required:   BioChainS_Input.xlsx
# Subfunctions:          none
# R-files required:      BioChainS_header.R; BioChainS_calc.R; BioChainS_figures.R; 
#                        BioChainS_sensitivity.R; BioChainS_tables.R
# other files:           none
# Problems:              none
#############################################################################################


pfad<-"~/BioChainS/" ## set your working directory here !!

source("BioChainS_header.R")

source("BioChainS_calc.R")

subDir<-paste("output",strtrim(Sys.time(),10),sep="")
mkdirs(paste("output",strtrim(Sys.time(),10),sep=""))

source("BioChainS_figures.R")

source("BioChainS_sensitivity.R")

source("BioChainS_tables.R")
