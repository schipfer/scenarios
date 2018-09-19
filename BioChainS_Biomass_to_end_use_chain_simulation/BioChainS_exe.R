#############################################################################################
# Filename: BioChainS_exe.R
#
# Author: Fabian Schipfer (FS)
# Created: 15-December-2016
#
# Version: 1.0
#
# Changed on: 19-September-2018 by (FS)
#         preparation for submission in Bioresource Technology: documentation updated; 
# Run on: RStudio Version 1.1.383 with R version 3.4.2 (2017-09-28)
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
rm(list = ls())

pfad<-"~/Submissions/Densification/BioChainS_Biomass_to_end_use_chain_simulation/" ## set your working directory here !!

source("BioChainS_header.R") #necessary packages are loaded here - manipulations could be required in this file, 
#depending on wether you have the packages installed or not and the correct path to your win-library (if you´re using R-Studio manually load one package to see your path)

source("BioChainS_calc.R")

subDir<-paste("output",strtrim(Sys.time(),10),sep="")
mkdirs(paste("output",strtrim(Sys.time(),10),sep=""))

source("BioChainS_figures.R")

source("BioChainS_sensitivity.R")

source("BioChainS_tables.R")
