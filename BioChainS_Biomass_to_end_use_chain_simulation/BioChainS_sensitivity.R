# Sensitivity scripts

delmode<-1
source("BioChainS_calc.R")

resm_delvar<-resm

delmode<-2
Yvar<-c(1-0.8,1-0.8)
source("BioChainS_calc.R")

resm_Yd<-resm
euLIST_Yd<-euLIST

Yvar<-c(1+0.8,1+0.8)
source("BioChainS_calc.R")

resm_Yu<-resm
euLIST_Yu<-euLIST


Yvar<-c(1,1)
INVvar<-(1-0.1)
source("BioChainS_calc.R")

resm_INVd<-resm

INVvar<-(1+0.1)
source("BioChainS_calc.R")

resm_INVu<-resm

INVvar<-(1)
STOREvar<-5
source("BioChainS_calc.R")

euLIST_STOREu<-euLIST
delC_STOREu<-delC

STOREvar<-1
DISTvar<-1.5
source("BioChainS_calc.R")

delC_DISTu<-delC
euLIST_DISTu<-euLIST

DISTvar<-0.5
source("BioChainS_calc.R")

delC_DISTd<-delC
euLIST_DISTd<-euLIST

DISTvar<-1
HANDvar<-1.5
source("BioChainS_calc.R")

delC_HANDu<-delC
euLIST_HANDu<-euLIST

HANDvar<-0.5
source("BioChainS_calc.R")

delC_HANDd<-delC
euLIST_HANDd<-euLIST

HANDvar<-1
BMcostvar<-1.2
source("BioChainS_calc.R")

euLIST_Cu<-euLIST

BMcostvar<-0.8
source("BioChainS_calc.R")

euLIST_Cd<-euLIST

BMcostvar<-1
source("BioChainS_calc.R")
