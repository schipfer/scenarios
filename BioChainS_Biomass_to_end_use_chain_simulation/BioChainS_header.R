

Inputdaten<-"BioChainS_Input.xlsx"
setwd(pfad)

library("xlsx", lib.loc="~/R/win-library/3.1")
library(plotrix)
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("gplots", lib.loc="~/R/win-library/3.1")

library("gridExtra", lib.loc="~/R/win-library/3.1")
library(grid)
library(gtable)
ttx<-ttheme_minimal()

mkdirs <- function(fp) {
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
}

##IDs for data sets to be ordered alphabetically
So<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="Sourcing",
              startRow=1,colIndex=c(1:8))
So[,1]<-match(So[,2],unique(So[,2]))
Su<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="Supply",
              startRow=1,colIndex=c(1:8))
Su[,1]<-match(Su[,2],unique(Su[,2]))
De<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="Densification",
              startRow=1,colIndex=c(1:8))
De[,1]<-match(De[,2],unique(De[,2]))
Ca<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="BEcarrier",
              startRow=1,colIndex=c(1:8))
Ca[,1]<-match(Ca[,2],unique(Ca[,2]))
Di<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="Distribution",
              startRow=1,colIndex=c(1:8))
Di[,1]<-match(Di[,2],unique(Di[,2]))
Co<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="Conversion",
              startRow=1,colIndex=c(1:8))
Co[,1]<-match(Co[,2],unique(Co[,2]))
Re<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="Ref_prices",
              startRow=1,colIndex=c(1:8))
Re[,1]<-match(Re[,2],unique(Re[,2]))
Se<-read.xlsx(paste(pfad,Inputdaten,sep=""),sheetName="Sensitivity",
              startRow=1,colIndex=c(1:7))
Se[,1]<-match(Se[,2],unique(Se[,2]))

## Supply chain configurations
biomass<-c("Wheat straw","Wood chips")
densitech<-c("None","Pelletisation","Torr&Pell","Pyrolysis")
densitechS<-densitech[2:4]

tech<-data.frame(c("Pell_straw", "Torr&Pell_straw","Fast_pyr_straw"),
                 c("Pell_wood","Torr&Pell_wood","Fast_pyr_wood"))
names(tech)<-biomass

prod<-data.frame(c("Straw pellets","Torr. Straw pellets","Biosyncrude_wheat"),
                 c("Wood pellets","Torr. Wood pellets","Biosyncrude_wood"))
names(prod)<-biomass

lief<-data.frame(c("Tractor_straw","Truck_straw"),
                 c("Tractor_chips","Truck_chips"))
names(lief)<-biomass


# structure for output files
prodcost<-matrix(nrow=2,ncol=4,0)
rownames(prodcost)<-biomass
colnames(prodcost)<-densitech
prodsize<-prodcost
prodtcost<-prodcost
supdist<-prodcost
bmcost<-prodcost
supcost<-prodcost
OPEXcost<-prodcost
CAPEXcost<-prodcost

# transport structure
carrier<-data.frame(straw=c("Wheat straw","Straw pellets","Torr. Straw pellets","Biosyncrude_wheat"),
                    wood=c("Wood chips","Wood pellets","Torr. Wood pellets","Biosyncrude_wood"))
modi<-data.frame(solid=c("Truck","Rail","Ocean"),
                 liqu=c("Truck_oil","Rail_oil","Ocean_oil"))

delC_structure<-matrix(nrow=6,ncol=3)
colnames(delC_structure)<-c("prodC","transC","storeC") #,"storeCxtra"
rownames(delC_structure)<-c("RDP1","RDP2","RDP3","RDP4","RDP5","RDP6") 

delC<-lapply(1:8,function(x) delC_structure)
names(delC)<-c("S","StP","StT","StO","W","WtP","WtT","WtO")

# end user efficiency and additional costs
RES<-rep(c("Res_pell","Res_torrpell","Res_syn"),2)
IND<-rep(c("Ind_pell","Ind_torrpell","Ind_syn"),2)
COF<-rep(c("Co_pell","Co_torrpell","Co_syn"),2)
FTR<-rep(c("FT_pell","FT_torrpell","FT_syn"),2)

euC<-matrix(nrow=4,ncol=6)
colnames(euC)<-c("StP","StT","StO","WtP","WtT","WtO")
rownames(euC)<-c("RES","IND","COF","FTR")
euEff<-euC
euRev<-euC

for(i in 1:6){
  euC[1,i]<-Co[Co$ID==match(RES[i],unique(Co[,2]))&Co$CD=="cost",]$ave[1]
  euC[2,i]<-Co[Co$ID==match(IND[i],unique(Co[,2]))&Co$CD=="cost",]$ave[1]
  euC[3,i]<-Co[Co$ID==match(COF[i],unique(Co[,2]))&Co$CD=="cost",]$ave[1]
  euC[4,i]<-Co[Co$ID==match(FTR[i],unique(Co[,2]))&Co$CD=="cost",]$ave[1]
  euEff[1,i]<-Co[Co$ID==match(RES[i],unique(Co[,2]))&Co$CD=="Y_E",]$ave[1]
  euEff[2,i]<-Co[Co$ID==match(IND[i],unique(Co[,2]))&Co$CD=="Y_E",]$ave[1]
  euEff[3,i]<-Co[Co$ID==match(COF[i],unique(Co[,2]))&Co$CD=="Y_E",]$ave[1]
  euEff[4,i]<-Co[Co$ID==match(FTR[i],unique(Co[,2]))&Co$CD=="Y_E",]$ave[1]
  euRev[1,i]<-Co[Co$ID==match(RES[i],unique(Co[,2]))&Co$CD=="Y_rev",]$ave[1]
  euRev[2,i]<-Co[Co$ID==match(IND[i],unique(Co[,2]))&Co$CD=="Y_rev",]$ave[1]
  euRev[3,i]<-Co[Co$ID==match(COF[i],unique(Co[,2]))&Co$CD=="Y_rev",]$ave[1]
  euRev[4,i]<-Co[Co$ID==match(FTR[i],unique(Co[,2]))&Co$CD=="Y_rev",]$ave[1]
}
euREF<-c(0,
         Co[Co$ID==match("Residential",unique(Co[,2]))&Co$CD=="price",]$ave[1],
         Co[Co$ID==match("Electricity",unique(Co[,2]))&Co$CD=="price",]$ave[1],
         Co[Co$ID==match("Diesel",unique(Co[,2]))&Co$CD=="price",]$ave[1])


### format for sensitivity analysis
pls <- function(x, ...)
{
  if (x > 0)
  {
    sprintf(
      fmt = "+%s", 
      format(round(x,1), ...,nsmall=1)
    )
  }
  else
  {
    format(round(x,1),nsmall=1)
  }
}

##representative distribution paths
RDPs<-matrix(nrow=6,ncol=7)
colnames(RDPs)<-c("t_store","d_1st","mode_1st","d_2nd","mode_2nd","d_3rd","mode_3rd")
rownames(RDPs)<-c("RDP1","RDP2","RDP3","RDP4","RDP5","RDP6") #,"RDP7","RDP8"
rdp<-rownames(RDPs)
for(i in 1:6){
  RDPs[i,]<-c(Se[Se$ID==match(rdp[i],unique(Se[,2]))&Se$CD=="t_store",]$ave[1],
             Se[Se$ID==match(rdp[i],unique(Se[,2]))&Se$CD=="d_1st",]$ave[1],
             Se[Se$ID==match(rdp[i],unique(Se[,2]))&Se$CD=="mode_1st",]$ave[1],
             Se[Se$ID==match(rdp[i],unique(Se[,2]))&Se$CD=="d_2nd",]$ave[1],
             Se[Se$ID==match(rdp[i],unique(Se[,2]))&Se$CD=="mode_2nd",]$ave[1],
             Se[Se$ID==match(rdp[i],unique(Se[,2]))&Se$CD=="d_3rd",]$ave[1],
             Se[Se$ID==match(rdp[i],unique(Se[,2]))&Se$CD=="mode_3rd",]$ave[1])
}

##for the sensitivity analysis the reference values
YuAuSvar<-1
BMcostvar<-1
DISTvar<-1
STOREvar<-1
NCVvar<-1  ###
BDvar<-1 ###
t_STOREvar<-1 ###
delmode<-2
INVvar<-1
Yvar<-c(1,1)
HANDvar<-1

