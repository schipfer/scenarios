#############################################################################################
# Filename: Advanced_biomaterials_scenarios.R
#
# Author: Fabian Schipfer (FS)
# Created: 07-July-2016
#
# Version: 1.0
#
# Changed on: 24-Dec-2016 by (FS)
#         preparation for submission in Biomass and Bioenergy: documentation updated; 
# Run on: RStudio Version 0.98.1049 with R version 3.1.1 (2014-07-10)
#############################################################################################
#
# All calculations and graphics presented in manuscript
#
#
# Data files required:   OSCE_IEA_PRIMES_data.rds
# Subfunctions:          none
# R-files required:      none
# other files:           none
# Problems:              none
#############################################################################################

#setwd("C:/USERS/WORKING DIRECTORY HERE")
options(java.parameters = "-Xmx1000m") 
library("xlsx", lib.loc="~/R/win-library/3.1")

# Set input parameters ####
countrylist<-c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech_Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovak_Republic","Slovenia","Spain","Sweden","United_Kingdom","EU28")
histdata<-c("Total","Bitumen","Lubricants_Waxes","non_E_consumption","PRIMES_projection[ktoe]")
histtime<-c(2000:2010,seq(2015,2050,5))

# Specific input data explained in the Schipfer et al., 2016 ####
non_E_share<-c(0.896,0.082,0.022) #for polymers, solvents and surfactants resp.
poly_share<-c(0.485,0.07,0.445) #for PE&PP, PET and more durable polymers (others) resp.
biobased_08<-c(0,0.15,0.13,0.63,1.52)*1000 #in tonnes for bitumen, lubr., poly., solv. & surf. (BioChem_Report)
biobased_MS<-c(10,11,15,20,27,28) #assuming biobased production to be situated in MS with funding (OECD,2013)
bio_poly_share<-c(0.87,0,0.13) #for PLA, Starch blends, PHA, PBAT vs PET vs PUR,PA and others resp.
polymers_14<-1.697*0.154*1000 #biobased polymers global production * European share for 2014
gluc_current<-1/c(0.7185,0.53,0.286,0.472)
gluc_theoretical<-1/c(0.8,0.556,0.311,0.511)
oil_current<-1/c(0.8,0.7691)
oil_theoretical<-1/c(1,0.4615)
gluc<-gluc_current
oil<-oil_current


# Calculate heating values for biogenic building blocks ####
HHV<-function(C,H,O,N,S,ash){0.3491*C+1.1783*H-0.1034*O-0.0151*N+0.1005*S-0.0211*ash} #Channiwala and Parikh
# glucose heating value for pure C6H12O6
weight_g<-(6*12.011+12*1.008+6*15.999)
gluc_HHV<-HHV((6*12.011)/weight_g,(12*1.008)/weight_g,(6*15.999)/weight_g,0,0,0)*100
# lignin heating value for pure C10H12O3
weight_l<-(10*12.011+12*1.008+3*15.999)
lig_HHV<-HHV((10*12.011)/weight_l,(12*1.008)/weight_l,(3*15.999)/weight_l,0,0,0)*100
# plant oil heating value for pure C18H32O2
weight_o<-(18*12.011+32*1.008+2*15.999)
oil_HHV<-HHV((18*12.011)/weight_o,(32*1.008)/weight_o,(2*15.999)/weight_o,0,0,0)*100
HHVs<-c(gluc_HHV,gluc_HHV,lig_HHV,oil_HHV)

# Read array on fossil based materials. Historic data from IEA&OSCE and the PRIMES reference scenario
ref<-readRDS("OSCE_IEA_PRIMES_data.rds")

# Project fossil non-E consumption up to 2050 based on PRIMES reference scenario ####
for(i in 1:28){
  for(j in 12:19){
    growth<-ref[5,j,i]/ref[5,j-1,i]
    ref[2:4,j,i]<-ref[2:4,j-1,i]*growth
  }
}
ref[,,]<-replace(ref,is.na(ref),0)
ref[1,12:19,1:28]<-apply(ref[2:4,12:19,1:28],c(2,3),sum)
ref[,12:19,29]<-apply(ref[,12:19,1:28],c(1,2),sum)

# Estimate product groups to be substituted ####
fossil<-c("Production","Bitumen","Lubricants","PE_PP","PET","Other","Solvents","Surfactants")

substitut<-array(0,dim=c(length(fossil),length(histtime),length(countrylist))) 
dimnames(substitut)[[1]]<-fossil
dimnames(substitut)[[2]]<-histtime
dimnames(substitut)[[3]]<-countrylist

substitut[2:3,,]<-ref[2:3,,]
substitut[4,,]<-ref[4,,]*non_E_share[1]*poly_share[1]
substitut[5,,]<-ref[4,,]*non_E_share[1]*poly_share[2]
substitut[6,,]<-ref[4,,]*non_E_share[1]*poly_share[3]
substitut[7,,]<-ref[4,,]*non_E_share[2]
substitut[8,,]<-ref[4,,]*non_E_share[3]
substitut[1,,]<-apply(substitut[2:8,,],c(2,3),sum)

# Current biobased-shares in the different MS ####
bio<-c("Production","Bitumen","Lubricants","PHB_PLA","PET","Other_polymers","Solvents","Surfactants")
scenariotime<-seq(2015,2050,5)

biobased<-array(0,dim=c(length(bio),length(scenariotime),length(countrylist))) 
dimnames(biobased)[[1]]<-bio
dimnames(biobased)[[2]]<-scenariotime
dimnames(biobased)[[3]]<-countrylist


development<-ref[1,12,29]/ref[1,9,29] #fossil relation 2015/2008
biobased[c(2:3,7:8),1,29]<-biobased_08[c(1:2,4:5)]*development #assuming similar development for all but polymers

polymers_15<-polymers_14+(polymers_14-biobased_08[3])/5 #assuming linear growth rate
biobased[4:6,1,29]<-polymers_15*bio_poly_share

productshare<-substitut[2:8,12,biobased_MS]/apply(substitut[2:8,12,biobased_MS],c(1),sum)
biobased[2:8,1,biobased_MS]<-biobased[2:8,1,29]*productshare

# Projection of substitution up to 2050 for two scenarios ####
names<-c("Reference","Full_substitution")

scenarios<-array(0,dim=c(length(bio),length(scenariotime),length(countrylist),length(names))) 
dimnames(scenarios)[[1]]<-bio
dimnames(scenarios)[[2]]<-scenariotime
dimnames(scenarios)[[3]]<-countrylist
dimnames(scenarios)[[4]]<-names

scenarios[,,,c(1,2)]<-biobased[,,]

for(i in 2:8){ #reference scenario
  scenarios[3,i,,1]<-scenarios[3,i-1,,1]+substitut[3,19,]*(0.4-(scenarios[3,5,,1]/(substitut[3,12,]+scenarios[3,1,,1])))*((1+(i-2)*2)/49)
  scenarios[4,i,,1]<-scenarios[4,i-1,,1]+substitut[4,19,]*(0.4-(scenarios[4,5,,1]/(substitut[4,12,]+scenarios[4,1,,1])))*((1+(i-2)*2)/49)
  scenarios[5,i,,1]<-scenarios[5,i-1,,1]+substitut[5,19,]*(0.4-(scenarios[5,5,,1]/(substitut[5,12,]+scenarios[5,1,,1])))*((1+(i-2)*2)/49)
  scenarios[6,i,,1]<-scenarios[6,1,,1]
  scenarios[7,i,,1]<-scenarios[7,i-1,,1]+substitut[7,19,]*(0.4-(scenarios[7,5,,1]/(substitut[7,12,]+scenarios[7,1,,1])))*((1+(i-2)*2)/49)
  scenarios[8,i,,1]<-scenarios[8,i-1,,1]+substitut[8,19,]*((1+(i-2)*2)/49)
}

for(i in 2:8){ #full_transition scenario
  scenarios[2,i,,2]<-scenarios[2,i-1,,2]+substitut[2,19,]*(0.7-(scenarios[2,1,,2]/(substitut[2,12,]+scenarios[2,1,,2])))*((1+(i-2)*2)/49)
  scenarios[3,i,,2]<-scenarios[3,i-1,,2]+substitut[3,19,]*(0.7-(scenarios[3,1,,2]/(substitut[3,12,]+scenarios[3,1,,2])))*((1+(i-2)*2)/49)
  scenarios[4,i,,2]<-scenarios[4,i-1,,2]+substitut[4,19,]*(0.7-(scenarios[4,1,,2]/(substitut[4,12,]+scenarios[4,1,,2])))*((1+(i-2)*2)/49)
  scenarios[5,i,,2]<-scenarios[5,i-1,,2]+substitut[5,19,]*(0.7-(scenarios[5,1,,2]/(substitut[5,12,]+scenarios[5,1,,2])))*((1+(i-2)*2)/49)
  scenarios[6,i,,2]<-scenarios[6,i-1,,2]+substitut[6,19,]*(0.7-(scenarios[6,1,,2]/(substitut[6,12,]+scenarios[6,1,,2])))*((1+(i-2)*2)/49)
  scenarios[7,i,,2]<-scenarios[7,i-1,,2]+substitut[7,19,]*(0.7-(scenarios[7,1,,2]/(substitut[7,12,]+scenarios[7,1,,2])))*((1+(i-2)*2)/49)
  scenarios[8,i,,2]<-scenarios[8,i-1,,2]+substitut[8,19,]*((1+(i-2)*2)/49)
}

# Cleaning array from NAs and summarising all products and all countries
scenarios<-replace(scenarios,is.na(scenarios),0)
scenarios[1,,,]<-apply(scenarios[2:8,,,],c(2,3,4),sum)
scenarios[,2:8,29,]<-apply(scenarios[,2:8,1:28,],c(1,2,4),sum)

# Estimation of demand for starch/sugar/oil & wood(cellulose) ####

types<-c("Total","Starch&Sugar_based_gluc","Wood_based_gluc","Wood_based_lign","Oil_based")

biomass<-array(0,dim=c(length(types),length(scenariotime),length(countrylist),length(names))) 
dimnames(biomass)[[1]]<-types
dimnames(biomass)[[2]]<-scenariotime
dimnames(biomass)[[3]]<-countrylist
dimnames(biomass)[[4]]<-names

##reference scenario (1): Starting with 2035 all glycose from starch &sugar capacities substituted with wood based
#glucose demand for capacities up to 2030:

biomass[2,1:4,1:28,1]<-scenarios[4,1:4,1:28,1]*gluc[1]+scenarios[5,1:4,1:28,1]*gluc[2]+
  scenarios[6,1:4,1:28,1]*gluc[3]+scenarios[7,1:4,1:28,1]*gluc[4]
#stagnating glucose demand from 2035 to 2050:
biomass[2,5:8,1:28,1]<-matrix(rep(t(biomass[2,4,1:28,1]),4),nrow=4,ncol=28,byrow=TRUE)
#glucose demand from wood starting with 2035:
biomass[3,5:8,1:28,1]<-scenarios[4,5:8,1:28,1]*gluc[1]+scenarios[5,5:8,1:28,1]*gluc[2]+
  scenarios[6,5:8,1:28,1]*gluc[3]+scenarios[7,5:8,1:28,1]*gluc[4]-biomass[2,5:8,1:28,1]

##full_substitution scenario (2): Starting with 2025 ... plus bitumen from wood
#glucose demand for capacities up to 2020:
biomass[2,1:2,1:28,2]<-scenarios[4,1:2,1:28,2]*gluc[1]+scenarios[5,1:2,1:28,2]*gluc[2]+
  scenarios[6,1:2,1:28,2]*gluc[3]+scenarios[7,1:2,1:28,2]*gluc[4]
#stagnating glucose demand from 2025 to 2050:
biomass[2,3:8,1:28,2]<-matrix(rep(t(biomass[2,2,1:28,2]),6),nrow=6,ncol=28,byrow=TRUE)
#glucose demand from wood starting with 2025:
biomass[3,3:8,1:28,2]<-scenarios[4,3:8,1:28,2]*gluc[1]+scenarios[5,3:8,1:28,2]*gluc[2]+
  scenarios[6,3:8,1:28,2]*gluc[3]+scenarios[7,3:8,1:28,2]*gluc[4]-biomass[2,3:8,1:28,2]
#bitumen with 1:1 from lignin
biomass[4,,1:28,2]<-scenarios[2,,1:28,2]


##both scenarios lubricants and surfactants from oil
biomass[5,,1:28,]<-scenarios[3,,1:28,]*oil[1]+scenarios[8,,1:28,]*oil[2]

#summarising all products and all countries
biomass[1,,,]<-apply(biomass[2:5,,,],c(2,3,4),sum)
biomass[,,29,]<-apply(biomass[,,1:28,],c(1,2,4),sum)

# Comparison with demand for energy ####

biomass[2:5,,29,1]*HHVs/1000 #in PJ
biomass[2:5,,29,2]*HHVs/1000 #in PJ
colSums(biomass[2:5,,29,2]*HHVs/1000) #in PJ

# Graphs and tables ####
# Figure 1 - comparing scenarios with fossil based production
fig1<-scenarios[,c(1:2,4,8),29,]
for(t in 4:2){
  fig1[,t,]<-fig1[,t,]-fig1[,t-1,]
}


prodffin<-c("Total","Bitumen","Lubricants","PE&PP","PET","PVC,PS&PUR","Solvents","Surfactants")
prodbfin<-c("Total","Bitumen","Lubricants","Degradable","PET","other polymers","Solvents","Surfactants")

fig1a<-matrix(0,nrow=7*2,ncol=4)
vers<-c(1,3,5,7,9,11,13)
fig1a[vers,]<-fig1[2:8,,1]
fig1a[vers+1,]<-fig1[2:8,,2]

kl<-.2#.2
gr<-.7#.7
abst<-c(kl,rep(c(kl,gr),6),c(kl))
weite<-.6
prodbfin<-c("Total","Bio-\nbitumen","Lubricants","Bio-\ndegradable\n  Polymers","PEF",
            "Other\ndrop-in bio\n       -polymers","Solvents\n      (Ethanol)","Surfactants")
legende<-c("Fossil based\nproduction in 2015\n",
           "Additional biobased\nmaterials in 2050",
           "Additional biobased\nmaterials in 2030",
           "Additional biobased\nmaterials in 2020",
           "Estimated biomaterials\nproduction in 2015")
farben<-c("#a50f15","#f03b20","#fd8d3c","#fecc5c") #bd0026

png(paste("fig1",".jpeg",sep=""),    # create PNG for the heat map        
    width = 8*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 12)

umg<-barplot(t(fig1a),width=weite,space=abst,ylim=c(0,25000),xlim=c(0,14*weite+7*kl+6*gr+kl),axes=F,col=farben)
par(new=TRUE)
points(umg[c(1.75,3.75,5.75,7.75,9.75,11.75,13.75)]+.35,substitut[2:8,c(12),29],pch=19,,cex=1,col="black")
axis(2,at=seq(0,25000,5000),labels=seq(0,25,5),las=2,cex.axis=.85)
text(x=umg[vers],y=-3000,labels=prodbfin[2:8],srt=50,xpd=T,cex=.85)
text(x=umg[c(0.75,1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75,11.75,12.75,13.75,14.75)],y=-300,labels=rep(c("O","#"),7),xpd=T,cex=.7)
legend(x=umg[12],25000,legende,col=c("black",rev(farben)),cex=.9,
       pch=c(19,NA,NA,NA,NA),fill=c(0,rev(farben)),border=c("white",rep("black",4)))
text(5.5,28000,"EU 28 biobased material production for 2015-2050 \nin the reference (O) and transition (#) scenario",
     cex=1.2,xpd=T,font=2)
text(-1.6,13000,"Annual production in 10^6 tonnes",cex=1,xpd=T,font=2,srt=90)

dev.off()

# Figure 2 - biogenic building blocks demand
A2<-c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IR","IT","LV","LT","LX","ML","NL","PL","PT","RO","SK","SI","ES","SE","UK")
fig5<-matrix(0,nrow=28*2,ncol=4)
fig5[seq(1,28*2-1,2),]<-t(biomass[2:5,8,1:28,1])
fig5[seq(2,28*2,2),]<-t(biomass[2:5,8,1:28,2])
E_content_ref<-colSums(biomass[2:5,8,,1]*HHVs/1000) #in PJ
E_content_full<-colSums(biomass[2:5,8,,2]*HHVs/1000) #in PJ

kl<-.2
gr<-.7
abst<-c(kl,rep(c(kl,gr),27),kl)
weite<-.6
farben5<-c("#238443","#78c679","#c2e699","#ffffcc")

png(paste("fig2",".jpeg",sep=""),    # create PNG         
    width = 10*300,        # 10 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 12)

umg<-barplot(t(fig5),width=weite,space=abst,ylim=c(0,35000),xlim=c(0,28*2*weite+27*kl+27*gr+2*kl),axes=F,col=farben5)
axis(2,at=seq(0,35000,5000),labels=seq(0,35,5),las=2,cex.axis=.85)
text(x=umg[seq(1,28*2-1,2)]+.35,y=-3800,labels=A2,srt=0,xpd=T,cex=.8)
text(x=umg[seq(1,28*2-1,2)],y=-600,labels=rep("O",28),srt=0,xpd=T,cex=.7)
text(x=umg[seq(2,28*2,2)],y=-600,labels=rep("#",28),srt=0,xpd=T,cex=.7)
text(24,35000,"Biogenic building block demand for advanced biomaterials 
in 2050 for the reference (O) and the transition (#) scenario",
     cex=1.2,xpd=T,font=2)
text(-5.5,17000,"Annual demand in 1000 tonnes",cex=1,xpd=T,font=2,srt=90)
legend(x=umg[28*2]+.5,30000,rev(c("Starch &sugar\nbased glucose","Lignocellulose\nbased glucose","Lignin","Plant oil\nbased\n")),
       col=c(rev(farben5)),cex=.8,lwd=c(0,0,0,0),
       lty=c(0,0,0,0),fill=c(rev(farben5)),border=rep("black",4))

dev.off()

# Evalution of results ####

#estimated share of total biobased chemical production around 2000
sub2000<-sum(1180,6,10,25)/(substitut[1,1,29])*100
#estimated share of total biobased chemical production around 1998
sub2008<-sum(1520,63,150,130)/(substitut[1,9,29])*100
#estimated share of total biobased chemical production in 2015
sub2015<-sum(biobased[2:8,1,29])/(substitut[1,12,29])*100
#estimated share of total biobased chemical production in 2020 , 2030 and 2050 in scenario 1
sub_scen1<-scenarios[1,c(2,4,8),29,1]/(substitut[1,c(13,15,19),29])*100
#estimated share of total biobased chemical production in 2020 , 2030 and 2050 in scenario 2
sub_scen2<-scenarios[1,c(2,4,8),29,2]/(substitut[1,c(13,15,19),29])*100

((sub2008-sub2000)/sub2000)/8
((sub2015-sub2008)/sub2008)/7
((sub_scen1[1]-sub2015)/sub2015)/15
((sub_scen2[1]-sub2015)/sub2015)/15
((sub_scen1[2]-sub2015)/sub2015)/25
((sub_scen2[2]-sub2015)/sub2015)/25
((sub_scen1[2]-sub_scen1[1])/sub_scen1[1])/10
((sub_scen2[2]-sub_scen2[1])/sub_scen2[1])/10
((sub_scen1[3]-sub_scen1[2])/sub_scen1[2])/10
((sub_scen2[3]-sub_scen2[2])/sub_scen2[2])/10

# Energy content of biogenic building blocks
c(E_content_ref[29],E_content_full[29]) #in PJ for all EU28 in 2050 for the reference and the transition scenario

# Growth potentials
scenarios[,c(1,4,8),29,1]

(scenarios[8,8,29,1]-scenarios[8,1,29,1])/scenarios[8,1,29,1]
(scenarios[7,8,29,2]-scenarios[7,1,29,2])/scenarios[7,1,29,2]
(scenarios[3,8,29,2]-scenarios[3,1,29,2])/scenarios[3,1,29,2]
(scenarios[4,8,29,2]-scenarios[4,1,29,2])/scenarios[4,1,29,2]
(scenarios[4,8,29,1]-scenarios[4,1,29,1])/scenarios[4,1,29,1]
(scenarios[6,8,29,2]-scenarios[6,1,29,2])/scenarios[6,1,29,2]
scenarios[2,8,29,2]

# Biogenic building blocks
biomass[,c(1,4,8),29,1]
biomass[,c(1,4,8),29,2]

biomass[3,c(1,4,8),29,1]/colSums(biomass[c(2:3),c(1,4,8),29,1])
biomass[3,c(1,4,8),29,2]/colSums(biomass[c(2:3),c(1,4,8),29,2])

round(biomass[1,c(1,4,8),1:28,1],1)
round(biomass[1,c(1,4,8),1:28,2],1)
round(substitut[1,c(12,15,19),1:28]/substitut[1,c(12,15,19),29],2)
rowSums(substitut[1,c(12,15,19),c(2,10,11,15,20,27,28)]/substitut[1,c(12,15,19),29])


# Energy content
colSums(biomass[2:5,1,,1]*HHVs/1000)
colSums(biomass[2:5,8,,1]*HHVs/1000)
colSums(biomass[2:5,8,,2]*HHVs/1000)

colSums(biomass[2:5,8,,1]*HHVs/1000)[29]/(colSums(biomass[2:5,8,,1]*HHVs/1000)[29]+13000)
colSums(biomass[2:5,8,,2]*HHVs/1000)[29]/(colSums(biomass[2:5,8,,2]*HHVs/1000)[29]+20000)
