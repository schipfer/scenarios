
## regularly used formulas
GCV<-function(NCV,MC){(NCV*100+2.44*MC)/(100-MC)}
NCV<-function(GCV,MC){(GCV*(100-MC)-2.44*MC)/100}
sup_dist<-function(size,inout,YuAuSraw){((size*inout)/((YuAuSraw*YuAuSvar)*100*pi))^0.5} #incl. variation
ed<-function(ca){Ca[Ca$ID==ca&Ca$CD=="NCV",]$ave[1]*NCVvar*Ca[Ca$ID==ca&Ca$CD=="bd",]$ave[1]*BDvar/1000} #Energy density incl variations
v<-function(x) {as.vector(t(as.matrix(x)))}

##formulas used for transport

#design ratio function
design<-function(ca,dx,bc){if(Ca[Ca$ID==ca&Ca$CD=="bd",]$ave[1]*BDvar<Di[Di$ID==match(modi[dx,bc],unique(Di[,2]))&Di$CD=="cap",]$ave[1]){
  (Di[Di$ID==match(modi[dx,bc],unique(Di[,2]))&Di$CD=="cap",]$ave[1]/Ca[Ca$ID==ca&Ca$CD=="bd",]$ave[1]*BDvar)}else{1}} #incl. variations

#distribution sub-functions
hand<-function(ca,dx,bc){Di[Di$ID==match(modi[dx,bc],unique(Di[,2]))&Di$CD=="h_cost",]$ave[1]*HANDvar/Ca[Ca$ID==ca&Ca$CD=="NCV",]$ave[1]*NCVvar} #incl. variation
var<-function(ca,dx,bc){Di[Di$ID==match(modi[dx,bc],unique(Di[,2]))&Di$CD=="v_cost",]$ave[1]*DISTvar/Ca[Ca$ID==ca&Ca$CD=="NCV",]$ave[1]*NCVvar} #incl. variation
distcost<-function(ca,dist,dx,bc){hand(ca,dx,bc)+var(ca,dx,bc)*dist*design(ca,dx,bc)}

#distribution cost function
DISTCDF<-function(dist,dx) {data.frame(none=c(distcost(match(carrier[1,1],unique(Ca[,2])),dist,dx,1),distcost(match(carrier[1,2],unique(Ca[,2])),dist,dx,1)),
                                       wp=c(distcost(match(carrier[2,1],unique(Ca[,2])),dist,dx,1),distcost(match(carrier[2,2],unique(Ca[,2])),dist,dx,1)),
                                       tp=c(distcost(match(carrier[3,1],unique(Ca[,2])),dist,dx,1),distcost(match(carrier[3,2],unique(Ca[,2])),dist,dx,1)),
                                       pyr=c(distcost(match(carrier[4,1],unique(Ca[,2])),dist,dx,2),distcost(match(carrier[4,2],unique(Ca[,2])),dist,dx,2)))} 

#storage cost function
stc<-c(15.56,34.48) #storage costs in Euro/m3*year
store<-function(ca,days,bc){(stc[bc]*STOREvar/365*days)/ed(ca)} #incl. variation
STOREDF<-function(days){data.frame(none=c(store(match(carrier[1,1],unique(Ca[,2])),days,1),store(match(carrier[1,2],unique(Ca[,2])),days,1)),
                                   wp=c(store(match(carrier[2,1],unique(Ca[,2])),days,1),store(match(carrier[2,2],unique(Ca[,2])),days,1)),
                                   tp=c(store(match(carrier[3,1],unique(Ca[,2])),days,1),store(match(carrier[3,2],unique(Ca[,2])),days,1)),
                                   pyr=c(store(match(carrier[4,1],unique(Ca[,2])),days,2),store(match(carrier[4,2],unique(Ca[,2])),days,2)))}


########### Calculation start
## Optimisation of densification plant size

for(i in 1:3){ #densification technologies
  for(j in 1:2){ #for wheat straw and wood chips
    so<-match(biomass[j],unique(So[,2]))
    de<-match(tech[i,j],unique(De[,2])) 
    ca<-match(prod[i,j],unique(Ca[,2])) 
    su<-match(lief[delmode,j],unique(Su[,2])) #Delivery Mode (Truck=2 | Traktor=1)
    NCV_spec<-Ca[Ca$ID==ca&Ca$CD=="NCV",]$ave[1]*NCVvar
    
    
    MC_dry_input<-10*100/(100-10)
    MC_dry_feed<-So[So$ID==so&So$CD=="mc",]$ave[1]*100/(100-So[So$ID==so&So$CD=="mc",]$ave[1])
    MCdifference<-(MC_dry_feed-MC_dry_input)/100
    #inoutratio_wet<-(1+MC_dry_feed/100)/De[De$ID==de&De$CD=="Y_M",]$ave[1]
    inoutratio_dry<-1/De[De$ID==de&De$CD=="Y_M",]$ave[1]
    sup_d<-function(size){sup_dist(size,inoutratio_dry,So[So$ID==so&So$CD=="Y",]$ave[1]*Yvar[j])} #price/yield relation!
    
    # biomass cost for energy carrier production # low losses and high energy density of carrier favorable!
    BM_cost<-So[So$ID==so&So$CD=="C",]$ave[1]*inoutratio_dry/NCV_spec
    BM_cost<-BM_cost*BMcostvar #incl. variation
    
    # handling and supply costs for densi plant input # bioboost already includes capacity factor and backhaul of transport!
    handC<-Su[Su$ID==su&Su$CD=="h_cost",]$ave[1]
    sup<-Su[Su$ID==su&Su$CD=="v_cost",]$ave[1]
    idle<-Su[Su$ID==su&Su$CD=="t_cost",]$ave[1]    
    sup_cost<-function(size){((handC+idle)*2+sup*sup_d(size))*size*inoutratio_dry}
    
    # annualised capital expenditures and variable & general costs
    CRF<-c(0.087,0.1175,0.1175)
    SCL<-c(0.7,0.7,0.8)
    # Energy costs - yearly    
    drying<-((MCdifference)*2.9*1000)/3.6 #kWh/t_in oder out? u. eigentlich MC anforderung!
    drying<-if(i==1){drying}else{0}
    CAPEX<-De[De$ID==de&De$CD=="INV",]$ave[1]*INVvar*CRF[i]*10^6 #Euro/year incl. Taxes, Financing (loan iterest), insurance, depreciation
    opex<-De[De$ID==de&De$CD=="var",]$ave[1]+drying*0.02312/NCV_spec #Euro/GJ_out gas costs 23euro/MWh (with 40 - only on pelletisation impact)      
    capex<-function(size){(CAPEX*(size/De[De$ID==de&De$CD=="refsize",]$ave[1])^SCL[i])}
    
    # carrier cost without biomass cost at densification plant gate and without variable and general costs   
    CA_cost<-function(size){(sup_cost(size)+capex(size))/(size*NCV_spec)}
    
    # optimisation supply distance and size of densification plant & write into matrix
    interval<-c(1000,5*10^5)
    prodsize[j,1+i]<-optimize(CA_cost,interval)$minimum/1000
    outEcontent<-optimize(CA_cost,interval)$minimum*NCV_spec
    bmcost[j,1+i]<-BM_cost
    prodcost[j,1+i]<-optimize(CA_cost,interval)$objective+BM_cost+opex
    supcost[j,1+i]<-sup_cost(optimize(CA_cost,interval)$minimum)/outEcontent
    supdist[j,1+i]<-sup_d(optimize(CA_cost,interval)$minimum)
    OPEXcost[j,1+i]<-opex
    CAPEXcost[j,1+i]<-capex(optimize(CA_cost,interval)$minimum)/outEcontent
  }
}
#costs for raw biomass
so<-match(biomass[1],unique(So[,2]))
bmcost[1,1]<-So[So$ID==so&So$CD=="C",]$ave[1]/NCV(So[So$ID==so&So$CD=="GCV",]$ave[1],So[So$ID==so&So$CD=="mc",]$ave[1])
prodcost[1,1]<-bmcost[1,1]

so<-match(biomass[2],unique(So[,2]))
bmcost[2,1]<-So[So$ID==so&So$CD=="C",]$ave[1]/NCV(So[So$ID==so&So$CD=="GCV",]$ave[1],So[So$ID==so&So$CD=="mc",]$ave[1])
prodcost[2,1]<-bmcost[2,1]

resm<-matrix(nrow=6,ncol=6,0)
rownames(resm)<-c("Feedstock","Supply","Dens. CAPEX","Dens. OPEX","Size (kt/a)","Supdist (km)")
colnames(resm)<-c(paste(prod[,1]),paste(prod[,2]))
resm[1,]<-c(bmcost[1,2:4],bmcost[2,2:4])
resm[2,]<-c(supcost[1,2:4],supcost[2,2:4])
resm[3,]<-c(CAPEXcost[1,2:4],CAPEXcost[2,2:4])
resm[4,]<-c(OPEXcost[1,2:4],OPEXcost[2,2:4])
resm[5,]<-c(prodsize[1,2:4],prodsize[2,2:4])
resm[6,]<-c(supdist[1,2:4],supdist[2,2:4])

## distribution calculation
for(r in 1:6){
  for(p in 1:8){
    delC[[p]][r,1]<-v(prodcost)[p]
    delC[[p]][r,2]<-v(DISTCDF(RDPs[r,2],RDPs[r,3]))[p]+if(RDPs[r,4]==0){0}else{v(DISTCDF(RDPs[r,4],RDPs[r,5]))[p]}+if(RDPs[r,6]==0){0}else{v(DISTCDF(RDPs[r,6],RDPs[r,7]))[p]}
    delC[[p]][r,3]<-if(v(STOREDF(RDPs[r,1]*t_STOREvar))[p]==0){0}else{v(STOREDF(RDPs[r,1]*t_STOREvar))[p]}
    #delC[[p]][r,4]<-v(STOREDF(100))[p]-v(STOREDF(RDPs[r,1]))[p]
  }
}

## end user calculation
euRES<-t(sapply(delC,rowSums)[,c(2:4,6:8)])*1.13/euEff[1,]+euC[1,]
euIND<-t(sapply(delC,rowSums)[,c(2:4,6:8)])/euEff[2,]+euC[2,]-euRev[2,]*euREF[2]
euCOF<-t(sapply(delC,rowSums)[,c(2:4,6:8)])/euEff[3,]+euC[3,]
euFTR<-t(sapply(delC,rowSums)[,c(2:4,6:8)])/euEff[4,]+euC[4,]-euRev[4,]*euREF[2]
euDEL<-t(sapply(delC,rowSums)[,c(2:4,6:8)])


euLIST<-list("euDEL"=euDEL,"euRES"=euRES,"euCOF"=euCOF,"euFTR"=euFTR)