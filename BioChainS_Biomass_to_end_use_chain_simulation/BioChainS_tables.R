#Table 2

png(paste(subDir,"/","bmcost",strtrim(Sys.time(),10),".png",sep=""),
    width=5*200,
    height=1*200,
    res=200,
    pointsize=4)
table<-tableGrob(round(bmcost,1),theme=ttx)
grid.draw(table)
dev.off()

#Table 3

sens1.5<-data.frame(Feedstock=c("Wheat Straw via Truck","Wood Chips via Truck","Wheat Straw via Tractor","Wood Chips via Tractor"),
                    Pelletisation=rep(0,4),Torr_Pell=rep(0,4),Pyrolysis=rep(0,4))
sens1.5[,2]<-c(paste(round(resm[2,c(1,4)],1)," (",round(resm[6,c(1,4)]),"km)",sep=""),
               paste(round(resm_delvar[2,c(1,4)],1)," (",round(resm_delvar[6,c(1,4)]),"km)",sep=""))
sens1.5[,3]<-c(paste(round(resm[2,c(2,5)],1)," (",round(resm[6,c(2,5)]),"km)",sep=""),
               paste(round(resm_delvar[2,c(2,5)],1)," (",round(resm_delvar[6,c(2,5)]),"km)",sep=""))
sens1.5[,4]<-c(paste(round(resm[2,c(3,6)],1)," (",round(resm[6,c(3,6)]),"km)",sep=""),
               paste(round(resm_delvar[2,c(3,6)],1)," (",round(resm_delvar[6,c(3,6)]),"km)",sep=""))
dimnames(sens1.5)[[2]][1]<-"Feedstock [\u20ac/GJ]"

write.xlsx(sens1.5,paste(subDir,"/","results",strtrim(Sys.time(),10),".xlsx",sep="")
             ,sheetName="supcost",row.names=FALSE)

png(paste(subDir,"/","supcost",strtrim(Sys.time(),10),".png",sep=""),
    width=6*200,
    height=2*200,
    res=200,
    pointsize=4)

table<-tableGrob(sens1.5,theme=ttx)
grid.draw(table)
dev.off()

#Table 4
supLIST<-list(resm_delvar,resm_Yd,resm_Yu,resm_INVu,resm_INVd)
supDF<-data.frame(Tech.=c("StP","StT","StO","WtP","WtT","WtO"),rep(0,6),rep(0,6),
                  rep(0,6),rep(0,6),rep(0,6),rep(0,6))
names(supDF)<-c("Tech.","Base [\u20ac/GJ]","Tractor","Yield-80%","Yield+80%","Inv.Cost+10%","Inv.Cost-10%") #±

for(i in 1:6){
  zwi<-paste(round(colSums(resm[1:4,])[i],1)," (",round(resm[5,i]),"kt/a)",sep="")
  supDF[i,2]<-if(resm[5,i]>499){paste(zwi,"*",sep="")}else{zwi}
  for(l in 1:5){
    resm_sens<-supLIST[[l]]
    zw<-paste(pls((colSums(resm_sens[1:4,])/colSums(resm[1:4,])-1)[i]*100),"% (",pls((resm_sens[5,]/resm[5,]-1)[i]*100),"%)",sep="")
    supDF[i,2+l]<-if(resm_sens[5,i]>499){paste(zw,"*",sep="")}else{zw}
  }
}
write.xlsx(supDF,
           paste(subDir,"/","results",strtrim(Sys.time(),10),".xlsx",sep="")
           ,sheetName="sensSUP",row.names=FALSE,append=TRUE)

png(paste(subDir,"/","sensSUP",strtrim(Sys.time(),10),".png",sep=""),
    width=9*200,
    height=2*200,
    res=200,
    pointsize=4)


table<-tableGrob(supDF,theme=ttx)
grid.draw(table)
dev.off()

#Table 5

sensXfun<-function(sensX){
  matrix(unlist(lapply(sensX,rowSums)[c(2:4,6:8)]),ncol=6,byrow=TRUE)[,c(1,6)]/
    matrix(unlist(lapply(delC,rowSums)[c(2:4,6:8)]),ncol=6,byrow=TRUE)[,c(1,6)]
}

delLIST<-list(delC_STOREu,delC_DISTu,delC_HANDu)

distDF<-data.frame(Tech.=c("StP","StT","StO","WtP","WtT","WtO"),rep(0,6),rep(0,6),
                   rep(0,6),rep(0,6))
names(distDF)<-c("Tech.","Base [\u20ac/GJ]","Store+80d","Dis.Cost±50%","Hand.Cost±50%")
distDF[,2]<-paste(round(matrix(unlist(lapply(delC,rowSums)[c(2:4,6:8)]),ncol=6,byrow=TRUE)[,1],1),"|",
                  round(matrix(unlist(lapply(delC,rowSums)[c(2:4,6:8)]),ncol=6,byrow=TRUE)[,6],1))

for(i in 1:6){
  distDF[i,3]<-paste("+",round((sensXfun(delC_STOREu)[i,1]-1)*100,1),"%|+",round((sensXfun(delC_STOREu)[i,2]-1)*100,1),"%",sep="")
  for(l in 2:3){
    delC_sens<-delLIST[[l]]
    distDF[i,2+l]<-paste("±",round((sensXfun(delC_sens)[i,1]-1)*100,1),"%|±",round((sensXfun(delC_sens)[i,2]-1)*100,1),"%",sep="")
  }
}
write.xlsx(distDF,
           paste(subDir,"/","results",strtrim(Sys.time(),10),".xlsx",sep="")
           ,sheetName="sensDEL",row.names=FALSE,append=TRUE)

png(paste(subDir,"/","sensDEL",strtrim(Sys.time(),10),".png",sep=""),
    width=6*200,
    height=2*200,
    res=200,
    pointsize=4)


table<-tableGrob(distDF,theme=ttx)
grid.draw(table)
dev.off()

#Table 6

ausw2<-function(sens,eu,fst,scd){ #,q1,q2
  paste(round(sens[[eu]][fst,1]-sens[[eu]][scd,1],1),"|",round(sens[[eu]][fst,6]-sens[[eu]][scd,6],1),sep="")
}
ausw1<-function(sens,eu,fst){
  paste(pls((sens[[eu]][fst,1]/euLIST[[eu]][fst,1]-1)*100),"%|",pls((sens[[eu]][fst,6]/euLIST[[eu]][fst,6]-1)*100),"%",sep="")
}



flist<-list(euLIST_Cd,euLIST_Yu,euLIST_Yd,euLIST_DISTu,euLIST_STOREu)
fcmpr<-c("WtT FT","WtT El.","WtT RH","WtT vs WtP FT","StT vs StP FT","StO vs StT RH")
fdf1<-data.frame(EU=c(4,3,2),Cases=fcmpr[1:3],CaseNr=c(4,4,4))
fdf2<-data.frame(EU=c(4,4,2),Cases=fcmpr,CaseNr1=c(4,1,2),CaseNr2=c(5,2,3))

flast<-data.frame(Case=fcmpr,Base=rep(0,6),rep(0,6),rep(0,6),rep(0,6),rep(0,6),rep(0,6))
names(flast)<-c("Case","Base [\u20ac/GJ]","BM.cost-20%","Yield+80%","Yield-80%","dist.cost+50%","Store+80d")



for(i in 1:3){
  flast[i,2]<-paste(round(euLIST[[fdf1$EU[i]]][fdf1$CaseNr[i],1],1),"|",round(euLIST[[fdf1$EU[i]]][fdf1$CaseNr[i],6],1),sep="")
  flast[i+3,2]<-paste(round(euLIST[[fdf2$EU[i]]][fdf2$CaseNr1[i],1]-euLIST[[fdf2$EU[i]]][fdf2$CaseNr2[i],1],1),"|",
                      round(euLIST[[fdf2$EU[i]]][fdf2$CaseNr1[i],6]-euLIST[[fdf2$EU[i]]][fdf2$CaseNr2[i],6],1),sep="")
  for(c in 1:5){
    blu<-flist[[c]]
    #q1<-euLIST[[fdf2$EU[i]]][fdf2$CaseNr1[i],1]-euLIST[[fdf2$EU[i]]][fdf2$CaseNr2[i],1]
    #q2<-euLIST[[fdf2$EU[i]]][fdf2$CaseNr1[i],6]-euLIST[[fdf2$EU[i]]][fdf2$CaseNr2[i],6]
    flast[i,2+c]<-ausw1(blu,fdf1$EU[i],fdf1$CaseNr[i])
    flast[i+3,2+c]<-ausw2(blu,fdf2$EU[i],fdf2$CaseNr1[i],fdf2$CaseNr2[i]) #,q1,q2
  }
}

write.xlsx(flast,
           paste(subDir,"/","results",strtrim(Sys.time(),10),".xlsx",sep="")
           ,sheetName="sensEU",row.names=FALSE,append=TRUE)

png(paste(subDir,"/","sensEU",strtrim(Sys.time(),10),".png",sep=""),
    width=9*200,
    height=2*200,
    res=200,
    pointsize=4)

table<-tableGrob(flast,theme=ttx)
grid.draw(table)
dev.off()