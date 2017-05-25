## Figure 1

resm2<-matrix(nrow=3,ncol=6,0)
rownames(resm2)<-c("Cost [\u20ac/GJ]","Size [kt/a]","Supply [km]")
colnames(resm2)<-c("WtP","StP","WtT","StT","WtO","StO")
resm2[1,]<-round(colSums(resm[1:4,c(4,1,5,2,6,3)]),1)
resm2[2,]<-round(resm[5,c(4,1,5,2,6,3)])
resm2[3,]<-round(resm[6,c(4,1,5,2,6,3)])

frbn<-c("#a50f15","#f03b20","#fd8d3c","#fecc5c")
png(paste(subDir,"/","production_cost",strtrim(Sys.time(),10),".png",sep=""),
    width=6*300,
    height=4*300,
    res=300,
    pointsize=5)

par(oma=c(0,2.5,0,2))
barplot(resm[1:4,c(4,1,5,2,6,3)],yaxt="n",xaxt="n",space=c(rep(c(1,0.5),3)),
        ylim=c(-5,16),col=frbn,xlim=c(1,15)) #ylim=c(0,round(max(colSums(resm[1:4,]))))
axis(2,at=seq(0,16,2),cex.axis=1.5)
legend("right",rev(rownames(resm)[1:4]),col=rev(frbn),bty="n",cex=2,pch=15,bg="white",pt.bg=rev(frbn),
       title="Cost shares")
title(ylab="              dBEC production costs [\u20ac 2014/GJ_dBEC]",cex.lab=1.5)

addtable2plot(0.6 ,-5,resm2,bty="o",display.rownames=FALSE,hlines=TRUE,
              vlines=TRUE,xpad=1,cex=1.85)

text(11,-2.5,rownames(resm2)[1],cex=1.5,adj=0)
text(11,-3.5,rownames(resm2)[2],cex=1.5,adj=0)
text(11,-4.5,rownames(resm2)[3],cex=1.5,adj=0)

abline(v=c(4.04,7.41),lty=3)

dev.off()

####### Figure 2

png(paste(subDir,"/","RDPs_cost",strtrim(Sys.time(),10),".png",sep=""),
    width=5*300,
    height=4*300,
    res=300,
    pointsize=5)

par(mfrow=c(2,1),oma=c(2,2,2,2),mar=c(4,5,2,1))

#par(fig=c(0,1,0.6,1))

#layout(matrix(c(1,3,2,3), 3, 1, byrow = TRUE),
#widths=c(3), heights=c(2,2,0.5))

z<-0
flg<-order(rowSums(delC[[1+z]]))
ygrenzen<-c(delC[[1+z]][1,1],
            max(rowSums(delC[[1+z]]),rowSums(delC[[2+z]]),rowSums(delC[[3+z]]),rowSums(delC[[4+z]])))

plot(1, type="n", axes=F, xlab="", ylab="",ylim=ygrenzen,xlim=c(1,8.5))
axis(2,at=seq(round(ygrenzen[1]),round(ygrenzen[2]),2),seq(round(ygrenzen[1]),round(ygrenzen[2]),2),lwd=2)
axis(1,at=1:7,c("none",rownames(delC[[1]])[flg]),lwd=2)
#points(1:7,c(delC[[1+z]][1,1],rowSums(delC[[1+z]])[flg]),pch=1,lwd=1)
points(1:7,c(delC[[2+z]][1,1],rowSums(delC[[2+z]])[flg]),pch=2,lwd=1.5)
points(1:7,c(delC[[3+z]][1,1],rowSums(delC[[3+z]])[flg]),pch=3,lwd=1.5) 
points(1:7,c(delC[[4+z]][1,1],rowSums(delC[[4+z]])[flg]),pch=4,lwd=1.5)

title(ylab="Delivery costs [\u20ac2014/GJ_dBEC]",main="dBECs based on Straw",cex.lab=1.2,cex.main=1.4)

legend("right",rev(c("StP","StT","StO")),pch=4:2,lty=0,cex=1.5,
       lwd=rev(c(2,2,2)),bg="white")
#par(fig=c(0,1,0.2,0.6),new=TRUE)

z<-4
flg<-order(rowSums(delC[[1+z]]))
ygrenzen<-c(delC[[1+z]][1,1],
            max(rowSums(delC[[1+z]]),rowSums(delC[[2+z]]),rowSums(delC[[3+z]]),rowSums(delC[[4+z]])))

plot(1, type="n", axes=F, xlab="", ylab="",ylim=ygrenzen,xlim=c(1,8.5))
axis(2,at=seq(round(ygrenzen[1]),round(ygrenzen[2]),2),seq(round(ygrenzen[1]),round(ygrenzen[2]),2),lwd=2)
axis(1,at=1:7,c("none",rownames(delC[[1]])[flg]),lwd=2)
#points(1:7,c(delC[[1+z]][1,1],rowSums(delC[[1+z]])[flg]),pch=1,lwd=1)
points(1:7,c(delC[[2+z]][1,1],rowSums(delC[[2+z]])[flg]),pch=2,lwd=1.5)
points(1:7,c(delC[[3+z]][1,1],rowSums(delC[[3+z]])[flg]),pch=3,lwd=1.5) 
points(1:7,c(delC[[4+z]][1,1],rowSums(delC[[4+z]])[flg]),pch=4,lwd=1.5)

title(ylab="Delivery costs [\u20ac2014/GJ_dBEC]",main="dBECs based on Wood",cex.lab=1.2,cex.main=1.4)

#par(fig=c(0,1,0,0.2),new=TRUE)

#plot(1, type="n", axes=F, xlab="", ylab="")
#frame()
legend("right",rev(c("WtP","WtT","WtO")),pch=4:2,lty=0,cex=1.5,
       lwd=rev(c(2,2,2)),bg="white") #,title="Bioenergy Carrier"

dev.off()

## Figure 3
png(paste(subDir,"/","tot_cost",strtrim(Sys.time(),10),".png",sep=""),
    width=6*300,
    height=4*300,
    res=300,
    pointsize=5)

par(mfrow=c(2,2),oma=c(2,2,2,4))
ueber<-c("bioenergy carrier delivery","residential heating","electricity production","Fischer Tropsch diesel")
newplot<-c(T,F,F,F)
plot(1, type="n", axes=F, xlab="", ylab="",xlim=c(0.5,6.5))
mtext("Energy deployment costs [\u20ac2014/GJ]         ",side=2,line=3,cex=2,adj=1)
for(e in 1:4){
  par(new=newplot[e])
  scen<-dim(RDPs)[1]
  unter<-round(min(euLIST[[e]])*0.8)  
  #par(oma=c(2,2,2,2))
  plot(1, type="n",xlab="",ylab="",xaxt="n",
       main=paste("Costs for ",ueber[e],sep=""),
       ylim=c(unter,max(round(euLIST[[e]]))),cex.main=2.4,
       xlim=c(0.5,6.5),cex.axis=1.5,font.axis=2)
  grid(col = "lightgray", lty = "dotted",lwd = c(1))
  for(s in 1:scen){
    points(1:scen,t(euLIST[[e]][,s]),pch="+",cex=2)
  }
  
  axis(1,at=1:scen,rownames(euLIST[[e]]),lwd=2,cex.axis=1.8) 
  abline(h=euREF[e],col="red")
  abline(h=c(0,40.5,0,19.6)[e],col="red",lty=2)
  abline(h=c(0,29.3,0,15.5)[e],col="red",lty=3)
}

dev.off()


