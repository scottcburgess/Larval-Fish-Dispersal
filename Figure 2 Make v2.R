### This code produces Figure 2 in the manuscript: 
### Burgess SC, Bode M, Leis JM, Mason LB. Individual variation in marine larval-fish swimming speed and the emergence of dispersal kernels. Submitted for publication.

library(tidyr) 
library(extrafont)
loadfonts(device="postscript")

#setwd("")
load("d_Inshore_Swimming1000.RData")
load("d_Inshore_Passive1000.RData")

ind <- 1:length(d_Inshore_Swimming1000)
n <- dim(d_Inshore_Swimming1000[[1]])[1]

#### Calculate summaries #####
summary.d <- as.data.frame(matrix(NA,nrow=length(ind),ncol=7))
names(summary.d) <- c("Mean.Distance","Median.Distance","Max.Distance","Distance.max","Local.Retention","Percent.Settle","Variance")
summary.d.passive <- summary.d
names(summary.d.passive) <- names(summary.d)
LRvec <- seq(0,500,1)
LRcurve <- as.data.frame(matrix(NA,nrow=length(ind),ncol=length(LRvec)))
LRvecp <- seq(0,500,1)
LRcurvep <- as.data.frame(matrix(NA,nrow=length(ind),ncol=length(LRvecp)))

for(i in ind){

# Swimming model
	foo <- d_Inshore_Swimming1000[[i]]
	foo <- foo[foo$Settlement=="1",]

	summary.d[i,1] <- mean(foo$Distance.km)
	summary.d[i,2] <- median(foo$Distance.km)
	summary.d[i,3] <- quantile(foo$Distance.km,0.99)
	un <- unique(round(foo$Distance.km,0))
	summary.d[i,4] <- un[which.max(tabulate(match(round(foo$Distance.km,0),un)))]
	summary.d[i,5] <- (dim(foo[foo$Distance.km<=5,])[1] / length(foo$Distance.km)) * 100
	summary.d[i,6] <- (length(foo$Distance.km) / n)*100
	summary.d[i,7] <- var(foo$xcoord) + var(foo$ycoord) + 2*cov(foo$xcoord,foo$xcoord)

	for(j in 1:length(LRvec)){
		LRcurve[i,j] <- (dim(foo[foo$Distance.km<=LRvec[j],])[1] / length(foo$Distance.km)) * 100
	}
	
# Passive model
	foop <- d_Inshore_Passive1000[[i]]
	foop <- foop[foop$Settlement=="1",]

	summary.d.passive[i,1] <- mean(foop$Distance.km)
	summary.d.passive[i,2] <- median(foop$Distance.km)
	summary.d.passive[i,3] <- quantile(foop$Distance.km,0.99)
	un <- unique(round(foop$Distance.km,0))
	summary.d.passive[i,4] <- un[which.max(tabulate(match(round(foop$Distance.km,0),un)))]
	summary.d.passive[i,5] <- (dim(foop[foop$Distance.km<=5,])[1] / length(foop$Distance.km)) * 100
	summary.d.passive[i,6] <- (length(foop$Distance.km) / n)*100
	summary.d.passive[i,7] <- var(foop$xcoord) + var(foop$ycoord) + 2*cov(foop$xcoord,foop$xcoord)

	for(j in 1:length(LRvecp)){
		LRcurvep[i,j] <- (dim(foop[foop$Distance.km<=LRvecp[j],])[1] / length(foop$Distance.km)) * 100
	}
}

brks <- seq(-230,230,10)
lwds <- 5
cexs <- 1.2 

d.diff <- summary.d - summary.d.passive
d.diff.long <- gather(d.diff[,1:4],
	key="Metric",value="Delta",c("Mean.Distance","Median.Distance","Max.Distance","Distance.max"))
d.diff.long$Metric <- factor(d.diff.long$Metric, levels=c("Mean.Distance","Median.Distance","Distance.max","Max.Distance"))

p.diff <- summary.d[,which(names(summary.d) %in% c("Local.Retention","Percent.Settle"))]/
summary.d.passive[,which(names(summary.d.passive) %in% c("Local.Retention","Percent.Settle"))]
p.diff.long <- gather(p.diff,key="Metric",value="Ratio",c("Local.Retention","Percent.Settle"))


quartz(width=11,height=4)
# par(mfrow=c(1,3),mar=c(2,4,1,1),oma=c(4,2,1,0))
# nf <- layout(mat=matrix(1:3,1,3,byrow=T),widths=c(3.5,3.5,2))
par(mfrow=c(1,4),mar=c(2,4,1,2),oma=c(4,2,1,0))
nf <- layout(mat=matrix(1:4,1,4,byrow=T),widths=c(3.5,1.7,3.5,1.7))
# layout.show(nf)

plot(c(0.5,4.5),c(-250,100),type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
set.seed(10)
points(jitter(rep(1,24),5),d.diff$Mean.Distance,pch=19,cex=1.5,col="grey")
points(jitter(rep(2,24),4),d.diff$Median.Distance,pch=19,cex=1.5,col="grey")
points(jitter(rep(3,24),3),d.diff$Distance.max,pch=19,cex=1.5,col="grey")
points(jitter(rep(4,24),1),d.diff$Max.Distance,pch=19,cex=1.5,col="grey")
with(d.diff.long, boxplot(Delta ~ Metric,xaxt="n",yaxt="n",frame=F,add=T,range=2,col=NA))
abline(h=0,lty=2,col="grey",lwd=1.5)

axis(side=1,at=1:4,labels=c("Mean","Median","Mode","Max"),cex.axis=1.5)
axis(side=2,at=seq(-500,500,50),las=1,cex.axis=1.5)
mtext(expression(paste("Effect of swimming (",Delta,"km)",sep="")),side=2,line=4,cex=cexs)
mtext("Quantity of the dispersal kernel compared",side=1,line=3.5,cex=cexs, at=4)
mtext("a)",adj=0,cex=1.2)

plot(c(0.6,1.4),c(-2000,8000),type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
set.seed(10)
points(jitter(rep(1,24),4),d.diff$Variance,pch=19,cex=1.5,col="grey")
with(d.diff, boxplot(Variance,xaxt="n",yaxt="n",frame=F,add=T,range=2,outline=F,col=NA))
abline(h=1,lty=2,col="grey",lwd=1.5)
axis(side=1,at=1,labels=c("Variance"),cex.axis=1.5)
axis(side=2,at=seq(-2000,8000,1000),las=1,cex.axis=1.5)
mtext(expression(paste("Effect of swimming (",Delta," ",km^2,")",sep="")),side=2,line=4,cex=cexs)
mtext("b)",adj=0,cex=1.2)


plot(c(0,40),c(-10,50),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="n")
for(i in ind){
	lines(LRvec,LRcurve[i,] - LRcurvep[i,])
}
abline(h=0,lty=2,col="grey",lwd=1.5)
axis(side=1,at=seq(0,50,5),labels=c("0","","10","","20","","30","","40","","50"),cex.axis=1.5)
axis(side=2,at=seq(-10,50,10),las=1,cex.axis=1.5)
mtext("Scale of Local Retention (LR)\n(radius, km)",side=1,line=5,cex=cexs)
mtext(expression(paste("Effect of swimming (",Delta,"LR%)",sep="")),side=2,line=3,cex=cexs)
mtext("c)",adj=0,cex=1.2)

plot(c(0.6,1.4),c(0,7),type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
set.seed(10)
points(jitter(rep(1,24),4),p.diff$Percent.Settle,pch=19,cex=1.5,col="grey")
with(p.diff.long[p.diff.long$Metric=="Percent.Settle",], boxplot(Ratio ~ Metric,xaxt="n",yaxt="n",frame=F,add=T,range=2,outline=F,col=NA))
axis(side=1,at=1,labels=c("% Settlement"),cex.axis=1.5)
axis(side=2,at=seq(0,7,1),las=1,cex.axis=1.5)
abline(h=1,lty=2,col="grey",lwd=1.5)
mtext("Effect of swimming (factor change)",side=2,line=2.5,cex=cexs)
mtext("d)",adj=0,cex=cexs)
################################################################


# dev.copy2pdf (file="Figure 2.pdf", family="ArialMT")
# Sys.setenv(R_GSCMD="/usr/local/bin/gs") # have to point R to the ghost script location.  
# # To find out where your ghost script is located, type system("which gs), then use that file path in code above.
# embed_fonts ("Figure 2.pdf",outfile='Figure 2.pdf') # journals often want fonts embedded


# Numbers reported in Results section of paper
sort(d.diff.long[d.diff.long$Metric=="Mean.Distance",2]) # 22/24
sort(d.diff.long[d.diff.long$Metric=="Median.Distance",2]) # 24/24
sort(d.diff.long[d.diff.long$Metric=="Distance.max",2]) # 20/24
round(quantile(d.diff.long[d.diff.long$Metric=="Median.Distance",2],c(0.25,0.75)),0) # Median middle 50%
round(quantile(d.diff.long[d.diff.long$Metric=="Distance.max",2],c(0.25,0.75)),0) # Modal middle 50%
round(sort(d.diff.long[d.diff.long$Metric=="Max.Distance",2]),0) # Max distances
sort(round(LRcurve[,which(LRvec==5)] - LRcurvep[,which(LRvec==5)],2)) # LR with 5km radius 
round(sort(p.diff.long[p.diff.long$Metric=="Percent.Settle",2]),2) # Percet settled

