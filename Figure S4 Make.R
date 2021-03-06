### This code produces Figure S4 in the manuscript: 
### Burgess SC, Bode M, Leis JM, Mason LB. Selection on marine larval-fish swimming speed and the emergence of dispersal kernels. Submitted for publication.

library(tidyr) # for gather
library(extrafont)
loadfonts(device="postscript")

# setwd("")
load("d_Swains_Swimming1000.RData")
load("d_Swains_Passive1000.RData")

ind <- 1:length(d_Swains_Swimming1000)
n <- dim(d_Swains_Swimming1000[[1]])[1]

#### Calculate summaries #####
summary.d <- as.data.frame(matrix(NA,nrow=length(ind),ncol=6))
names(summary.d) <- c("Mean.Distance","Median.Distance","Max.Distance","Distance.max","Local.Retention","Percent.Settle")
summary.d.passive <- summary.d
names(summary.d.passive) <- names(summary.d)
LRvec <- seq(0,500,1)
LRcurve <- as.data.frame(matrix(NA,nrow=length(ind),ncol=length(LRvec)))
LRvecp <- seq(0,500,1)
LRcurvep <- as.data.frame(matrix(NA,nrow=length(ind),ncol=length(LRvecp)))

for(i in ind){

# Swimming model
	foo <- d_Swains_Swimming1000[[i]]
	foo <- foo[foo$Settlement=="1",]

	summary.d[i,1] <- mean(foo$Distance.km)
	summary.d[i,2] <- median(foo$Distance.km)
	summary.d[i,3] <- quantile(foo$Distance.km,0.99)
	un <- unique(round(foo$Distance.km,0))
	summary.d[i,4] <- un[which.max(tabulate(match(round(foo$Distance.km,0),un)))]
	summary.d[i,5] <- (dim(foo[foo$Distance.km<=5,])[1] / length(foo$Distance.km)) * 100
	summary.d[i,6] <- (length(foo$Distance.km) / n)*100

	for(j in 1:length(LRvec)){
		LRcurve[i,j] <- (dim(foo[foo$Distance.km<=LRvec[j],])[1] / length(foo$Distance.km)) * 100
	}
	
# Passive model
	foop <- d_Swains_Passive1000[[i]]
	foop <- foop[foop$Settlement=="1",]

	summary.d.passive[i,1] <- mean(foop$Distance.km)
	summary.d.passive[i,2] <- median(foop$Distance.km)
	summary.d.passive[i,3] <- quantile(foop$Distance.km,0.99)
	un <- unique(round(foop$Distance.km,0))
	summary.d.passive[i,4] <- un[which.max(tabulate(match(round(foop$Distance.km,0),un)))]
	summary.d.passive[i,5] <- (dim(foop[foop$Distance.km<=5,])[1] / length(foop$Distance.km)) * 100
	summary.d.passive[i,6] <- (length(foop$Distance.km) / n)*100

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


quartz(width=9,height=4)
par(mfrow=c(1,3),mar=c(2,4,1,1),oma=c(4,2,1,0))
nf <- layout(mat=matrix(1:3,1,3,byrow=T),widths=c(3.5,3.5,2))
layout.show(nf)

plot(c(0.5,4.5),c(-250,100),type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
set.seed(10)
points(jitter(rep(1,24),5),d.diff$Mean.Distance,pch=19,cex=1.5,col="grey")
points(jitter(rep(2,24),4),d.diff$Median.Distance,pch=19,cex=1.5,col="grey")
points(jitter(rep(3,24),3),d.diff$Distance.max,pch=19,cex=1.5,col="grey")
points(jitter(rep(4,24),1),d.diff$Max.Distance,pch=19,cex=1.5,col="grey")
with(d.diff.long, boxplot(Delta ~ Metric,xaxt="n",yaxt="n",frame=F,add=T,range=2))
abline(h=0,lty=2,col="grey",lwd=1.5)

axis(side=1,at=1:4,labels=c("Mean","Median","Mode","Max"),cex.axis=1.5)
axis(side=2,at=seq(-500,500,50),las=1,cex.axis=1.5)
mtext(expression(paste("Effect of swimming (",Delta,"km)",sep="")),side=2,line=4,cex=cexs)
mtext("Quantity of the dispersal\nkernel compared",side=1,line=5,cex=cexs)
mtext("a)",adj=0,cex=1.2)


plot(c(0,40),c(-10,50),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="n")
for(i in ind){
	lines(LRvec,LRcurve[i,] - LRcurvep[i,])
}
abline(h=0,lty=2,col="grey",lwd=1.5)
axis(side=1,at=seq(0,50,5),labels=c("0","","10","","20","","30","","40","","50"),cex.axis=1.5)
axis(side=2,at=seq(-10,50,10),las=1,cex.axis=1.5)
mtext("Scale of Local Retention (LR)\n(radius, km)",side=1,line=5,cex=cexs)
mtext(expression(paste("Effect of swimming (",Delta,"LR, %)",sep="")),side=2,line=3,cex=cexs)
mtext("b)",adj=0,cex=1.2)

plot(c(0.5,1.5),c(0,7),type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
set.seed(10)
points(jitter(rep(1,24),4),p.diff$Percent.Settle,pch=19,cex=1.5,col="grey")
with(p.diff.long[p.diff.long$Metric=="Percent.Settle",], boxplot(Ratio ~ Metric,xaxt="n",yaxt="n",frame=F,add=T,range=2,outline=F))
axis(side=1,at=1,labels=c("% Settlement"),cex.axis=1.5)
axis(side=2,at=seq(0,7,1),las=1,cex.axis=1.5)
abline(h=1,lty=2,col="grey",lwd=1.5)
mtext("Effect of swimming (factor change)",side=2,line=2.5,cex=cexs)
mtext("c)",adj=0,cex=cexs)
################################################################

# dev.copy2pdf (file="Figure S4.pdf", family="ArialMT")
# Sys.setenv(R_GSCMD="/usr/local/bin/gs") # have to point R to the ghost script location.  
# # To find out where your ghost script is located, type system("which gs), then use that file path in code above.
# embed_fonts ("Figure S4.pdf",outfile='Figure S4.pdf') # journals often want fonts embedded

