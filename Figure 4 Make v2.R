### This code produces Figure 4 in the manuscript: 
### Burgess SC, Bode M, Leis JM, Mason LB. Individual variation in marine larval-fish swimming speed and the emergence of dispersal kernels. Submitted for publication.

library(tidyr)
library(extrafont)
loadfonts(device="postscript")

# setwd("")
load("d_Inshore_Swimming10000.RData")

ind <- 1:length(d_Inshore_Swimming10000)
n <- dim(d_Inshore_Swimming10000[[1]])[1]

# Rolling averages.
speed.step <- 0.01 # m/s
speed.window <- 0.04/speed.step
summary.speed <- vector(mode="list",length=length(ind))
minss <- min(unlist(d_Inshore_Swimming10000,recursive=F)$Max.swimming.speed.ms)
maxss <- max(unlist(d_Inshore_Swimming10000,recursive=F)$Max.swimming.speed.ms)
speed.vec <- seq(minss,maxss,by=speed.step)

for(i in ind){
	foo <- d_Inshore_Swimming10000[[i]]
	foo <- foo[foo$Settlement==1,]

	# Choose certain island groups
	# j <- foo
	# Percy
	# j <- j[j$Start.point.lat>-21.9 & j$Start.point.lat < (-21.6) & j$Start.point.long > 150 & j$Start.point.long < 151 ,]
	# # with(j,plot(Start.point.long,Start.point.lat))
	# Keppels
	# j <- j[j$Start.point.lat>-23.23 & j$Start.point.lat < (-23) & j$Start.point.long > 150.85 & j$Start.point.long < 151.03 ,]  
	# # with(j,plot(Start.point.long,Start.point.lat))
	# Cap Bunk
	# j <- j[j$Start.point.lat>-23.94 & j$Start.point.lat < (-23.1) & j$Start.point.long > 151.6 & j$Start.point.long < 152.45,] 
	# # with(j,plot(Start.point.long,Start.point.lat))
	# foo <- j
	
	summary.foo <- as.data.frame(matrix(NA,nrow=length(speed.vec),ncol=8))
	summary.foo$Speed.from <- speed.vec
	summary.foo$Speed.mid <- speed.vec + ((speed.step*speed.window)/2) 
	names(summary.foo) <- 	c("Mean.Distance","Median.Distance","Max.Distance","Distance.max",
							"Local.Retention","Percent.Settle","PLD",
							"n","Speed.from","Speed.mid")

		for(j in 1:length(speed.vec)){
			foo1 <- foo[foo$Max.swimming.speed.ms>=speed.vec[j] & foo$Max.swimming.speed.ms<speed.vec[j+speed.window],]
			summary.foo[j,1] <- mean(foo1$Distance.km,na.rm=T)
			summary.foo[j,2] <- median(foo1$Distance.km,na.rm=T)
			summary.foo[j,3] <- quantile(foo1$Distance.km,0.99,na.rm=T)
			un <- unique(round(foo1$Distance.km,0))
			summary.foo[j,4] <- un[which.max(tabulate(match(round(foo1$Distance.km,0),un)))]
			# summary.foo[j,5] <- (dim(foo1[foo1$Distance.km<=5,])[1] / length(foo1$Distance.km)) * 100 # Local retention
			summary.foo[j,5] <- quantile(foo1$Distance.km,0.1,na.rm=T) # The distance that the lower 10% of larvae travel
			summary.foo[j,6] <- (length(foo1$Distance.km) / n)*100
			summary.foo[j,7] <- mean(foo1$Realised.PLD.days)
			summary.foo[j,8] <- length(foo1$Distance.km)
		}
	summary.speed[[i]] <- summary.foo
}



## Make plot of swimming speed vs Dispersal distance for settlers only
quartz(width=6,height=6)
par(mfrow=c(2,3),mar=c(5,3,1,1),oma=c(1,3.5,1,1))
cex.tcks <- 1.2
cex.labs <- 1.2
lwds=4

plot(c(0, maxss),c(-30,30),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=seq(0, maxss,0.02),cex.axis=cex.tcks)
axis(side=2,at=seq(-30,30,10),las=1,cex.axis=cex.tcks)
mtext("a) Mean distance",side=3)
for(i in ind){
	tmp <- summary.speed[[i]]
	with(tmp,lines(Speed.mid,Mean.Distance - mean(Mean.Distance,na.rm=T),lty=1))	
}
abline(h=0,col="grey",lty=1,lwd=2)
mtext("Dispersal distance\n(km, relative to the mean)",side=2,line=3,cex=cex.labs,outer=F)

plot(c(0, maxss),c(-30,30),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=seq(0, maxss,0.02),cex.axis=cex.tcks)
axis(side=2,at=seq(-30,30,10),las=1,cex.axis=cex.tcks)
mtext("b) Median distance",side=3)
for(i in ind){
	tmp <- summary.speed[[i]]
	with(tmp,lines(Speed.mid,Median.Distance - mean(Median.Distance,na.rm=T),lty=1))	
}
abline(h=0,col="grey",lty=1,lwd=2)

plot(c(0, maxss),c(-70,70),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=seq(0, maxss,0.02),cex.axis=cex.tcks)
axis(side=2,at=seq(-100,100,20),las=1,cex.axis=cex.tcks)
mtext("c) Max distance",side=3)
for(i in ind){
	tmp <- summary.speed[[i]]
	with(tmp,lines(Speed.mid,Max.Distance - mean(Max.Distance,na.rm=T),lty=1))	
}
abline(h=0,col="grey",lty=1,lwd=2)

mtext(expression(paste("Swimming speed (",ms^-1,")")),side=1,line=-24,cex=cex.labs,outer=T)
#########################################################

dat <- do.call(rbind.data.frame, d_Inshore_Swimming10000)
foo <- dat[dat$Settlement==1,]
foohist <- with(foo,hist(Distance.km,breaks=seq(0,500,10),plot=F))
foohist <- data.frame(mids=foohist$mids, counts=foohist$counts)

# (maxss - minss) / 2
# speed.vec[10]

for(j in c(3,10,20)){
			foo1 <- foo[foo$Max.swimming.speed.ms>=speed.vec[j] & foo$Max.swimming.speed.ms<speed.vec[j+speed.window],]
	maxy <- max(c(foohist$counts))

foo1hist <- with(foo1,hist(Distance.km,breaks=seq(0,500,10),plot=F))
foo1hist <- data.frame(mids=foo1hist$mids, counts=foo1hist$counts)

plot(x=c(0,450),y=c(0,maxy),type="n",yaxt="n",xaxt="n",xlab="",ylab="",bty="n")

with(foohist[foohist$counts>0,],
	segments(mids,rep(0,length(counts)),mids,counts,col="light grey",lwd=3))
# with(foo,segments(median(foo$Distance.km),0,median(foo$Distance.km),10000,col="black"))

with(foo1hist[foo1hist$counts>0,],
	segments(mids,rep(0,length(counts)),mids,counts,col="indianred1",lwd=3))
# with(foo1,segments(median(foo1$Distance.km),0,median(foo1$Distance.km),10000,col="red"))
	
	
	# with(foo1,hist(Distance.km,main="",
		# las=1,xlim=c(0,450),breaks=seq(0,500,10),
		# # col=adjustcolor("red",alpha.f=0.3),
		# col="indianred1",
		# freq=T,ylim=c(0,maxy/3),yaxt="n",xaxt="n",xlab="",ylab=""))

axis(side=2,at=seq(0,round(maxy/nrow(dat),2),0.01)*nrow(dat),labels=round(seq(0,round(maxy/nrow(dat),2),0.01),2),las=1,cex.axis=1.2)
axis(side=1,at=seq(0,500,100),cex.axis=1.2)
mtext(side=3,line=-1.8,adj=0.1,substitute(paste(i),
	list(i=ifelse(j==3,"d) Slower swimmers\n",ifelse(j==10,"e) Average swimmers\n","f) Faster swimmers\n")))))
legend(x=-40,y=24500,legend=substitute(paste(x," - ",y," ",ms^-1,sep=""),
	list(x=round(speed.vec[j],3),y=round(speed.vec[j+speed.window],3))),bty="n",cex=1.2)

# mtext(side=3, eval(paste(
	# round(speed.vec[j],3), 
	# " - ",
	# round(speed.vec[j+speed.window],3),
	# sep=""
	# )))
}
mtext("Proportion settlement",side=2,line=1,at=0.3,outer=T,cex=cex.labs)
mtext("Distance from natal site (km)",side=1,line=-2,cex=cex.labs,outer=T)






# # dev.copy2pdf (file="Figure 4.pdf", family="ArialMT")
# Sys.setenv(R_GSCMD="/usr/local/bin/gs") # have to point R to the ghost script location.  
# # To find out where your ghost script is located, type system("which gs), then use that file path in code above.
# embed_fonts ("Figure 4.pdf",outfile='Figure 4.pdf') # journals often want fonts embedded
