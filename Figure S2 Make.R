### This code produces Figure S2 in the manuscript: 
### Burgess SC, Bode M, Leis JM, Mason LB. Selection on marine larval-fish swimming speed and the emergence of dispersal kernels. Submitted for publication.


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
	
	summary.foo <- as.data.frame(matrix(NA,nrow=length(speed.vec),ncol=10))
	summary.foo$Speed.from <- speed.vec
	summary.foo$Speed.mid <- speed.vec + ((speed.step*speed.window)/2) 
	names(summary.foo) <- 	c("Mean.Distance","Median.Distance","Max.Distance","Distance.max",
							"Local.Retention","Percent.Settle","Mean.PLD","Median.PLD","Max.PLD",
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
			summary.foo[j,7] <- mean(foo1$Realised.PLD.days,na.rm=T)
			summary.foo[j,8] <- median(foo1$Realised.PLD.days,na.rm=T)
			summary.foo[j,9] <- max(foo1$Realised.PLD.days,na.rm=T)		
			summary.foo[j,10] <- length(foo1$Distance.km)
		}
	summary.speed[[i]] <- summary.foo
}

## Make plot of swimming speed vs PLD distance for settlers only
quartz(width=6,height=3)
par(mfrow=c(1,2),mar=c(3,3,1,1),oma=c(2,3.5,1,1))
cex.tcks <- 1.2
cex.labs <- 1.2
lwds=4

plot(c(0, maxss),c(24,25.5),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=seq(0, maxss,0.02),cex.axis=cex.tcks)
axis(side=2,at=seq(-30,30,0.5),las=1,cex.axis=cex.tcks)
mtext("a) Mean PLD",side=3,adj=0)
for(i in ind){
	tmp <- summary.speed[[i]]
	with(tmp,lines(Speed.mid,Mean.PLD,lty=1))	
}
# abline(h=0,col="grey",lty=1,lwd=2)

plot(c(0, maxss),c(24,25.5),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=seq(0, maxss,0.02),cex.axis=cex.tcks)
axis(side=2,at=seq(-30,30,0.5),las=1,cex.axis=cex.tcks)
mtext("b) Median PLD",side=3,adj=0)
for(i in ind){
	tmp <- summary.speed[[i]]
	with(tmp,lines(Speed.mid,Median.PLD,lty=1))	
}
# abline(h=0,col="grey",lty=1,lwd=2)

# plot(c(0, maxss),c(24,34),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
# axis(side=1,at=seq(0, maxss,0.02),cex.axis=cex.tcks)
# axis(side=2,at=seq(-30,30,0.5),las=1,cex.axis=cex.tcks)
# mtext("c) Max PLD",side=3)
# for(i in ind){
	# tmp <- summary.speed[[i]]
	# with(tmp,lines(Speed.mid,Max.PLD,lty=1))	
# }
# abline(h=0,col="grey",lty=1,lwd=2)

mtext(expression(paste("Swimming speed (",ms^-1,")")),side=1,line=0.5,cex=cex.labs,outer=T)
mtext("Pelagic Larval Duration\n(PLD, days)",side=2,line=1,cex=cex.labs,outer=T,padj=0.4)
#########################################################

# dev.copy2pdf (file="Figure S2.pdf", family="ArialMT")
# Sys.setenv(R_GSCMD="/usr/local/bin/gs") # have to point R to the ghost script location.  
# # To find out where your ghost script is located, type system("which gs), then use that file path in code above.
# embed_fonts ("Figure S2.pdf",outfile='Figure S2.pdf') # journals often want fonts embedded

